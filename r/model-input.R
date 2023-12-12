# generate model-input from cleaned data
# <- {wd}/data-clean.rds
# <- {wd}/data-breakpoints.rds
# <- {wd}/data-covariates.rds
# <- {wd}/daymet.csv
# -> {wd}/model-input.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting model-input:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(RPostgres))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(stringr))

source("functions.R")

config <- load_config()
rejects <- list(
  values = list()
)

# load data ---------------------------------------------------------------

cat("loading input data frames...")
df_temp <- readRDS(file.path(config$wd, "data-clean.rds"))$data %>%
  mutate(year = year(date)) %>%
  select(featureid, year, date, temp = mean)
df_bp <- readRDS(file.path(config$wd, "data-breakpoints.rds"))$model %>%
  select(-featureid_year)
df_covariates <- readRDS(file.path(config$wd, "data-covariates.rds"))
df_daymet <- read_csv(
  file.path(config$wd, "data-daymet.csv"),
  col_types = cols(
    featureid = col_integer(),
    year = col_integer(),
    date = col_date(format = ""),
    tmax = col_double(),
    tmin = col_double(),
    prcp = col_double()
  )
) %>%
  mutate(
    airTemp = (tmin + tmax) / 2
  ) %>%
  select(featureid, year, date, airTemp, prcp)
df_huc <- readRDS(file.path(config$wd, "data-huc.rds")) %>%
  select(featureid, huc8)
cat("done\n")


# transform ---------------------------------------------------------------

cat("computing lagged climate variables...")
df_daymet <- df_daymet %>%
  arrange(featureid, year, date) %>%
  mutate(dOY = yday(date)) %>%
  group_by(featureid, year) %>%
  # filter(featureid == 20628453, year == 2010) |>
  mutate(
    airTempLagged1 = lag(airTemp, n = 1),
    temp7p = rollapply(
      data = airTempLagged1,
      width = 7,
      FUN = mean,
      align = "right",
      fill = NA,
      na.rm = TRUE
    ),
    prcp2 = rollsum(x = prcp, 2, align = "right", na.pad = TRUE),
    prcp30 = rollsum(x = prcp, 30, align = "right", na.pad = TRUE)
  )
cat("done\n")

cat("joining data frames...")
df <- df_temp %>%
  left_join(df_bp, by = c("featureid", "year")) %>%
  left_join(df_covariates, by = "featureid") %>%
  left_join(df_daymet, by = c("featureid", "year", "date")) %>%
  mutate(
    impoundArea = AreaSqKM * allonnet / 100,
    spring_bp = coalesce(spring_bp, 75),
    spring_bp = if_else(spring_bp < 75, 75, spring_bp),
    fall_bp = coalesce(fall_bp, 330),
    fall_bp = if_else(fall_bp > 330, 330, fall_bp),
    dOY = yday(date)
  )
cat("done (nrow = ", nrow(df), ")\n", sep = "")

cat("trimming to breakpoints...")
rejects$values$outside_bp <- df %>%
  filter(
    dOY < spring_bp |
    dOY > fall_bp
  )
df <- df %>%
  filter(
    dOY >= spring_bp,
    dOY <= fall_bp
  )
cat("done (nrow = ", nrow(df), ", n removed = ", nrow(rejects$values$outside_bp), ")\n", sep = "")

cat("joining huc...")
df <- df %>%
  left_join(df_huc, by = "featureid")
cat("done (nrow = ", nrow(df), ")\n", sep = "")

cat("selecting columns...")
df <- df %>%
  select(
    featureid, huc8, year, date, temp,
    airTemp, prcp2, prcp30, temp7p,
    AreaSqKM, forest, devel_hi, agriculture, impoundArea
  )
cat("done (nrow = ", nrow(df), ")\n", sep = "")

cat("removing NAs...")
rejects$values$incomplete_case <- df[!complete.cases(df), ]
df <- df[complete.cases(df), ]
cat("done (nrow = ", nrow(df), ", n excluded = ", nrow(rejects$values$incomplete_case), ")\n", sep = "")

cat("removing series w/ less than 5 days of data...")
df <- df %>%
  arrange(featureid, year, date) %>%
  group_by(featureid, year) %>%
  mutate(
    delta_date = coalesce(as.numeric(difftime(date, lag(date), units = "day")), 1)
  ) %>%
  ungroup() %>%
  mutate(
    new_series = delta_date != 1,
    new_featureid = coalesce(featureid != lag(featureid), TRUE),
    new_year = coalesce(year != lag(year), TRUE),
    deploy_id = cumsum(1 * (new_series | new_featureid | new_year))
  )
df <- df %>%
  group_by(deploy_id) %>%
  mutate(
    n = n()
  ) %>%
  ungroup()
rejects$values$n_lt_5 <- df %>%
  filter(n < 5)
df <- df %>%
  filter(n >= 5) %>%
  select(-delta_date, -new_series, -new_featureid, -new_year, -deploy_id, -n)
cat("done (nrow = ", nrow(df), ", n excluded = ", nrow(rejects$values$n_lt_5), ")\n", sep = "")

df |>
  ggplot(aes(yday(date), temp)) +
  geom_hex(bins = 100)

df |>
  ggplot(aes(airTemp, temp)) +
  geom_hex(bins = 100)

# standardize -------------------------------------------------------------

cat("converting to long format...")
df_long <- df %>%
  gather(var, value, -featureid, -huc8, -year, -date, -temp)
cat("done\n")

cat("computing mean/sd of each variable...")
df_std <- df_long %>%
  group_by(var) %>%
  summarize(
    mean = mean(value),
    sd = sd(value)
  )
cat("done\n")

cat("standardizing variables...")
df <- df_long %>%
  group_by(var) %>%
  mutate(
    value = (value - mean(value)) / sd(value)
  ) %>%
  spread(var, value) %>%
  arrange(huc8, featureid, year, date)
cat("done\n")

cat("calculating derived covariates...")
df <- df %>%
  mutate(
    prcp2.da = prcp2 * AreaSqKM,
    prcp30.da = prcp30 * AreaSqKM,
    airTemp.prcp2 = airTemp * prcp2,
    airTemp.prcp2.da = airTemp * prcp2 * AreaSqKM,
    airTemp.prcp30 = airTemp * prcp30,
    airTemp.prcp30.da = airTemp * prcp30 * AreaSqKM,
    airTemp.forest = airTemp * forest,
    airTemp.devel_hi = airTemp * devel_hi,
    airTemp.da = airTemp * AreaSqKM,
    airTemp.impoundArea = airTemp * impoundArea,
    airTemp.agriculture = airTemp * agriculture
  )
cat("done (nrow = ", nrow(df), ")\n", sep = "")

cat("splitting dataset...")
featureid_years <- df %>%
  select(featureid, year) %>%
  distinct()

set.seed(4321)
training <- sample(1:nrow(featureid_years), size = round(0.9 * nrow(featureid_years)))
featureid_years_train <- featureid_years[training, ]
featureid_years_test <- featureid_years[-training, ]

df_train <- featureid_years_train %>%
  left_join(df, by = c("featureid", "year"))
df_test <- featureid_years_test %>%
  left_join(df, by = c("featureid", "year"))
cat("done (n train = ", nrow(df_train), ", n test = ", nrow(df_test), ")\n", sep = "")

cat("indexing deployments...")
df_train <- df_train %>%
  mutate(
    featureid_id = as.numeric(as.factor(featureid)),
    huc8_id = as.numeric(as.factor(huc8)),
    year_id = as.numeric(as.factor(year))
  ) %>%
  arrange(huc8_id, featureid_id, date) %>%
  mutate(
    delta_date = as.numeric(difftime(date, lag(date), units = "day")),
    new_series = delta_date != 1,
    new_featureid = coalesce(featureid_id != lag(featureid_id), TRUE),
    new_year = coalesce(year != lag(year), TRUE),
    new_deploy = 1 * (new_series | new_featureid | new_year),
    deploy_id = cumsum(new_deploy),
    featureid_id
  )
df_test <- df_test %>%
  mutate(
    featureid_id = as.numeric(as.factor(featureid)),
    huc8_id = as.numeric(as.factor(huc8)),
    year_id = as.numeric(as.factor(year))
  ) %>%
  arrange(huc8_id, featureid_id, date) %>%
  mutate(
    delta_date = as.numeric(difftime(date, lag(date), units = "day")),
    new_series = delta_date != 1,
    new_featureid = coalesce(featureid_id != lag(featureid_id), TRUE),
    new_year = coalesce(year != lag(year), TRUE),
    new_deploy = 1 * (new_series | new_featureid | new_year),
    deploy_id = cumsum(new_deploy)
  )

ids <- list(
  featureid = df_train %>% select(featureid, featureid_id) %>% distinct() %>% arrange(featureid_id),
  huc8 = df_train %>% select(huc8, huc8_id) %>% distinct() %>% arrange(huc8_id),
  year = df_train %>% select(year, year_id) %>% distinct() %>% arrange(year_id)
)

cat("done\n")

# J = nrow(ids$featureid)
# M = nrow(ids$huc8)
# Ti = nrow(ids$year)


# export ------------------------------------------------------------------

cat("saving to model-input.rds...")
list(
  train = df_train,
  test = df_test,
  ids = ids,
  std = df_std
) %>%
  saveRDS(file.path(config$wd, "model-input.rds"))
cat("done\n")

# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))
cat("finished model-input: ", as.character(end, tz = "US/Eastern"), " (elapsed = ", round(elapsed / 60, digits = 1), " min)\n", sep = "")
