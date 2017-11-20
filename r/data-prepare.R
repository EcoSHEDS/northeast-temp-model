# process raw data (clean, qaqc)
# <- {wd}/data/clean.rds
# <- {wd}/data/breakpoints.rds
# <- {wd}/data/covariates.rds
# -> {wd}/data/model-input.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting data-prepare:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(stringr))

config <- fromJSON("../config.json")


cat("loading input data frames...")
df_temp <- readRDS(file.path(config$wd, "data", "clean.rds")) %>%
  mutate(year = year(date)) %>%
  select(featureid, year, date, temp = mean)
df_bp <- readRDS(file.path(config$wd, "data", "breakpoints.rds")) %>%
  select(-featureid_year)
df_covariates <- readRDS(file.path(config$wd, "data", "covariates.rds"))
df_daymet <- read_csv(
  file.path(config$wd, "daymet.csv"),
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
cat("done\n")

cat("computing lagged climate variables...")
df_daymet <- df_daymet %>%
  arrange(featureid, year, date) %>%
  mutate(dOY = yday(date)) %>%
  group_by(featureid, year) %>%
  mutate(
    airTempLagged1 = lag(airTemp, n = 1, fill = NA),
    # temp5p = rollapply(
    #   data = airTempLagged1,
    #   width = 5,
    #   FUN = mean,
    #   align = "right",
    #   fill = NA,
    #   na.rm = TRUE
    # ),
    temp7p = rollapply(
      data = airTempLagged1,
      width = 7,
      FUN = mean,
      align = "right",
      fill = NA,
      na.rm = TRUE
    ),
    prcp2 = rollsum(x = prcp, 2, align = "right", fill = NA),
    # prcp7 = rollsum(x = prcp, 7, align = "right", fill = NA),
    prcp30 = rollsum(x = prcp, 30, align = "right", fill = NA)
  )
cat("done\n")

cat("joining data frames...")
df <- df_temp %>%
  left_join(df_bp, by = c("featureid", "year")) %>%
  left_join(df_covariates, by = "featureid") %>%
  left_join(df_daymet, by = c("featureid", "year", "date")) %>%
  mutate(
    site = as.character(featureid),
    impoundArea = AreaSqKM * allonnet / 100,
    spring_bp = coalesce(spring_bp, 75),
    spring_bp = if_else(spring_bp < 75, 75, spring_bp),
    fall_bp = coalesce(fall_bp, 330),
    fall_bp = if_else(fall_bp < 75, 75, fall_bp)
  )
cat("done ( nrow =", nrow(df), ")\n")

cat("trimming to breakpoints...")
df <- df %>%
  mutate(
    dOY = yday(date)
  ) %>%
  filter(
    dOY >= spring_bp,
    dOY <= fall_bp
  )
cat("done ( nrow =", nrow(df), ")\n")

cat("connecting to db ( host =", config$db$host, ", dbname =", config$db$dbname, ")...")
con <- dbConnect(PostgreSQL(), host = config$db$host, dbname = config$db$dbname, user = config$db$user, password = config$db$password)
cat("done\n")

cat("fetching featureid-huc12...")
featureids <- unique(df$featureid)
df_huc <- tbl(con, "catchment_huc12") %>%
  filter(featureid %in% featureids) %>%
  collect()
df_huc <- df_huc %>%
  mutate(
    huc2 = str_sub(huc12, 1, 2),
    huc4 = str_sub(huc12, 1, 4),
    huc8 = str_sub(huc12, 1, 8),
    huc10 = str_sub(huc12, 1, 10)
  )
disconnected <- dbDisconnect(con)
cat("done\n")

cat("joining huc...")
df <- df %>%
  left_join(df_huc, by = "featureid")
cat("done ( nrow =", nrow(df), ")\n")

cat("selecting columns...")
df <- df %>%
  select(
    site, huc8, year, date, temp,
    airTemp, prcp2, prcp30, temp7p,
    AreaSqKM, forest, devel_hi, agriculture, impoundArea
  )
cat("done ( nrow =", nrow(df), ")\n")

cat("removing NAs...")
df <- df[complete.cases(df), ]
cat("done ( nrow =", nrow(df), ")\n")

cat("removing series w/ less than 5 days of data...")
df <- df %>%
  arrange(site, year, date) %>%
  group_by(site, year) %>%
  mutate(
    delta_date = coalesce(as.numeric(difftime(date, lag(date), units = "day")), 1)
  ) %>%
  ungroup() %>%
  mutate(
    new_series = delta_date != 1,
    new_site = coalesce(site != lag(site), TRUE),
    new_year = coalesce(year != lag(year), TRUE),
    deploy_id = cumsum(1 * (new_series | new_site | new_year))
  )
df <- df %>%
  group_by(deploy_id) %>%
  mutate(
    n = n()
  ) %>%
  ungroup()
df <- df %>%
  filter(n >= 5) %>%
  select(-delta_date, -new_series, -new_site, -new_year, -deploy_id, -n)
cat("done\n")

cat("converting to long format...")
df_long <- df %>%
  gather(var, value, -site, -huc8, -year, -date, -temp)
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
  arrange(huc8, site, year, date)
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
cat("done ( nrow =", nrow(df), ")\n")

cat("splitting dataset...")
site_years <- df %>%
  select(site, year) %>%
  distinct()

set.seed(4321)
training <- sample(1:nrow(site_years), size = round(0.9 * nrow(site_years)))
site_years_train <- site_years[training, ]
site_years_test <- site_years[-training, ]

df_train <- site_years_train %>%
  left_join(df, by = c("site", "year"))
df_test <- site_years_test %>%
  left_join(df, by = c("site", "year"))
cat("done\n")

cat("indexing deployments...")
df_train <- df_train %>%
  mutate(
    site_id = as.numeric(as.factor(site)),
    huc8_id = as.numeric(as.factor(huc8)),
    year_id = as.numeric(as.factor(year))
  ) %>%
  arrange(huc8_id, site_id, date) %>%
  mutate(
    delta_date = as.numeric(difftime(date, lag(date), units = "day")),
    new_series = delta_date != 1,
    new_site = coalesce(site_id != lag(site_id), TRUE),
    new_year = coalesce(year != lag(year), TRUE),
    new_deploy = 1 * (new_series | new_site | new_year),
    deploy_id = cumsum(new_deploy),
    site_id
  )
df_test <- df_test %>%
  mutate(
    site_id = as.numeric(as.factor(site)),
    huc8_id = as.numeric(as.factor(huc8)),
    year_id = as.numeric(as.factor(year))
  ) %>%
  arrange(huc8_id, site_id, date) %>%
  mutate(
    delta_date = as.numeric(difftime(date, lag(date), units = "day")),
    new_series = delta_date != 1,
    new_site = coalesce(site_id != lag(site_id), TRUE),
    new_year = coalesce(year != lag(year), TRUE),
    new_deploy = 1 * (new_series | new_site | new_year),
    deploy_id = cumsum(new_deploy)
  )

ids <- list(
  site = df_train %>% select(site, site_id) %>% distinct() %>% arrange(site_id),
  huc8 = df_train %>% select(huc8, huc8_id) %>% distinct() %>% arrange(huc8_id),
  year = df_train %>% select(year, year_id) %>% distinct() %>% arrange(year_id)
)

# J = nrow(rand_ids$site)
# M = nrow(rand_ids$huc8)
# Ti = nrow(rand_ids$year)

list(
  train = df_train,
  test = df_test,
  ids = ids,
  std = df_std
) %>%
  saveRDS(file.path(config$wd, "model-input.rds"))

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))
cat("finished data-process:", as.character(end, tz = "US/Eastern"), "( elapsed =", round(elapsed / 60, digits = 1), "min )\n")
