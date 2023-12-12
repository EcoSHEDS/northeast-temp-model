# process raw data (clean, qaqc)
# <- {wd}/data-db.rds
# <- {wd}/data-daymet.csv
# -> {wd}/data-clean.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting data-clean: ", as.character(start, tz = "US/Eastern"), "\n", sep = "")

suppressPackageStartupMessages(library(RPostgres))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(slider))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(broom))

source("functions.R")

config <- load_config()

rejects <- list()

# load data ---------------------------------------------------------------

cat("loading data-daymet.csv...")
df_daymet <- read_csv(
  file.path(config$wd, "data-daymet.csv"),
  col_types = cols(
    featureid = col_integer(),
    date = col_date(format = ""),
    tmax = col_double(),
    tmin = col_double(),
    prcp = col_double()
  )
) %>%
  mutate(
    airtemp = (tmax + tmin) / 2
  ) %>%
  select(featureid, date, airtemp, prcp)

df_daymet_leap_years <- df_daymet %>%
  mutate(year = year(date)) %>%
  distinct(featureid, year) %>%
  filter(leap_year(year)) %>%
  mutate(
    date = ymd(paste(year, "12", "30", sep = "-"))
  ) %>%
  left_join(df_daymet, by = c("featureid", "date")) %>%
  mutate(
    date = date + days(1),
    prcp = 0
  ) %>%
  select(featureid, date, airtemp, prcp)

df_daymet <- df_daymet %>%
  bind_rows(df_daymet_leap_years) %>%
  arrange(featureid, date)

MAX_YEAR <- max(year(df_daymet$date))

cat("done (nrow = ", scales::comma(nrow(df_daymet)), ", max year = ", MAX_YEAR, ")\n", sep = "")

cat("loading data-db.rds...")
db <- readRDS(file.path(config$wd, "data-db.rds"))
db_series <- db$series
db_locations <- db$locations
db_values <- db$values
cat("done\n")

df_series <- db_series %>%
  mutate(
    flags = coalesce(as.character(flags), "[]"),
    flags = map(flags, function(x) {
      if (x == "[]") {
        return(tibble())
      }
      fromJSON(x)
    }),
    n_flags = map_int(flags, nrow)
  )

cat("setting up flags data frame...")
df_series_flags <- df_series %>%
  filter(n_flags > 0) %>%
  select(id, flags) %>%
  mutate(
    flags = map(flags, function (x) {
      x %>%
        mutate(
          start_date = as.character(as.Date(ymd_hms(start), tz = "US/Eastern")),
          end_date = as.character(as.Date(ymd_hms(end), tz = "US/Eastern")),
          date = map2(start_date, end_date, function (x, y) {
            tibble(date = seq.Date(from = as.Date(x, tz = "US/Eastern"), to = as.Date(y, tz = "US/Eastern"), by = "day"))
          })
        ) %>%
        unnest(date) %>%
        mutate(flagged = TRUE) %>%
        select(date, comment, flagged)
    })
  ) %>%
  unnest(flags)
cat("done\n")

cat("setting up values data frame...")
df_values_all <- db_values %>%
  filter(year(date) <= MAX_YEAR) %>%
  left_join(
    df_series_flags,
    by = c("series_id" = "id", "date")
  ) %>%
  mutate(
    flagged = coalesce(flagged, FALSE)
  ) %>%
  left_join(
    db_series %>%
      select(id, location_id),
    by = c("series_id" = "id")
  ) %>%
  left_join(
    db_locations %>%
      select(id, featureid = catchment_id),
    by = c("location_id" = "id")
  )
cat("done ( nrow =", nrow(df_values_all), ")\n")


# filter: values ------------------------------------------------------------------

cat("removing", sum(df_values_all$flagged), "flagged values...")
rejects$values <- list(
  flagged = df_values_all %>% filter(flagged)
)
df_values_unflagged <- df_values_all %>%
  filter(!flagged) %>%
  select(-flagged, -comment)
cat("done ( nrow =", nrow(df_values_unflagged), ")\n")

cat("nesting values...")
df_series_unflagged <- df_values_unflagged %>%
  group_by(series_id, location_id, featureid) %>%
  nest() %>%
  ungroup()
cat("done ( nrow =", nrow(df_series_unflagged), ")\n")

cat("excluding", sum(is.na(df_series_unflagged$featureid)), "series missing featureid...")
rejects$series <- list(
  missing_featureid = df_series_unflagged %>%
    filter(is.na(featureid))
)
df_series_featureid <- df_series_unflagged %>%
  filter(!is.na(featureid))
cat("done ( nrow =", nrow(df_series_featureid), ")\n")

config_trout <- load_config("../config-trout.sh")
cat("connecting to db (host = ", config_trout$db$host, ", dbname = ", config_trout$db$dbname, ")...", sep = "")
con <- dbConnect(Postgres(), host = config_trout$db$host, dbname = config_trout$db$dbname, user = config_trout$db$user, password = config_trout$db$password)
cat("done\n")

cat("fetching drainage areas...")
featureids <- unique(df_series_featureid$featureid)
df_covariates <- tbl(con, "covariates") %>%
  filter(
    is.na(riparian_distance_ft),
    zone == "upstream",
    variable %in% c("AreaSqKM", "allonnet"),
    featureid %in% featureids
  ) %>%
  collect()
cat("done\n")

cat("disconnecting from db...")
diconnected <- dbDisconnect(con)
cat("done\n")

cat("joining covariates...")
df_covariates <- df_covariates %>%
  select(featureid, variable, value) %>%
  spread(variable, value) %>%
  rename(area_km2 = AreaSqKM) %>%
  mutate(allonnet = coalesce(allonnet, 0))

df_series_featureid_cov <- df_series_featureid %>%
  left_join(
    df_covariates,
    by = "featureid"
  )

stopifnot(all(!is.na(df_series_featureid_cov$area_km2)))
stopifnot(all(!is.na(df_series_featureid_cov$allonnet)))
cat("done (nrow = ", nrow(df_series_featureid_cov), ")\n", sep = "")

cat("excluding", sum(df_series_featureid_cov$area_km2 >= 200), "series with area_km2 >= 200...")
rejects$series$area_km2_gte_200 <- df_series_featureid_cov %>%
  filter(area_km2 <= 200)
df_series_small_area <- df_series_featureid_cov %>%
  filter(area_km2 < 200)
cat("done (nrow = ", nrow(df_series_small_area), ")\n", sep = "")

cat("excluding", sum(df_series_small_area$allonnet >= 50), "series with allonnet >= 50...")
rejects$series$allonnet_gte_50 <- df_series_small_area %>%
  filter(allonnet >= 50)
df_series_unimpounded <- df_series_small_area %>%
  filter(allonnet < 50)
cat("done (nrow = ", nrow(df_series_unimpounded), ")\n", sep = "")

cat("unnesting values...")
df_values_start <- df_series_unimpounded %>%
  unnest(data) %>%
  select(-area_km2, -allonnet)
cat("done (nrow = ", nrow(df_values_start), ")\n", sep = "")

cat("joining daymet data...")
df_values_daymet <- df_values_start %>%
  left_join(
    df_daymet,
    by = c("featureid", "date")
  ) |>
  filter(!is.na(airtemp))
cat("done (nrow = ", nrow(df_values_daymet), ")\n", sep = "")

# remove values where min = mean = max = 0 (represent gaps)
cat("removing zeros...")
df_values_nonzeros <- df_values_daymet %>%
  filter(
    !(min == 0 & mean == 0 & max == 0),
    !(min == 0 & mean < 10 & airtemp > 20)
  )
cat("done (nrow = ", nrow(df_values_nonzeros), ")\n", sep = "")

# remove outliers (max-min > 40)
cat("removing outliers (max-min > 40)")
df_values_nonoutliers <- df_values_nonzeros %>%
  filter((max-min) < 40)
cat("done (nrow = ", nrow(df_values_nonoutliers), ")\n", sep = "")

# manually remove series
exclude_series_id <- c(643L, 3028L, 16013L, 16015L, 23364L, 24039L)
cat("removing manually selected series (n =", length(exclude_series_id), ")\n")
df_values_exclude_series <- df_values_nonoutliers %>%
  filter(!series_id %in% exclude_series_id)
cat("done (nrow = ", nrow(df_values_exclude_series), ")\n", sep = "")

# split series into continuous chunks
df_values_chunks <- df_values_exclude_series %>%
  select(-featureid, -location_id) %>%
  arrange(series_id, date) %>%
  group_by(series_id) %>%
  mutate(
    new_chunk = coalesce(as.numeric(difftime(date, lag(date), "day")), 0) > 1,
    series_chunk = cumsum(new_chunk)
  ) %>%
  select(-new_chunk) %>%
  relocate(series_chunk, .after = series_id)

# trim chunks if n < median(n) on first or last day
df_values_chunks_trim <- df_values_chunks %>%
  group_by(series_id, series_chunk) %>%
  mutate(
    first = row_number() == 1,
    last = row_number() == n(),
    median_n = median(n),
    trim = (first | last) & (n != median(n))
  ) %>%
  ungroup()

df_values_chunks_trim %>%
  filter(trim)

# group by chunk
df_chunks <- df_values_chunks_trim %>%
  filter(!trim) %>%
  select(-first, -last, -median_n, -trim) %>%
  group_by(series_id, series_chunk) %>%
  mutate(
    dmin = coalesce(min - lag(min), 0),
    dmax = coalesce(max - lag(max), 0),
    dmean = coalesce(mean - lag(mean), 0),
    `max-min` = max - min,
    `airtemp-mean` = pmax(airtemp, 0) - pmax(mean, 0)
    # `airtemp-mean` = pmax(airtemp, 10) - pmax(mean, 10)
  ) %>%
  ungroup() %>%
  nest(data = -c(series_id, series_chunk))

# filter: chunks ------------------------------------------------------------------

plot_ts <- function (x) {
  x %>%
    ggplot(aes(date)) +
    geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.25) +
    geom_line(aes(y = airtemp), color = "goldenrod") +
    geom_line(aes(y = mean), color = "deepskyblue") +
    facet_wrap(vars(series_id, series_chunk), labeller = label_both, scales = "free")
}
plot_cor <- function (x) {
  x %>%
    ggplot(aes(mean, airtemp)) +
    geom_abline() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
    facet_wrap(vars(series_id, series_chunk), labeller = label_both, scales = "free")
}
# df_chunks %>%
#   filter(series_id == 19504) %>%
#   unnest(data) %>%
#   plot_ts()
  # plot_cor()

df_chunks_filter_1 <- df_chunks %>%
  rowwise() %>%
  mutate(
    includes_summer = any(month(data$date) %in% 3:11),
    n_values = nrow(data),
    min_value = min(data$min),
    min_mean = min(data$mean),
    mean_mean = mean(data$mean),
    median_mean = median(data$mean),
    max_mean = max(data$mean),
    max_value = max(data$max),
    sd_mean = sd(data$mean),
    max_dmin = max(data$dmin),
    max_dmean = max(data$dmean),
    max_dmax = max(data$dmax),
    `max_max-min` = max(data$`max-min`),
    `min_airtemp-mean` = min(data$`airtemp-mean`),
    `q10_airtemp-mean` = quantile(data$`airtemp-mean`, probs = 0.1),
    `q25_airtemp-mean` = quantile(data$`airtemp-mean`, probs = 0.25),
    `median_airtemp-mean` = median(data$`airtemp-mean`),
    `q75_airtemp-mean` = quantile(data$`airtemp-mean`, probs = 0.75),
    `q90_airtemp-mean` = quantile(data$`airtemp-mean`, probs = 0.9),
    `mean_airtemp-mean` = mean(data$`airtemp-mean`),
    `max_airtemp-mean` = max(data$`airtemp-mean`),
    `sd_airtemp-mean` = sd(data$`airtemp-mean`),
    `q25_abs_airtemp-mean` = quantile(abs(data$`airtemp-mean`), probs = 0.25),
    `median_abs_airtemp-mean` = median(abs(data$`airtemp-mean`)),
    `q75_abs_airtemp-mean` = quantile(abs(data$`airtemp-mean`), probs = 0.75),
    `mean_abs_airtemp-mean` = mean(abs(data$`airtemp-mean`)),
    `max_abs_airtemp-mean` = max(abs(data$`airtemp-mean`)),
    `sd_abs_airtemp-mean` = sd(abs(data$`airtemp-mean`)),
    n_airtemp_gt_5 = sum(data$airtemp > 5),
    pct_airtemp_gt_5 = n_airtemp_gt_5 / n_values
  ) %>%
  print()

cat("removing", sum(df_chunks_filter_1$n_values <= 10),"chunks where n_values <= 10...")
rejects$values$n_values_lt_5 <- df_chunks_filter_1 %>%
  filter(n_values <= 10)
df_chunks_filter_2 <- df_chunks_filter_1 %>%
  filter(n_values > 10)
cat("done (n series = ", nrow(df_chunks_filter_2), ")\n", sep = "")

cat("removing", sum(!df_chunks_filter_2$includes_summer),"chunks that only include winter (Dec-Feb)...")
rejects$values$winter_only <- df_chunks_filter_2 %>%
  filter(!includes_summer)
df_chunks_filter_3 <- df_chunks_filter_2 %>%
  filter(includes_summer)
cat("done (n series = ", nrow(df_chunks_filter_3), ")\n", sep = "")

cat("removing", sum(df_chunks_filter_3$min_value > 30 & df_chunks_filter_3$max_value > 60),"series where min_value > 30 & max_value > 60 (suspect fahranheit)...")
rejects$values$suspect_fahranheit <- df_chunks_filter_3 %>%
  filter(min_value > 30 & max_value > 60)
df_chunks_filter_4 <- df_chunks_filter_3 %>%
  filter(!(min_value > 30 & max_value > 60)) %>%
  mutate(
    cor_airtemp = cor(data$mean, data$airtemp),
    lm_airtemp = list(lm(airtemp ~ mean, data)),
    intercept_airtemp = lm_airtemp$coefficients[[1]],
    slope_airtemp = lm_airtemp$coefficients[[2]]
  ) %>%
  select(-lm_airtemp)
cat("done (n series = ", nrow(df_chunks_filter_4), ")\n", sep = "")

cat("removing", sum(df_chunks_filter_4$n_values > 100 & df_chunks_filter_4$min_value < -10 & df_chunks_filter_4$cor_airtemp > 0.98),"series where n_values > 100 & min_value < -10 & cor_airtemp > 0.98 (suspect air temp measurements)...")
rejects$values$suspect_airtemp <- df_chunks_filter_4 %>%
  filter(n_values > 100, min_value < -10, cor_airtemp > 0.98)
df_chunks_filter_5 <- df_chunks_filter_4 %>%
  filter(!(n_values > 100 & min_value < -10 & cor_airtemp > 0.98))
cat("done (n series = ", nrow(df_chunks_filter_5), ")\n", sep = "")

cat("removing", sum(df_chunks_filter_5$`sd_airtemp-mean` > 8),"series where `sd_airtemp-mean` > 8...")
rejects$values$`sd_airtemp-mean_gt_8` <- df_chunks_filter_5 %>%
  filter(`sd_airtemp-mean` > 8)
df_chunks_filter_6 <- df_chunks_filter_5 %>%
  filter(!(`sd_airtemp-mean` > 8))
cat("done (n series = ", nrow(df_chunks_filter_6), ")\n", sep = "")

cat("removing", sum(df_chunks_filter_6$pct_airtemp_gt_5 < 0.1),"series where pct(airtemp > 5) < 0.1 (freezing)...")
rejects$values$freezing <- df_chunks_filter_6 %>%
  filter(pct_airtemp_gt_5 < 0.1)
df_chunks_filter_7 <- df_chunks_filter_6 %>%
  filter(!(pct_airtemp_gt_5 < 0.1))
cat("done (n series = ", nrow(df_chunks_filter_7), ")\n", sep = "")

cat("removing", sum(df_chunks_filter_7$slope_airtemp < -5 | df_chunks_filter_7$slope_airtemp > 50),"series where slope_airtemp < -5 or > 50 (extreme slopes)...")
rejects$values$slope_airtemp <- df_chunks_filter_7 %>%
  filter(slope_airtemp < -5 | slope_airtemp > 50)
df_chunks_filter_8 <- df_chunks_filter_7 %>%
  filter(!(slope_airtemp < -5 | slope_airtemp > 50))
cat("done (n series = ", nrow(df_chunks_filter_8), ")\n", sep = "")

df_chunks_filter_8 %>%
  arrange(desc(cor_airtemp)) %>%
  head(9) %>%
  # select(-data) %>% view
  unnest(data) %>%
  plot_ts()

df_chunks_filter_8 %>%
  ggplot(aes(max_dmean)) +
  geom_histogram()

df_chunks_filter_9 <- df_chunks_filter_8 %>%
  filter(min_value > -1, max_value < 35, `max_max-min` < 10, n_values >= 10) %>%
  filter(max_dmean < 10, `mean_abs_airtemp-mean` < 5, `max_abs_airtemp-mean` < 10)
# select(-data, -lm_airtemp) %>% summary()

df_chunks_filter_9 %>%
  arrange(desc(`max_max-min`)) %>%
  head(9) %>%
  mutate(series_id = fct_inorder(as.character(series_id))) %>%
  unnest(data) %>%
  plot_ts()

db_huc <- readRDS(file.path(config$wd, "data-huc.rds"))
df_chunks_filter_9_huc <- df_chunks_filter_9 %>%
  left_join(
    db_series %>%
      select(series_id = id, location_id),
    by = "series_id"
  ) %>%
  left_join(
    db_locations %>%
      select(location_id = id, featureid = catchment_id),
    by = "location_id"
  ) %>%
  left_join(
    db_huc,
    by = c("featureid")
  )

df_chunks_filter_9_huc %>%
  select(huc2, huc4, location_id, featureid, series_id, series_chunk, data) %>%
  # pull(huc2) %>% table()
  # filter(huc2 == "04") %>%
  # filter(series_id %in% c(643L, 3028L, 16013L, 16015L, 23364L)) %>%
  unnest(data) %>%
  # filter(yday(date) == 105, mean > 20)
  ggplot(aes(yday(date), mean)) +
  geom_line(aes(group = str_c(series_id, series_chunk, year(date), sep = ":")), alpha = 0.05)

df_chunks_filter_9 %>%
  ggplot(aes(intercept_airtemp, slope_airtemp)) +
  geom_point(aes(color = pct_airtemp_gt_5)) +
  scale_color_viridis_c()

df_chunks_filter_9 %>%
  ggplot(aes(n_values)) +
  stat_ecdf() +
  scale_x_log10()

x <- df_chunks_filter_9 %>%
  select(max_dmax, `max_max-min`, `mean_abs_airtemp-mean`, `max_abs_airtemp-mean`, cor_airtemp, intercept_airtemp, slope_airtemp)
cor(x)
df_chunks_filter_9$mahal <- mahalanobis(x, colMeans(x), cov(x))
df_chunks_filter_9$mahal_p <- pchisq(df_chunks_filter_9$mahal, df=ncol(x) - 1, lower.tail=FALSE)

df_chunks_filter_9 %>%
  arrange(mahal_p) %>%
  filter(mahal_p < 0.01) %>%
  tail(16) %>%
  mutate(series_id = fct_inorder(as.character(series_id))) %>%
  unnest(data) %>%
  plot_ts()

df_chunks_filter_9 %>%
  # select(huc2, huc4, location_id, featureid, series_id, series_chunk, data, mahal_p) %>%
  # pull(huc2) %>% table()
  # filter(huc2 == "04") %>%
  # filter(series_id %in% c(643L, 3028L, 16013L, 16015L, 23364L)) %>%
  unnest(data) %>%
  # filter(yday(date) == 105, mean > 20)
  ggplot(aes(yday(date), mean)) +
  geom_line(aes(group = str_c(series_id, series_chunk, year(date), sep = ":")), alpha = 0.05) +
  facet_wrap(vars(mahal_p < 0.001))


# filter: locations -------------------------------------------------------

# which locations have overlapping series with different means?
df_chunks_locations <- df_chunks_filter_9 %>%
  select(series_id, series_chunk, data) %>%
  left_join(
    db_series %>%
      select(series_id = id, location_id),
    by = "series_id"
  ) %>%
  left_join(
    db_locations %>%
      select(location_id = id, featureid = catchment_id),
    by = "location_id"
  ) %>%
  unnest(data) %>%
  select(featureid, location_id, series_id, series_chunk, date, min, mean, max, airtemp, prcp) %>%
  # group_by(featureid, location_id, date) %>%
  # mutate(n = n(), range = diff(range(mean))) %>%
  nest(data = -c(featureid, location_id)) %>%
  mutate(
    dups = map(data, function (x) {
      if (!any(duplicated(x$date))) {
        return(tibble())
      }
      x %>%
        group_by(date) %>%
        summarise(
          n = n(),
          range = diff(range(mean)),
          .groups = "drop"
        ) %>%
        filter(
          n > 1,    # overlapping
          range > 0 # different values
        )
    })
  ) %>%
  rowwise() %>%
  mutate(
    n_dups = nrow(dups),
    max_range = if_else(n_dups > 0, max(dups$range), NA_real_)
  )

df_chunks_locations %>%
  filter(n_dups > 0, max_range > 5) %>%
  unnest(data) %>%
  # filter(year(date) == 2009) %>%
  ggplot(aes(date, mean)) +
  geom_point(aes(color = factor(series_id))) +
  geom_line(aes(y = airtemp)) +
  # ggplot(aes(mean, airtemp)) +
  # geom_abline(color = "red") +
  # geom_point() +
  facet_wrap(~location_id, scales = "free")

# some series seem to have surface/bottom measurements
# just take the mean of each day
# exclude dates where diff(mean) > 5
cat("calculating mean of duplicate dates by location...")
df_locations_daily <- df_chunks_locations %>%
  select(-dups, -n_dups, -max_range) %>%
  unnest(data) %>%
  group_by(featureid, location_id, date) %>%
  summarise(
    n = n(),
    range = diff(range(mean)),
    mean = mean(mean),
    airtemp = mean(airtemp),
    prcp = mean(prcp),
    .groups = "drop"
  ) %>%
  ungroup()
cat("done (nrow = ", nrow(df_locations_daily), ")\n", sep = "")


cat("joining location-flowline distance...")
df_location_featureid_distance <- read_csv(
  file.path(config$wd, "locations-flowlines-distance.csv"),
  col_types = cols(
    location_id = col_integer(),
    featureid = col_integer(),
    line_distance_m = col_double(),
    pour_distance_m = col_double()
  )
)

df_locations_daily_distance <- df_locations_daily %>%
  left_join(df_location_featureid_distance, by = c("location_id", "featureid"))
cat("done ( nrow =", nrow(df_locations_daily_distance), ")\n")

df_locations_daily_distance %>%
  distinct(featureid, location_id, line_distance_m, pour_distance_m) %>%
  gather(var, value, line_distance_m, pour_distance_m) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~var, scales = "free")

df_locations_daily_distance %>%
  distinct(featureid, location_id, line_distance_m, pour_distance_m) %>%
  ggplot(aes(line_distance_m, pour_distance_m)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

# identify line_distance cutoff
df_locations_daily_distance %>%
  count(featureid, location_id, line_distance_m, pour_distance_m) %>%
  arrange(line_distance_m) %>%
  mutate(cumsum_n = cumsum(n) / sum(n)) %>%
  ggplot(aes(line_distance_m, cumsum_n)) +
  geom_point() +
  geom_vline(xintercept = 60)

# no good pour_distance cutoff
df_locations_daily_distance %>%
  count(featureid, location_id, line_distance_m, pour_distance_m) %>%
  filter(line_distance_m > 60) %>%
  arrange(pour_distance_m) %>%
  mutate(cumsum_n = cumsum(n) / sum(n)) %>%
  ggplot(aes(pour_distance_m, cumsum_n)) +
  geom_point()

cat("excluding", length(unique(df_locations_daily_distance[df_locations_daily_distance$line_distance_m > 60, ]$location_id)), "locations with flowline distance > 60 m")
rejects$locations <- list(
  line_distance_gt_60m <- df_locations_daily_distance %>%
    filter(line_distance_m > 60)
)
df_locations_near_flowline <- df_locations_daily_distance %>%
  filter(line_distance_m <= 60)
cat("done (nrow = ", nrow(df_locations_near_flowline), ")\n", sep = "")


# filter: catchment multiple locations within catchment ------------------------------

df_featureid_daily <- df_locations_near_flowline %>%
  nest(data = -featureid) %>%
  mutate(
    locations = map(data, function (x) {
      x %>%
        group_by(location_id, line_distance_m, pour_distance_m) %>%
        summarise(
          n = n(),
          n_summer = sum(month(date) %in% 4:10),
          .groups = "drop"
        )
    }),
    n_locations = map_int(locations, nrow)
  )

df_featureid_daily %>%
  filter(n_locations > 1) %>%
  arrange(desc(n_locations))

# df_featureid_daily %>%
#   filter(featureid == 201492321) %>%
#   unnest(data) %>%
#   ggplot(aes(date, mean, color = factor(location_id))) +
#   geom_point()
#
#
# df_values_featureid_multi <- df_values_featureid %>%
#   filter(n_locations > 1) %>%
#   arrange(desc(n_locations))
#
# df_values_featureid_multi$locations[[1]]
#
# df_values_featureid_multi$data[[1]] %>%
#   select(location_id, date, mean) %>%
#   spread(location_id, mean) %>%
#   select(-date) %>%
#   cor(use = "pairwise.complete.obs")

cat("filtering for locations with most data points within each catchment...")
df_featureid_daily_count <- df_featureid_daily %>%
  mutate(
    locations = map2(data, locations, function (x, y) {
      loc <- y %>%
        ungroup() %>%
        arrange(desc(n_summer), pour_distance_m)
      loc$location_rank <- 1:nrow(loc)
      loc
    })
  ) %>%
  select(-data, -n_locations) %>%
  unnest(locations)

rejects$locations$rank_gt_1 <- df_featureid_daily_count %>%
  filter(location_rank > 1)
df_featureid_daily_location <- df_locations_near_flowline %>%
  filter(!location_id %in% rejects$locations$rank_gt_1$location_id)
cat("done (nrow = ", nrow(df_featureid_daily_location), ", n locations excluded = ", nrow(rejects$locations$rank_gt_1), ")\n", sep = "")

df_featureid_daily_location |>
  ggplot(aes(yday(date), mean)) +
  geom_hex(bins = 100)

# export ------------------------------------------------------------------

cat("saving to data-clean.rds...")
list(
  data = df_featureid_daily_location %>%
    select(featureid, location_id, date, mean, airtemp, prcp),
  rejects = rejects
) %>%
  write_rds(file.path(config$wd, "data-clean.rds"))
cat("done\n")


# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))
cat("finished data-clean: ", as.character(end, tz = "US/Eastern"), " (elapsed = ", round(elapsed / 60, digits = 1), " min)\n", sep = "")
