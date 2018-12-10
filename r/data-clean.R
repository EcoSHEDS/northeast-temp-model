# process raw data (clean, qaqc)
# <- {wd}/data-db.rds
# <- {wd}/data-daymet.csv
# -> {wd}/data-clean.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting data-clean: ", as.character(start, tz = "US/Eastern"), "\n", sep = "")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(sensorQC))
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
  select(featureid, year) %>%
  distinct() %>%
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
    flags = coalesce(flags, "[]"),
    flags = map(flags, function(x) {
      if (x == "[]") {
        return(data_frame())
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
            data_frame(date = seq.Date(from = as.Date(x, tz = "US/Eastern"), to = as.Date(y, tz = "US/Eastern"), by = "day"))
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
df_values <- db_values %>%
  mutate(date = as.Date(date, tz = "UTC")) %>%
  filter(year(date) <= MAX_YEAR) %>%
  left_join(
    df_series_flags,
    by = c("series_id" = "id", "date")
  ) %>%
  mutate(
    flagged = coalesce(flagged, FALSE)
  )

df_values <- df_values %>%
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
cat("done ( nrow =", nrow(df_values), ")\n")


# filter ------------------------------------------------------------------

cat("removing", sum(df_values$flagged), "observations with user flag...")
rejects$values <- list(
  flagged = df_values %>% filter(flagged)
)
df_values <- df_values %>%
  filter(!flagged) %>%
  select(-flagged, -comment)
cat("done ( nrow =", nrow(df_values), ")\n")

cat("nesting values...")
df_values <- df_values %>%
  group_by(series_id, location_id, featureid) %>%
  nest()
cat("done ( nrow =", nrow(df_values), ")\n")

cat("excluding", sum(is.na(df_values$featureid)), "series missing featureid...")
rejects$series <- list(
  missing_featureid = df_values %>%
    filter(is.na(featureid))
)
df_values <- df_values %>%
  filter(!is.na(featureid))
cat("done ( nrow =", nrow(df_values), ")\n")

cat("connecting to db (host = ", config$db$host, ", dbname = ", config$db$dbname, ")...", sep = "")
con <- dbConnect(PostgreSQL(), host = config$db$host, dbname = config$db$dbname, user = config$db$user, password = config$db$password)
cat("done\n")

cat("fetching drainage areas...")
featureids <- unique(df_values$featureid)
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

df_values <- df_values %>%
  left_join(
    df_covariates,
    by = "featureid"
  )

stopifnot(all(!is.na(df_values$area_km2)))
stopifnot(all(!is.na(df_values$allonnet)))
cat("done (nrow = ", nrow(df_values), ")\n", sep = "")


cat("excluding", sum(df_values$area_km2 >= 200), "series with area_km2 >= 200...")
rejects$series$area_km2_gte_200 <- df_values %>%
  filter(area_km2 <= 200)
df_values <- df_values %>%
  filter(area_km2 < 200)
cat("done (nrow = ", nrow(df_values), ")\n", sep = "")

cat("excluding", sum(df_values$allonnet >= 50), "series with allonnet >= 50...")
rejects$series$allonnet_gte_50 <- df_values %>%
  filter(allonnet >= 50)
df_values <- df_values %>%
  filter(allonnet < 50)
cat("done (nrow = ", nrow(df_values), ")\n", sep = "")

cat("unnesting values...")
df_values <- df_values %>%
  unnest(data) %>%
  select(-area_km2, -allonnet)
cat("done (nrow = ", nrow(df_values), ")\n", sep = "")

cat("joining daymet data...")
df_values <- df_values %>%
  left_join(
    df_daymet,
    by = c("featureid", "date")
  )
cat("done (nrow = ", nrow(df_values), ")\n", sep = "")

cat("removing", sum(df_values$mean < -25),"values where mean temp < -25 (assume NA indicators)...")
rejects$values$mean_temp_lt_m25 <- df_values %>%
  filter(mean < -25)
df_values <- df_values %>%
  filter(mean >= -25)
cat("done ( nrow =", nrow(df_values), ")\n")

cat("removing", sum(df_values$mean > 35),"values where mean temp > 35...")
rejects$values$mean_temp_gt_35 <- df_values %>%
  filter(mean > 35)
df_values <- df_values %>%
  filter(mean <= 35)
cat("done (nrow = ", nrow(df_values), ")\n", sep = "")

cat("removing", sum(df_values$max > 35),"values where max temp > 35...")
rejects$values$max_temp_gt_35 <- df_values %>%
  filter(max > 35)
df_values <- df_values %>%
  filter(max <= 35)
cat("done (nrow = ", nrow(df_values), ")\n", sep = "")

# air temperature correlation
# n > 10, slope in [0.95, 1.05], intercept in [-0.5, 0.5]
df_values_airtemp <- df_values %>%
  group_by(series_id, location_id, featureid) %>%
  mutate(
    n = n()
  ) %>%
  filter(n > 10) %>%
  group_by(series_id, location_id, featureid) %>%
  nest() %>%
  mutate(
    lm = map(data, ~ lm(airtemp ~ mean, data = .)),
    glance = map(lm, glance),
    r.squared = map_dbl(glance, "r.squared"),
    intercept = map_dbl(lm, function (x) {
      tidy(x)$estimate[1]
    }),
    slope = map_dbl(lm, function (x) {
      tidy(x)$estimate[2]
    })
  ) %>%
  arrange(desc(r.squared))

df_values_airtemp %>%
  head(20) %>%
  unnest(data) %>%
  ggplot(aes(mean, airtemp)) +
  geom_point() +
  geom_abline(color = "red") +
  facet_wrap(~series_id, scales = "free")

df_values_airtemp %>%
  head(3) %>%
  unnest(data) %>%
  select(series_id, date, watertemp = mean, airtemp) %>%
  gather(var, value, -series_id, -date) %>%
  ggplot(aes(date, value, color = var)) +
  geom_line() +
  scale_color_manual("", values = c("orangered", "deepskyblue")) +
  facet_wrap(~series_id, scales = "free", ncol = 1)


airtemp_exclude <- df_values_airtemp %>%
  filter(
    slope >= 0.95,
    slope <= 1.05,
    intercept >= -1,
    intercept <= 1,
    r.squared > 0.95
  )

cat("removing", nrow(airtemp_exclude), "series with high air temp correlation...")
rejects$series$airtemp_corr <- airtemp_exclude
df_values <- df_values %>%
  filter(!series_id %in% airtemp_exclude$series_id)
cat("done (nrow = ", nrow(df_values), ")\n", sep = "")

# 502 - high correlation in winter, but not summer
df_values %>%
  filter(min < -10) %>%
  select(series_id) %>%
  unique() %>%
  head() %>%
  left_join(df_values, by = "series_id") %>%
  ggplot(aes(date, mean)) +
  geom_line(aes(y = airtemp)) +
  geom_point() +
  geom_point(aes(y = min), color = "red") +
  facet_wrap(~series_id, scales = "free")

df_values %>%
  filter(min < -10) %>%
  select(series_id) %>%
  unique() %>%
  head() %>%
  left_join(df_values, by = "series_id") %>%
  ggplot(aes(mean, airtemp)) +
  geom_point() +
  geom_abline(color = "red") +
  facet_wrap(~series_id, scales = "free")

cat("nesting values...")
df_values <- df_values %>%
  group_by(series_id, location_id, featureid) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow)
  )
cat("done (nrow = ", nrow(df_values), ")\n", sep = "")

cat("removing", sum(df_values$n < 5),"series with count < 5...")
rejects$series$n_lt_5 <- df_values %>%
  filter(n < 5)
df_values <- df_values %>%
  filter(n >= 5)
cat("done (nrow = ", nrow(df_values), ")\n", sep = "")


# persist(x) > 5 & x > 3
df_values_sensor <- df_values %>%
  mutate(
    sensor = map(data, ~ sensor(data_frame(times = .$date, x = .$mean))),
    flags = map(sensor, ~ flag(., 'persist(x) > 5', 'x > 3'))
  )
df_values_sensor_flags <- df_values_sensor %>%
  mutate(
    n_flag = map_int(flags, ~ length(.$flags[[1]]$flag.i)),
    flag_i = map(flags, ~ intersect(.$flags[[1]]$flag.i, .$flags[[2]]$flag.i)),
    n_exclude = map_int(flag_i, length),
    data_flag = map2(data, flag_i, function (x, y) {
      x$flagged <- FALSE
      if (length(y) > 0) {
        x[y, "flagged"] <- TRUE
      }
      x
    }),
    n_data_flag = map_int(data_flag, ~ sum(!.$flagged)),
    check = (n_data_flag + n_exclude) == n
  )
stopifnot(all(df_values_sensor_flags$check))
#
# df_values_sensor_flags %>%
#   select(-data, -sensor, -flags, -flag_i, -data_flag) %>%
#   arrange(desc(n_exclude)) %>%
#   filter(n_exclude > 0)
#
# df_values_sensor_flags %>%
#   arrange(desc(n_exclude)) %>%
#   filter(n_exclude > 0) %>%
#   unnest(data_flag) %>%
#   ggplot(aes(date, mean)) +
#   geom_point(aes(color = flagged)) +
#   geom_line(aes(y = airtemp)) +
#   # ggplot(aes(mean, airtemp)) +
#   # geom_abline(color = "red") +
#   # geom_point() +
#   facet_wrap(~series_id, scales = "free")

df_values_sensor_flags <- df_values_sensor_flags %>%
  select(series_id, location_id, featureid, data_flag) %>%
  unnest(data_flag)

cat("removing", sum(df_values_sensor_flags$flagged),"observations with persist(mean) > 5 and mean > 3...")
rejects$values$sensor_persist <- df_values_sensor_flags %>%
  filter(flagged)
df_values <- df_values_sensor_flags %>%
  filter(!flagged) %>%
  select(-flagged)
cat("done (nrow = ", nrow(df_values), ")\n", sep = "")

cat("nesting values...")
df_values <- df_values %>%
  group_by(series_id, location_id, featureid) %>%
  nest()
cat("done (nrow = ", nrow(df_values), ")\n", sep = "")


# remove first or last day of each subseries if incomplete (frequency(t) != frequency(t-1 or t+1))
cat("trimming start/end values of each series if freq differs from adjacent day...")
df_values <- df_values %>%
  mutate(
    data = map(data, function (x_raw) {
      f <- median(x_raw$n) # median frequency
      x <- x_raw
      x$trim <- FALSE

      if (nrow(x) > 3) {
        if (x[["n"]][1] < x[["n"]][2]) {
          x <- x[-1, ]
          x[1, "trim"] <- TRUE
        }
        if (x[["n"]][nrow(x)] < x[["n"]][nrow(x)-1]) {
          x <- x[-nrow(x), ]
          x[nrow(x), "trim"] <- TRUE
        }
      }
      x
    })
  ) %>%
  unnest(data)
rejects$values$trim_ends <- df_values %>%
  filter(trim)
df_values <- df_values %>%
  filter(!trim) %>%
  select(-trim)
cat("done (nrow = ", nrow(df_values), ", n trimmed = ", nrow(rejects$values$trim_ends), ")\n", sep = "")

cat("nesting values...")
df_values <- df_values %>%
  group_by(series_id, location_id, featureid) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow)
  )
cat("done (nrow = ", nrow(df_values), ")\n", sep = "")

cat("removing", sum(df_values$n < 5), "series with length < 5...")
rejects$values$n_lt_5_2 <- df_values %>%
  filter(n < 5)
df_values <- df_values %>%
  filter(n >= 5)
cat("done (nrow = ", nrow(df_values), ")\n", sep = "")

# which locations have overlapping series with different means?
df_values_location <- df_values %>%
  unnest(data) %>%
  group_by(location_id, featureid) %>%
  nest() %>%
  mutate(
    date_dups = map(data, function (x) {
      x %>%
        group_by(date) %>%
        summarise(
          n = length(mean),
          range = diff(range(mean))
        ) %>%
        filter(
          n > 1,    # overlapping
          range > 0 # different values
        )
    }),
    n_date_dups = map_int(date_dups, nrow),
    max_range = map_dbl(date_dups, function (x) {
      m <- 0
      if (nrow(x) > 0) {
        m <- max(x$range)
      }
      m
    })
  )

df_values_location %>%
  filter(max_range > 5) %>%
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
df_values <- df_values_location %>%
  select(-n_date_dups, -max_range, -date_dups) %>%
  mutate(
    data = map(data, function (x) {
      x %>%
        group_by(date) %>%
        summarise(
          series = paste(series_id, collapse = ","),
          n_series = length(series_id),
          min_range = diff(range(min)),
          min = mean(min),
          mean_range = diff(range(mean)),
          mean = mean(mean),
          max_range = diff(range(max)),
          max = mean(max),
          n_range = diff(range(n)),
          n = mean(n),
          airtemp = mean(airtemp),
          prcp = mean(prcp)
        )
    })
  ) %>%
  unnest(data)

rejects$values$mean_range_gt_5 <- df_values %>%
  filter(
    mean_range >= 5
  )
df_values <- df_values %>%
  filter(
    mean_range < 5
  )
cat("done (nrow = ", nrow(df_values), ", n filter = ", nrow(rejects$values$mean_range_gt_5), ")\n", sep = "")



# multiple locations per featureid ----------------------------------------

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

df_values <- df_values %>%
  left_join(df_location_featureid_distance, by = c("location_id", "featureid"))
cat("done ( nrow =", nrow(df_values), ")\n")

df_values %>%
  select(featureid, location_id, line_distance_m, pour_distance_m) %>%
  distinct() %>%
  gather(var, value, line_distance_m, pour_distance_m) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~var, scales = "free")

df_values %>%
  group_by(featureid, location_id, line_distance_m, pour_distance_m) %>%
  distinct() %>%
  ggplot(aes(line_distance_m, pour_distance_m)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

# identify line_distance cutoff
df_values %>%
  group_by(featureid, location_id, line_distance_m, pour_distance_m) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  arrange(line_distance_m) %>%
  mutate(cumsum_n = cumsum(n) / sum(n)) %>%
  ggplot(aes(line_distance_m, cumsum_n)) +
  geom_point() +
  geom_vline(xintercept = 60)

# no good pour_distance cutoff
df_values %>%
  group_by(featureid, location_id, line_distance_m, pour_distance_m) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  filter(line_distance_m > 60) %>%
  arrange(pour_distance_m) %>%
  mutate(cumsum_n = cumsum(n) / sum(n)) %>%
  ggplot(aes(pour_distance_m, cumsum_n)) +
  geom_point()

cat("excluding", length(unique(df_values[df_values$line_distance_m > 60, ]$location_id)), "locations with flowline distance > 60 m")
rejects$locations <- list(
  line_distance_gt_60m <- df_values %>%
    filter(line_distance_m > 60)
)
df_values <- df_values %>%
  filter(line_distance_m <= 60)
cat("done (nrow = ", nrow(df_values), ")\n", sep = "")


# filter multiple locations within catchment ------------------------------

df_values_featureid <- df_values %>%
  group_by(featureid) %>%
  nest() %>%
  mutate(
    locations = map(data, function (x) {
      x %>%
        group_by(location_id, line_distance_m, pour_distance_m) %>%
        summarise(
          n = length(date),
          n_summer = sum(month(date) %in% 4:10)
        )
    }),
    n_locations = map_int(locations, nrow)
  )

# df_values_featureid %>%
#   filter(n_locations > 1) %>%
#   arrange(desc(n_locations))
#
# df_values_featureid %>%
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
df_featureid_locations <- df_values_featureid %>%
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

rejects$locations$rank_gt_1 <- df_featureid_locations %>%
  filter(location_rank > 1)
df_values <- df_values %>%
  filter(!location_id %in% rejects$locations$rank_gt_1$location_id)
cat("done (nrow = ", nrow(df_values), ", n locations excluded = ", nrow(rejects$locations$rank_gt_1), ")\n", sep = "")


# export ------------------------------------------------------------------

cat("saving to data-clean.rds...")
list(
  data = df_values %>%
    select(featureid, location_id, date, mean, airtemp, prcp),
  rejects = rejects
) %>%
  saveRDS(file.path(config$wd, "data-clean.rds"))
cat("done\n")


# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))
cat("finished data-clean: ", as.character(end, tz = "US/Eastern"), " (elapsed = ", round(elapsed / 60, digits = 1), " min)\n", sep = "")
