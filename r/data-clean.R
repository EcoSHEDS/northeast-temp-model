# process raw data (clean, qaqc)
# <- {wd}/data-db.rds
# <- {wd}/data-daymet.csv
# -> {wd}/data-clean.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting data-clean:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(sensorQC))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(broom))

source("functions.R")

config <- load_config()

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
cat("done ( nrow =", nrow(df_daymet), ", max year =", MAX_YEAR, ")\n")

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

cat("removing", sum(df_values$flagged), "observations with user flag...")
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
df_values <- df_values %>%
  filter(!is.na(featureid))
cat("done ( nrow =", nrow(df_values), ")\n")

cat("connecting to db ( host =", config$db$host, ", dbname =", config$db$dbname, ")...")
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
cat("done ( nrow =", nrow(df_values), ")\n")


cat("excluding", sum(df_values$area_km2 >= 200), "series with area_km2 >= 200...")
df_values <- df_values %>%
  filter(area_km2 < 200)
cat("done ( nrow =", nrow(df_values), ")\n")

cat("excluding", sum(df_values$allonnet >= 50), "series with allonnet >= 50...")
df_values <- df_values %>%
  filter(allonnet < 50)
cat("done ( nrow =", nrow(df_values), ")\n")

cat("unnesting values...")
df_values <- df_values %>%
  unnest(data) %>%
  select(-area_km2, -allonnet)
cat("done ( nrow =", nrow(df_values), ")\n")

cat("joining daymet data...")
df_values <- df_values %>%
  left_join(
    df_daymet,
    by = c("featureid", "date")
  )
cat("done ( nrow =", nrow(df_values), ")\n")

cat("removing", sum(df_values$mean < -25),"values where mean temp < -25 (assume NA indicators)...")
df_values <- df_values %>%
  filter(mean >= -25)
cat("done ( nrow =", nrow(df_values), ")\n")

cat("removing", sum(df_values$mean > 35),"values where mean temp > 35...")
df_values <- df_values %>%
  filter(mean <= 35)
cat("done ( nrow =", nrow(df_values), ")\n")

cat("removing", sum(df_values$max > 35),"values where max temp > 35...")
df_values <- df_values %>%
  filter(max <= 35)
cat("done ( nrow =", nrow(df_values), ")\n")

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
  ggplot(aes(date)) +
  geom_point(aes(y = mean), color = "red") +
  geom_line(aes(y = airtemp)) +
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
df_values <- df_values %>%
  filter(!series_id %in% airtemp_exclude$series_id)
cat("done ( nrow =", nrow(df_values), ")\n")

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

# df_values %>%
#   ggplot(aes(mean, airtemp)) +
#   geom_point(size = 1, alpha = 0.1) +
#   geom_abline(color = "red")
#
# df_raw %>%
#   filter(airtemp < 5, mean > 18) %>%
#   select(series_id) %>%
#   distinct() %>%
#   head() %>%
#   left_join(df_raw, by = "series_id") %>%
#   filter(year(date) == 2012) %>%
#   ggplot(aes(date, mean)) +
#   geom_point() +
#   geom_line(aes(y = airtemp)) +
#   facet_wrap(~series_id)
#   # ggplot(aes(mean, airtemp)) +
#   # geom_point() +
#   # geom_abline(color = "red") +
#   # facet_wrap(~series_id, scales = "free")
#
# df_values %>%
#   mutate(
#     abs_diff = abs(airtemp - mean)
#   ) %>%
#   ggplot(aes(abs_diff)) +
#   geom_histogram()
#
# df_values %>%
#   mutate(
#     abs_diff = abs(airtemp - mean)
#   ) %>%
#   filter(month(date) %in% 4:10) %>%
#   ggplot(aes(mean, airtemp)) +
#   geom_point(aes(color = abs_diff > 15), size = 1, alpha = 0.1) +
#   geom_abline(color = "red")
#
# df_values %>%
#   mutate(
#     abs_diff = abs(airtemp - mean)
#   ) %>%
#   filter(
#     abs_diff > 15,
#     month(date) %in% 4:10
#   ) %>%
#   select(series_id) %>%
#   distinct() %>%
#   left_join(df_values, by = "series_id") %>%
#   ggplot(aes(date, mean)) +
#   geom_point() +
#   geom_line(aes(y = airtemp)) +
#   facet_wrap(~series_id, scales = "free_x")


cat("nesting values...")
df_values <- df_values %>%
  group_by(series_id, location_id, featureid) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow)
  )
cat("done ( nrow =", nrow(df_values), ")\n")

cat("removing", sum(df_values$n < 5),"series with count < 5...")
df_values <- df_values %>%
  filter(n >= 5)
cat("done ( nrow =", nrow(df_values), ")\n")


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

df_values_sensor_flags %>%
  select(-data, -sensor, -flags, -flag_i, -data_flag) %>%
  arrange(desc(n_exclude)) %>%
  filter(n_exclude > 0)

df_values_sensor_flags %>%
  arrange(desc(n_exclude)) %>%
  filter(n_exclude > 0) %>%
  unnest(data_flag) %>%
  ggplot(aes(date, mean)) +
  geom_point(aes(color = flagged)) +
  geom_line(aes(y = airtemp)) +
  # ggplot(aes(mean, airtemp)) +
  # geom_abline(color = "red") +
  # geom_point() +
  facet_wrap(~series_id, scales = "free")

df_values_sensor_flags <- df_values_sensor_flags %>%
  select(series_id, location_id, featureid, data_flag) %>%
  unnest(data_flag)


cat("removing", sum(df_values_sensor_flags$flagged),"observations with persist(mean) > 5 and mean > 3...")
df_values <- df_values_sensor_flags %>%
  filter(!flagged) %>%
  select(-flagged)
cat("done ( nrow =", nrow(df_values), ")\n")


cat("nesting values...")
df_values <- df_values %>%
  group_by(series_id, location_id, featureid) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow)
  )
cat("done ( nrow =", nrow(df_values), ")\n")


# remove first or last day of each subseries if incomplete (frequency(t) < median(frequency(t)))
cat("trimming start/end values of each series if freq < median(freq)...")
df_values <- df_values %>%
  mutate(
    data = map(data, function (x_raw) {
      f <- median(x_raw$n) # median frequency
      x <- x_raw

      if (nrow(x) > 3) {
        if (x[["n"]][1] < x[["n"]][2]) {
          x <- x[-1, ]
        }
        if (x[["n"]][nrow(x)] < x[["n"]][nrow(x)-1]) {
          x <- x[-nrow(x), ]
        }
      }
      x
    }),
    n = map_int(data, nrow)
    # freq = map(data, function (x) {
    #   data_frame(
    #     freq_min = min(x$n),
    #     freq_q10 = quantile(x$n, probs = 0.1),
    #     freq_mean = mean(x$n),
    #     freq_median = median(x$n),
    #     freq_q90 = quantile(x$n, probs = 0.9),
    #     freq_max = max(x$n),
    #     freq_n_unique = length(unique(x$n))
    #   )
    # })
  )
  # unnest(freq)
cat("done ( nrow =", nrow(df_values), ")\n")

# # trim diagnostics
# df_trim %>%
#   select(-data) %>%
#   summary()
#
# table(df_trim$n_day)
# summary(df_trim$n_day)
# table(df_trim$freq_median)
#
# df_trim %>%
#   ggplot(aes(n_day)) +
#   geom_histogram()
#
# df_trim %>%
#   mutate(
#     start = map_chr(data, ~ as.character(min(.$date))),
#     end = map_chr(data, ~ as.character(max(.$date))),
#     start = as.Date(start),
#     end = as.Date(end),
#     n_day_diff = as.numeric(end - start) + 1,
#     n_day_equal = n_day == n_day_diff
#   ) %>%
#   select(-data) %>%
#   summary
#
# # n_day_equal should be all TRUE


cat("removing", sum(df_values$n < 5), "series with length < 5...")
df_values <- df_values %>%
  filter(n >= 5)
cat("done ( nrow =", nrow(df_values), ")\n")

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


# df_values_location %>%
#   filter(max_range > 5) %>%
#   unnest(data) %>%
#   # filter(year(date) == 2009) %>%
#   ggplot(aes(date, mean)) +
#   geom_point(aes(color = factor(series_id))) +
#   geom_line(aes(y = airtemp)) +
#   # ggplot(aes(mean, airtemp)) +
#   # geom_abline(color = "red") +
#   # geom_point() +
#   facet_wrap(~location_id, scales = "free")

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
          n_series = length(mean),
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
        ) %>%
        filter(
          mean_range < 5
        )
    })
  ) %>%
  unnest(data)
cat("done ( nrow =", nrow(df_values), ")\n")



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

cat("excluding", length(unique(df_values[df_values$line_distance_m > 60, ]$location_id)), "locations with flowline distance > 100 m")
df_values <- df_values %>%
  filter(line_distance_m <= 60)
cat("done ( nrow =", nrow(df_values), ")\n")



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

df_values_featureid %>%
  filter(n_locations > 1) %>%
  arrange(desc(n_locations))
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
df_values <- df_values_featureid %>%
  mutate(
    data = map2(data, locations, function (x, y) {
      loc <- y %>%
        arrange(desc(n_summer), pour_distance_m) %>%
        head(1)
      x %>%
        filter(location_id == loc$location_id[1])
    })
  ) %>%
  select(-locations, -n_locations) %>%
  unnest(data)
cat("done ( nrow =", nrow(df_values), ")\n")


# df_values %>%
#   ggplot(aes(airtemp, mean)) +
#   geom_point()
#
#
# df_values %>%
#   group_by(featureid) %>%
#   summarise(
#     n = length(date)
#   )
#

# df_values_huc <- df_values %>%
#   left_join(df_huc %>% select(featureid, huc2), by = "featureid") %>%
#   mutate(jday = yday(date)) %>%
#   group_by(huc2, jday) %>%
#   mutate(
#     mean_q01 = quantile(mean, probs = 0.01),
#     mean_q99 = quantile(mean, probs = 0.99),
#     mean_airtemp = mean - airtemp,
#     mean_airtemp_q01 = quantile(mean_airtemp, probs = 0.01),
#     mean_airtemp_q99 = quantile(mean_airtemp, probs = 0.99)
#   ) %>%
#   ungroup()
#
# df_values_huc %>%
#   ggplot(aes(jday, mean)) +
#   geom_point(size = 1, alpha = 0.2) +
#   facet_wrap(~ huc2)
#
# df_values_quantile <- df_values %>%
#   mutate(jday = yday(date)) %>%
#   group_by(jday) %>%
#   mutate(
#     mean_q01 = quantile(mean, probs = 0.01),
#     mean_q99 = quantile(mean, probs = 0.99),
#     mean_airtemp = mean - airtemp,
#     mean_airtemp_q01 = quantile(mean_airtemp, probs = 0.01),
#     mean_airtemp_q99 = quantile(mean_airtemp, probs = 0.99)
#   )
#
# df_values_quantile %>%
#   ggplot(aes(yday(date), mean, color = mean > (mean_q99 + 5))) +
#   geom_point(size = 1, alpha = 0.2) +
#   geom_line(
#     data = df_values %>%
#       mutate(jday = yday(date)) %>%
#       group_by(jday) %>%
#       summarize(
#         q01 = quantile(mean, probs = 0.01),
#         q99 = quantile(mean, probs = 0.99)
#       ) %>%
#       gather(var, value, -jday),
#     aes(jday, value, color = var)
#   )
#
# df_values_quantile %>%
#   ggplot(aes(yday(date), mean_airtemp)) +
#   geom_point(size = 1, alpha = 0.2) +
#   geom_line(
#     data = df_values_quantile %>%
#       select(jday, mean_airtemp_q01, mean_airtemp_q99) %>%
#       distinct() %>%
#       gather(var, value, -jday),
#     aes(jday, value, color = var)
#   )
#
#
# df_values_quantile %>%
#   ggplot(aes(yday(date), mean_airtemp, color = mean_airtemp > (mean_airtemp_q99 + 5))) +
#   geom_point(size = 1, alpha = 0.2)
#
#
# df_values_quantile %>%
#   filter(
#     location_id %in% c(8246, 1050, 1353, 8190, 8252, 8155),
#     year(date) == 2012
#   ) %>%
#   ggplot(aes(date, mean_airtemp)) +
#   geom_point(aes(color = mean_airtemp > (mean_airtemp_q99 + 5))) +
#   geom_line(aes(y = mean_airtemp_q99)) +
#   facet_wrap(~location_id)
#
#
# df_values_quantile %>%
#   filter(jday > 300, mean > (mean_q99 + 5)) %>%
#   mutate(year = year(date)) %>%
#   group_by(location_id, year) %>%
#   count() %>%
#   arrange(desc(nn))
#
# df_values_quantile %>%
#   filter(
#     location_id %in% c(8246, 1050, 1353, 8190, 8252, 8155),
#     year(date) == 2012
#   ) %>%
#   ggplot(aes(date, mean)) +
#   geom_point(aes(color = mean > (mean_q99 + 5))) +
#   geom_line(aes(y = mean_q99)) +
#   facet_wrap(~location_id)
#
# df_values_quantile %>%
#   filter(location_id==8161) %>%
#   ggplot(aes(date, mean)) +
#   geom_point(aes(color = mean > (mean_q99 + 5))) +
#   geom_line(aes(y = mean_q99))
#
# df_values_quantile %>%
#   filter(location_id==1050, year(date) == 2012) %>%
#   ggplot(aes(date, mean)) +
#   geom_point(aes(color = mean > (mean_q99 + 5))) +
#   geom_line(aes(y = mean_q99))
#
# df_values_quantile %>%
#   filter(location_id==1353, year(date) == 2012) %>%
#   ggplot(aes(date, mean)) +
#   geom_point(aes(color = mean > (mean_q99 + 5))) +
#   geom_line(aes(y = mean_q99))

# SUMMARY
# 1. Gaps - Identify gaps of MIN_GAP_LENGTH or less and split into subseries
# 2. Trim - Remove first/last day of each subseries if not complete
# 3. QAQC - Apply QAQC checks and flag series/values

# df_gap <- df_raw %>%
#   mutate(
#     gaps = map(data_raw, function (x) {
#       gaps <- data_frame(
#         gap_n = 0,
#         gap_min = NA_real_,
#         gap_median = NA_real_,
#         gap_mean = NA_real_,
#         gap_max = NA_real_
#       )
#       if (nrow(x) > 1) {
#         diff_days <- as.numeric(difftime(x$date, lag(x$date), units = "days"))[-1] - 1
#         diff_days <- diff_days[diff_days > 0]
#         if (length(diff_days) > 0) {
#           gaps <- data_frame(
#             gap_n = length(diff_days),
#             gap_min = min(diff_days),
#             gap_median = median(diff_days),
#             gap_mean = mean(diff_days),
#             gap_max = max(diff_days)
#           )
#         }
#       }
#       gaps
#     })
#   ) %>%
#   unnest(gaps)
# table(df_gap$gap_n)
# table(df_gap$gap_min)


# gaps --------------------------------------------------------------------
#
# MAX_GAP_LENGTH <- 1
#
# df_gap <- df_year %>%
#   mutate(
#     data = map(data, function (x) {
#       x %>%
#         arrange(date) %>%
#         mutate(
#           diff_date = c(1, as.numeric(diff(date, unit = "day"))),
#           gap_split = 1 * (diff_date > MAX_GAP_LENGTH),
#           gap_id = cumsum(gap_split)
#         ) %>%
#         select(-diff_date, -gap_split)
#     })
#   )
# df_gap <- df_gap %>%
#   unnest(data) %>%
#   mutate(series_id_year_gap = paste(series_id_year, gap_id, sep = "-")) %>%
#   group_by(series_id, location_id, featureid, year, series_id_year, gap_id, series_id_year_gap) %>%
#   nest()


# trim --------------------------------------------------------------------

# qaqc series -------------------------------------------------------------

# df_qaqc_series <- df_trim %>%
#   mutate(
#     reject = FALSE,
#     reason = ""
#   )
#
# assign_flag <- function (df, idx, reason) {
#   cat("Rejected", length(idx), "series for:", reason, "\n")
#   df[["reject"]][idx] <- TRUE
#   df[["reason"]][idx] <- paste0(df[["reason"]][idx], paste0(reason, "; "))
#   df
# }
#
# # n_day < 10
# idx <- with(df_qaqc_series, which(n_day < MIN_SERIES_LENGTH))
# df_qaqc_series <- assign_flag(df_qaqc_series, idx, reason = paste0("Duration less than ", MIN_SERIES_LENGTH, " days"))
#
# # 1 < median frequency < 24
# idx <- with(df_qaqc_series, which(!freq_median %in% c(1, 12, 24 > 1 & freq_median < 24 & freq_median != 12))
# df_qaqc_series <- assign_flag(df_qaqc_series, idx, "Median daily frequency > 1 and < 24 (but not 12)")
#
# saveRDS(df_qaqc_series, file.path(config$wd, "data", "df_qaqc_series.rds"))

# exclude subseries with duration less than MIN_SERIES_DURATION
# MIN_SERIES_DURATION <- 5
#
# cat("excluding", sum(df_trim$n_day < MIN_SERIES_DURATION), "with duration <", MIN_SERIES_DURATION, "days...")
# df_trim <- df_trim %>%
#   filter(n_day >= MIN_SERIES_DURATION)
# cat("done (", nrow(df_trim), "remaining )\n")
#
# cat("excluding", sum(df_trim$freq_n_unique > 10), "with # unique frequencies > 10...")
# df_trim <- df_trim %>%
#   filter(freq_n_unique <= 10)
# cat("done (", nrow(df_trim), "remaining )\n")
#
# cat("excluding", sum(!df_trim$freq_median %in% c(1, 6, 12, 24, 48, 96, 144, 288, 360, 480, 720, 1080, 1440)), "with non-standard frequencies...")
# df_trim <- df_trim %>%
#   filter(freq_median %in% c(1, 6, 12, 24, 48, 96, 144, 288, 360, 480, 720, 1080, 1440))
# cat("done (", nrow(df_trim), "remaining )\n")
#


# qaqc values -------------------------------------------------------------

# df_qaqc_values <- df_trim %>%
#   mutate(
#     data = map(data, function (x) {
#       x %>%
#         mutate(
#           flag_cold = mean < -1,
#           flag_hot = mean > 35,
#           flag_extreme_hot = mean > quantile(mean, probs = 0.999),
#           flag_extreme_cold = mean < quantile(mean, probs = 0.001),
#           flag_delta_gt5 = coalesce(abs(mean - lag(mean)) > 5, FALSE)
#         )
#     }),
#     flags = map(data, function (x) {
#       data_frame(
#         frac_flag_cold = mean(x$flag_cold),
#         frac_flag_hot = mean(x$flag_hot),
#         frac_flag_extreme_hot = mean(x$flag_extreme_hot),
#         frac_flag_extreme_cold = mean(x$flag_extreme_cold),
#         frac_flag_delta_gt5 = mean(x$flag_delta_gt5)
#       )
#     })
#   ) %>%
#   unnest(flags)
#
# df_qaqc_values %>%
#   select(-data) %>%
#   View()
#
# df_qaqc_values %>%
#   select(series_id_year_gap, starts_with("frac_")) %>%
#   View
#
# df_qaqc_values %>%
#   arrange(desc(frac_flag_cold)) %>%
#   head(20) %>%
#   unnest(data) %>%
#   ggplot(aes(date, mean)) +
#   geom_line() +
#   facet_wrap(~ series_id_year_gap, scales = "free")
#
#
#
# df_qaqc_values %>%
#   filter(series_id_year_gap == "6480-2014-0") %>%
#   unnest(data) %>%
#   ggplot(aes(date, mean)) +
#   geom_line() +
#   facet_wrap(~ series_id_year_gap)
#
# df_qaqc_values %>%
#   unnest(data) %>%
#   filter(mean < -25) %>%
#   select(series_id_year_gap) %>%
#   distinct()

# sensorQC ----------------------------------------------------------------

# df_qaqc_series <- readRDS(file.path(config$wd, "data", "df_qaqc_series.rds"))
#
# df_qaqc_sensorQC <- df_trim %>%
#   mutate(
#     data = map(data, function (x) {
#       x$mad30 <- MAD(x$mean, rep(30, length(x$mean)))
#       x$mad60 <- MAD(x$mean, rep(60, length(x$mean)))
#       x
#     }),
#     sensor = map(data, ~ sensor(data_frame(times = .$date, x = .$mean))),
#     flags = map(sensor, ~ flag(., 'x < 0', 'persist(x) > 3')),
#     n_flag_1 = map_int(flags, ~ length(.$flags[[1]]$flag.i)),
#     n_flag_2 = map_int(flags, ~ length(.$flags[[2]]$flag.i))
#   )
#
# df_qaqc_sensorQC <- df_qaqc_sensorQC %>%
#   mutate(
#     sensor = map(data, ~ sensor(data_frame(times = .$date, x = .$mean))),
#     sensor_w5 = map(sensor, ~ window(., n = 5, type = "rolling")),
#     sensor_w10 = map(sensor, ~ window(., n = 10, type = "rolling")),
#     sensor_w30 = map(sensor, ~ window(., n = 30, type = "rolling")),
#     sensor_w60 = map(sensor, ~ window(., n = 60, type = "rolling"))
#   )
#
# df_qaqc_sensorQC <- df_qaqc_sensorQC %>%
#   mutate(
#     sensor_flags = map(sensor, ~ flag(., 'x <= 2', 'persist(x) > 2')),
#     sensor_w5_flags = map(sensor_w5, ~ flag(., 'x <= 2', 'persist(x) > 2', 'MAD(x,w) > 10')),
#     sensor_w10_flags = map(sensor_w10, ~ flag(., 'x <= 2', 'persist(x) > 2', 'MAD(x,w) > 10')),
#     sensor_w30_flags = map(sensor_w30, ~ flag(., 'x <= 2', 'persist(x) > 2', 'MAD(x,w) > 10')),
#     sensor_w60_flags = map(sensor_w60, ~ flag(., 'x <= 2', 'persist(x) > 2', 'MAD(x,w) > 10'))
#   )
#
# df_qaqc_sensorQC <- df_qaqc_sensorQC %>%
#   mutate(
#     df_sensor_flags = map2(data, sensor_flags, function (x, f) {
#       x$flag <- ""
#       for (i in seq_along(f$flags)) {
#         x$flag[f$flags[[i]]$flag.i] <- paste(x$flag[f$flags[[i]]$flag.i], f$flags[[i]]$expression, sep = "|")
#       }
#       x
#     }),
#     df_sensor_w5_flags = map2(data, sensor_w5_flags, function (x, f) {
#       x$flag <- ""
#       for (i in seq_along(f$flags)) {
#         x$flag[f$flags[[i]]$flag.i] <- paste(x$flag[f$flags[[i]]$flag.i], f$flags[[i]]$expression, sep = "|")
#       }
#       x
#     }),
#     df_sensor_w10_flags = map2(data, sensor_w10_flags, function (x, f) {
#       x$flag <- ""
#       for (i in seq_along(f$flags)) {
#         x$flag[f$flags[[i]]$flag.i] <- paste(x$flag[f$flags[[i]]$flag.i], f$flags[[i]]$expression, sep = "|")
#       }
#       x
#     }),
#     df_sensor_w30_flags = map2(data, sensor_w30_flags, function (x, f) {
#       x$flag <- ""
#       for (i in seq_along(f$flags)) {
#         x$flag[f$flags[[i]]$flag.i] <- paste(x$flag[f$flags[[i]]$flag.i], f$flags[[i]]$expression, sep = "|")
#       }
#       x
#     }),
#     df_sensor_w60_flags = map2(data, sensor_w60_flags, function (x, f) {
#       x$flag <- ""
#       for (i in seq_along(f$flags)) {
#         x$flag[f$flags[[i]]$flag.i] <- paste(x$flag[f$flags[[i]]$flag.i], f$flags[[i]]$expression, sep = "|")
#       }
#       x
#     })
#   )
#
# df_qaqc_sensorQC %>%
#   arrange(desc(n_flag_1)) %>%
#   select(-data, -sensor, -flags) %>%
#   View
#
#
# plot_mad <- function(i, xlim = NULL) {
#   p1 <- df_qaqc_sensorQC$data[[i]] %>%
#     ggplot(aes(date, mean)) +
#     geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2) +
#     geom_line() +
#     geom_point() +
#     geom_line(aes(y = airtemp), color = "orangered")
#   p2 <- df_qaqc_sensorQC$data[[i]] %>%
#     select(date, mad30, mad60) %>%
#     gather(var, value, -date) %>%
#     ggplot(aes(date, value, color = var)) +
#     geom_line() +
#     geom_point() +
#     theme(
#       legend.position = c(0, 1),
#       legend.justification = c(0, 1)
#     )
#
#   if(!is.null(xlim)) {
#     p1 <- p1 + xlim(xlim)
#     p2 <- p2 + xlim(xlim)
#   }
#
#   gridExtra::grid.arrange(p1, p2, ncol = 1)
# }
# plot_mad(145)
#
# plot_mad(which(df_qaqc_sensorQC$series_id_year_gap == "10257-2014-0"))
# plot_mad(which(df_qaqc_sensorQC$series_gap_id == "4694-0"))
#
# # MAD(x, 60) > 10
# plot_mad(which(df_qaqc_sensorQC$series_gap_id == "14305-0"))
# plot_mad(which(df_qaqc_sensorQC$series_gap_id == "14305-0"), xlim = ymd(c(20140415, 20140515)))
# plot_mad(which(df_qaqc_sensorQC$series_gap_id == "10296-0"))
# # TODO: REMOVE mean < 5 FIRST, THEN GAP SPLIT
#
# plot_mad(which(df_qaqc_sensorQC$series_gap_id == "10293-0"))
# plot_mad(which(df_qaqc_sensorQC$series_gap_id == "2052-0")) # WEIRD
# plot_mad(which(df_qaqc_sensorQC$series_gap_id == "11324-0"))
#
# df_qaqc_sensorQC %>%
#   unnest(data) %>%
#   ggplot(aes(mad60)) +
#   geom_histogram() +
#   xlim(-0.0001, 5)
#
# df_qaqc_sensorQC %>%
#   unnest(data) %>%
#   filter(mad60 >= 10) %>%
#   arrange(mad60) %>%
#   select(-starts_with("freq_"))
#
#
# # qaqc values -------------------------------------------------------------
#
# df_qaqc_values <- df_qaqc_series %>%
#   filter(!reject)
#
# df_qaqc_values[["data"]][[1]] %>%
#   ggplot(aes(date, mean)) +
#   geom_point()
#
# df_qaqc_values <- df_qaqc_values %>%
#   mutate(
#     data = map(data, function (x) {
#       x %>%
#         mutate(
#           mean_delta = coalesce(lag(mean) - mean, 0),
#           max_min_diff = max - min,
#           mean_zero = mean <= 0
#         )
#     })
#     # qaqc_stats = map(data, function (x) {
#     #   data_frame(
#     #     mean_delta_min = min(x$mean_delta, na.rm = TRUE),
#     #     mean_delta_median = median(x$mean_delta, na.rm = TRUE),
#     #     mean_delta_max = max(x$mean_delta, na.rm = TRUE),
#     #     min_max_diff_min = min(x$min_max_diff),
#     #     min_max_diff_median = median(x$min_max_diff),
#     #     min_max_diff_max = max(x$min_max_diff),
#     #     mean_zero_count = sum(x$mean_zero)
#     #   )
#     # })
#   )
#
# df_qaqc_values2 <- df_qaqc_values %>%
#   mutate(
#     qaqc_stats = map(data, function (x) {
#       data_frame(
#         mean_delta_min = min(x$mean_delta),
#         mean_delta_median = median(x$mean_delta),
#         mean_delta_max = max(x$mean_delta),
#         max_min_diff_min = min(x$max_min_diff),
#         max_min_diff_median = median(x$max_min_diff),
#         max_min_diff_max = max(x$max_min_diff),
#         mean_zero_count = sum(x$mean_zero)
#       )
#     })
#   ) %>%
#   unnest(qaqc_stats)
#
# df_qaqc_values2 %>%
#   select(-data) %>%
#   summary()
#
# df_qaqc_values2 %>%
#   select(-data) %>%
#   View
#
# df_qaqc_values %>%
#   unnest(data) %>%
#   ggplot(aes(mean_delta)) +
#   geom_histogram() +
#   xlim(-10, 10)
#
# df_qaqc_values %>%
#   unnest(data) %>%
#   ggplot(aes(max_min_diff)) +
#   geom_histogram(binwidth = 0.5) +
#   xlim(0, 20)
#
# df_qaqc_values[which(df_qaqc_values$series_gap_id == "2979-0"), ]$data[[1]] %>%
#   ggplot(aes(date)) +
#   geom_line(aes(y = mean))


cat("saving to data-clean.rds...")
df_values %>%
  select(featureid, location_id, date, mean, airtemp, prcp) %>%
  saveRDS(file.path(config$wd, "data-clean.rds"))
cat("done\n")

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))
cat("finished data-clean:", as.character(end, tz = "US/Eastern"), "( elapsed =", round(elapsed / 60, digits = 1), "min )\n")
