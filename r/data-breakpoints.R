# generate breakpoints
# <- {wd}/data-clean.rds
# -> {wd}/data-breakpoints.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting data-process:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(sensorQC))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(broom))

config <- fromJSON("../config.json")


# load data ---------------------------------------------------------------

df <- readRDS(file.path(config$wd, "data-clean.rds"))

df <- df %>%
  mutate(
    year = year(date),
    featureid_year = paste(featureid, year, sep = "-")
  ) %>%
  group_by(featureid, year, location_id, featureid_year) %>%
  nest() %>%
  mutate(
    n_data = map_int(data, nrow),
    data = map(data, function (x) {
      x <- x %>%
        mutate(
          mean = if_else(mean < 0, 0, mean),
          temp_index = (mean - airtemp) / (mean + 0.00000001),
          roll_temp_index = NA_real_
        )
      if (nrow(x) >= 10) {
        x$roll_temp_index <- rollapply(x$temp_index, width = 10, fill = NA, mean)
      }
      x
    }),
    breaks = map(data, function (x) {
      x <- x %>%
        filter(
          yday(date) >= 150,
          yday(date) <= 250
        )
      stopifnot(all(!is.na(x$temp_index)))
      temp_index_qtiles <- quantile(x$temp_index, probs = c(0.001, 0.999))
      data_frame(
        q_n = nrow(x),
        q_lo = temp_index_qtiles[1],
        q_med = median(x$temp_index),
        q_hi = temp_index_qtiles[2]
      )
    })
  )

df %>%
  unnest(breaks) %>%
  ggplot(aes(q_lo)) +
  geom_histogram()
df %>%
  unnest(breaks) %>%
  ggplot(aes(q_hi)) +
  geom_histogram()

df %>%
  unnest(breaks) %>%
  ggplot(aes(q_hi-q_lo)) +
  geom_histogram()

# Set range (dOY) and count for assigning spring BP
complete_spring_jday <- c(15, 175)
complete_spring_n <- round((complete_spring_jday[2] - complete_spring_jday[1]) * 0.9)
# Set range (dOY) and count for assigning fall BP
complete_fall_jday <- c(225, 350)
complete_fall_n <- round((complete_fall_jday[2] - complete_fall_jday[1]) * 0.9)
# Number of days in a row that need to be within the CIs to get assigned synchronised (referred to as numForward range)
numForwardSpring <- 10
numForwardFall   <- 16

df <- df %>%
  mutate(
    complete_spring = map_lgl(data, function (x) {
      x <- x %>%
        filter(
          yday(date) >= complete_spring_jday[1],
          yday(date) <= complete_spring_jday[2]
        )
      nrow(x) >= complete_spring_n
    }),
    complete_fall = map_lgl(data, function (x) {
      x <- x %>%
        filter(
          yday(date) >= complete_fall_jday[1],
          yday(date) <= complete_fall_jday[2]
        )
      nrow(x) >= complete_fall_n
    })
  )

# df_complete <- df %>%
#   filter(complete_either)
#
# df_complete$data[[2]] %>%
#   select(date, mean, airtemp, temp_index, roll_mean) %>%
#   gather(var, value, -date) %>%
#   ggplot(aes(date, value)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~var, ncol = 1)

df_spring <- df %>%
  filter(complete_spring) %>%
  mutate(
    data_sync = map2(data, breaks, function (x, breaks) {
      x %>%
        mutate(jday = yday(date)) %>%
        filter(jday >= 1, jday <= (200 + numForwardSpring - 1)) %>%
        complete(jday = 1:(200 + numForwardSpring - 1)) %>%
        mutate(
          sync = roll_temp_index >= breaks$q_lo & roll_temp_index <= breaks$q_hi,
          sync = coalesce(sync, TRUE),
          roll_sync = rollapply(sync, width = numForwardSpring, align = "left", all, fill = NA)
        )
    }),
    spring_bp = map_int(data_sync, function (x) {
      as.integer(min(yday(x$date[x$roll_sync]), na.rm = TRUE))
    })
  )

df_fall <- df %>%
  filter(complete_fall) %>%
  mutate(
    data_sync = map2(data, breaks, function (x, breaks) {
      x %>%
        mutate(jday = yday(date)) %>%
        filter(jday >= 225, jday <= (350 + numForwardFall - 1)) %>%
        complete(jday = 1:(350 + numForwardSpring - 1)) %>%
        mutate(
          sync = roll_temp_index >= breaks$q_lo & roll_temp_index <= breaks$q_hi,
          sync = coalesce(sync, TRUE),
          roll_sync = rollapply(sync, width = numForwardFall, align = "left", all, fill = NA)
        )
    }),
    fall_bp = map_int(data_sync, function (x) {
      as.integer(max(yday(x$date[x$roll_sync]), na.rm = TRUE))
    })
  )


df <- df %>%
  mutate(
    data_spring = pmap(list(data, breaks, complete_spring), function (x, breaks, complete) {
      out <- data_frame()
      if (complete) {
        out <- x %>%
          mutate(jday = yday(date)) %>%
          filter(jday >= 1, jday <= (200 + numForwardSpring - 1)) %>%
          complete(jday = 1:(200 + numForwardSpring - 1)) %>%
          mutate(
            sync = roll_temp_index >= breaks$q_lo & roll_temp_index <= breaks$q_hi,
            sync = coalesce(sync, FALSE),
            roll_sync = rollapply(sync, width = numForwardSpring, align = "left", all, fill = NA)
          )
      }
      out
    }),
    spring_bp = map_int(data_spring, function (x) {
      out <- NA_integer_
      if (nrow(x) > 0) {
        out <- as.integer(min(yday(x$date[x$roll_sync]), na.rm = TRUE))
      }
      out
    }),
    data_fall = pmap(list(data, breaks, complete_fall), function (x, breaks, complete) {
      out <- data_frame()
      if (complete) {
        out <- x %>%
          mutate(jday = yday(date)) %>%
          filter(jday >= 225, jday <= (350 + numForwardFall - 1)) %>%
          complete(jday = 225:(350 + numForwardFall - 1)) %>%
          mutate(
            sync = roll_temp_index >= breaks$q_lo & roll_temp_index <= breaks$q_hi,
            sync = coalesce(sync, FALSE),
            roll_sync = rollapply(sync, width = numForwardFall, align = "right", all, fill = NA)
          )
      }
      out
    }),
    fall_bp = map_int(data_fall, function (x) {
      out <- NA_integer_
      if (nrow(x) > 0) {
        out <- as.integer(max(yday(x$date[x$roll_sync]), na.rm = TRUE))
      }
      out
    })
  )

df %>%
  select(spring_bp, fall_bp) %>%
  gather(var, value) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~ var, scales = "free")

df %>%
  filter(!is.na(spring_bp)) %>%
  arrange(desc(spring_bp)) %>%
  head(20) %>%
  unnest(data_spring) %>%
  filter(yday(date) >= 1, yday(date) <= 200) %>%
  ggplot(aes(date, mean)) +
  geom_line(aes(y = airtemp)) +
  geom_point(aes(color = roll_sync)) +
  facet_wrap(~featureid_year, scales = "free_x")

df %>%
  filter(!is.na(spring_bp)) %>%
  arrange(spring_bp) %>%
  head(20) %>%
  unnest(data_spring) %>%
  filter(yday(date) >= 1, yday(date) <= 200) %>%
  ggplot(aes(date, mean)) +
  geom_line(aes(y = airtemp)) +
  geom_point(aes(color = roll_sync)) +
  facet_wrap(~featureid_year, scales = "free_x")

df %>%
  filter(!is.na(fall_bp)) %>%
  arrange(desc(fall_bp)) %>%
  head(20) %>%
  unnest(data_fall) %>%
  filter(yday(date) >= 225, yday(date) <= 365) %>%
  ggplot(aes(date, mean)) +
  geom_line(aes(y = airtemp)) +
  geom_point(aes(color = roll_sync)) +
  facet_wrap(~featureid_year, scales = "free_x")

df %>%
  filter(!is.na(fall_bp)) %>%
  arrange(fall_bp) %>%
  head(20) %>%
  unnest(data_fall) %>%
  filter(yday(date) >= 225, yday(date) <= 350) %>%
  ggplot(aes(date, mean)) +
  geom_line(aes(y = airtemp)) +
  geom_point(aes(color = roll_sync)) +
  facet_wrap(~featureid_year, scales = "free_x")

df %>%
  filter(
    !is.na(fall_bp),
    fall_bp > 350
  ) %>%
  unnest(data_fall) %>%
  filter(yday(date) >= 225, yday(date) <= 350) %>%
  ggplot(aes(date, mean)) +
  geom_line(aes(y = airtemp)) +
  geom_point(aes(color = roll_sync)) +
  facet_wrap(~featureid_year, scales = "free_x")


# compute average breakpoints ---------------------------------------------

cat("loading hucs...")
df_huc <- readRDS(file.path(config$wd, "huc.rds"))
cat("done\n")

cat("joining huc...")
df <- df %>%
  left_join(df_huc, by = "featureid")
cat("done\n")

cat("compute mean breakpoints by featureid/huc...")
df_featureid <- df %>%
  group_by(featureid) %>%
  summarize(
    spring_bp_featureid_n = sum(!is.na(spring_bp)),
    spring_bp_featureid_mean = mean(spring_bp, na.rm = TRUE),
    fall_bp_featureid_n = sum(!is.na(fall_bp)),
    fall_bp_featureid_mean = mean(fall_bp, na.rm = TRUE)
  )

df_huc4 <- df %>%
  group_by(huc4) %>%
  summarize(
    spring_bp_huc4_n = sum(!is.na(spring_bp)),
    spring_bp_huc4_mean = mean(spring_bp, na.rm = TRUE),
    fall_bp_huc4_n = sum(!is.na(fall_bp)),
    fall_bp_huc4_mean = mean(fall_bp, na.rm = TRUE)
  )

df_huc8 <- df %>%
  group_by(huc8) %>%
  summarize(
    spring_bp_huc8_n = sum(!is.na(spring_bp)),
    spring_bp_huc8_mean = mean(spring_bp, na.rm = TRUE),
    fall_bp_huc8_n = sum(!is.na(fall_bp)),
    fall_bp_huc8_mean = mean(fall_bp, na.rm = TRUE)
  )

df_huc12 <- df %>%
  group_by(huc12) %>%
  summarize(
    spring_bp_huc12_n = sum(!is.na(spring_bp)),
    spring_bp_huc12_mean = mean(spring_bp, na.rm = TRUE),
    fall_bp_huc12_n = sum(!is.na(fall_bp)),
    fall_bp_huc12_mean = mean(fall_bp, na.rm = TRUE)
  )
cat("done\n")



cat("fill missing breakpoints with featureid/huc means...")
df_fill <- df %>%
  select(-data, -breaks, -data_spring, -data_fall) %>%
  rename(
    spring_bp_data = spring_bp,
    fall_bp_data = fall_bp
  ) %>%
  mutate(
    spring_bp_data = as.numeric(spring_bp_data),
    fall_bp_data = as.numeric(fall_bp_data)
  ) %>%
  left_join(df_featureid, by = "featureid") %>%
  left_join(df_huc4, by = "huc4") %>%
  left_join(df_huc8, by = "huc8") %>%
  left_join(df_huc12, by = "huc12") %>%
  mutate(
    spring_bp_all_n = sum(!is.na(spring_bp_data)),
    spring_bp_all_median = median(spring_bp_data, na.rm = TRUE),
    fall_bp_all_n = sum(!is.na(fall_bp_data)),
    fall_bp_all_median = median(fall_bp_data, na.rm = TRUE)
  )

df_fill <- df_fill %>%
  mutate(
    spring_bp_src = if_else(
      !is.na(spring_bp_data),
      "data",
      NA_character_
    ),
    spring_bp = if_else(
      !is.na(spring_bp_data),
      spring_bp_data,
      NA_real_
    ),
    fall_bp_src = if_else(
      !is.na(fall_bp_data),
      "data",
      NA_character_
    ),
    fall_bp = if_else(
      !is.na(fall_bp_data),
      fall_bp_data,
      NA_real_
    )
  ) %>%
  mutate(
    spring_bp_src = if_else(
      is.na(spring_bp) & !is.na(spring_bp_featureid_mean),
      "featureid",
      spring_bp_src
    ),
    spring_bp = if_else(
      is.na(spring_bp) & !is.na(spring_bp_featureid_mean),
      spring_bp_featureid_mean,
      spring_bp
    ),
    fall_bp_src = if_else(
      is.na(fall_bp) & !is.na(fall_bp_featureid_mean),
      "featureid",
      fall_bp_src
    ),
    fall_bp = if_else(
      is.na(fall_bp) & !is.na(fall_bp_featureid_mean),
      fall_bp_featureid_mean,
      fall_bp
    )
  ) %>%
  mutate(
    spring_bp_src = if_else(
      is.na(spring_bp) & !is.na(spring_bp_huc12_mean),
      "huc12",
      spring_bp_src
    ),
    spring_bp = if_else(
      is.na(spring_bp) & !is.na(spring_bp_huc12_mean),
      spring_bp_huc12_mean,
      spring_bp
    ),
    fall_bp_src = if_else(
      is.na(fall_bp) & !is.na(fall_bp_huc12_mean),
      "huc12",
      fall_bp_src
    ),
    fall_bp = if_else(
      is.na(fall_bp) & !is.na(fall_bp_huc12_mean),
      fall_bp_huc12_mean,
      fall_bp
    )
  ) %>%
  mutate(
    spring_bp_src = if_else(
      is.na(spring_bp) & !is.na(spring_bp_huc8_mean),
      "huc8",
      spring_bp_src
    ),
    spring_bp = if_else(
      is.na(spring_bp) & !is.na(spring_bp_huc8_mean),
      spring_bp_huc8_mean,
      spring_bp
    ),
    fall_bp_src = if_else(
      is.na(fall_bp) & !is.na(fall_bp_huc8_mean),
      "huc8",
      fall_bp_src
    ),
    fall_bp = if_else(
      is.na(fall_bp) & !is.na(fall_bp_huc8_mean),
      fall_bp_huc8_mean,
      fall_bp
    )
  ) %>%
  mutate(
    spring_bp_src = if_else(
      is.na(spring_bp) & !is.na(spring_bp_huc4_mean),
      "huc4",
      spring_bp_src
    ),
    spring_bp = if_else(
      is.na(spring_bp) & !is.na(spring_bp_huc4_mean),
      spring_bp_huc4_mean,
      spring_bp
    ),
    fall_bp_src = if_else(
      is.na(fall_bp) & !is.na(fall_bp_huc4_mean),
      "huc4",
      fall_bp_src
    ),
    fall_bp = if_else(
      is.na(fall_bp) & !is.na(fall_bp_huc4_mean),
      fall_bp_huc4_mean,
      fall_bp
    )
  ) %>%
  mutate(
    spring_bp_src = if_else(
      is.na(spring_bp) & !is.na(spring_bp_all_median),
      "all",
      spring_bp_src
    ),
    spring_bp = if_else(
      is.na(spring_bp) & !is.na(spring_bp_all_median),
      spring_bp_all_median,
      spring_bp
    ),
    fall_bp_src = if_else(
      is.na(fall_bp) & !is.na(fall_bp_all_median),
      "all",
      fall_bp_src
    ),
    fall_bp = if_else(
      is.na(fall_bp) & !is.na(fall_bp_all_median),
      fall_bp_all_median,
      fall_bp
    )
  )

stopifnot(all(!is.na(df_fill$spring_bp)))
stopifnot(all(!is.na(df_fill$fall_bp)))

table(df_fill$spring_bp_src)
table(df_fill$fall_bp_src)

df_fill %>%
  ggplot(aes(spring_bp, fill = spring_bp_src)) +
  geom_histogram()

df_fill %>%
  ggplot(aes(fall_bp, fill = fall_bp_src)) +
  geom_histogram()


# export ------------------------------------------------------------------

list(
  model = df_fill %>%
    select(featureid, year, featureid_year, spring_bp, fall_bp),
  data = df,
  featureid = df_featureid,
  huc4 = df_huc4,
  huc8 = df_huc8,
  huc12 = df_huc12
) %>%
  saveRDS(file.path(config$wd, "data-breakpoints.rds"))

