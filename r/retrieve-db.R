library(RPostgreSQL)
library(tidyverse)
library(jsonlite)
library(lubridate)

config <- fromJSON("../config.json")


locations_exclude <- read_table(
  file.path(config$wd, "locations-exclude.txt"), col_names = "location_id",
  col_types = cols(
    location_id = col_integer()
  )
)

cat("Excluding", nrow(locations_exclude), "locations\n")

con <- dbConnect(PostgreSQL(), host = config$db$host, dbname = config$db$dbname, user = config$db$user)

db_agencies <- tbl(con, "agencies")
df_agencies <- collect(db_agencies)

agencies_exclude <- df_agencies %>%
  filter(name %in% config$dataset$agencies$exclude) %>%
  select(agency_id = id)
cat("Excluding", nrow(agencies_exclude), "agencies\n")

db_locations <- tbl(con, "locations") %>%
  filter(
    !id %in% locations_exclude$location_id,
    !agency_id %in% agencies_exclude$agency_id
  )
df_locations <- collect(db_locations)
cat("Locations Count:", nrow(df_locations), "\n")

db_series <- tbl(con, "series") %>%
  select(-flags) %>%
  filter(
    location_id %in% df_locations$id,
    reviewed == TRUE,
    value_count > 10
  )
df_series <- collect(db_series)
cat("Series Count:", nrow(df_series), "\n")

# df_elapsed = data_frame()
# for (n in c(10, 100, 200, 500, 1000, 2000)) {
# # for (n in c(10, 20, 50, 100)) {
#   cat("n =", n, "\n")
#   x <- system.time({
#     db_values <- tbl(con, "values") %>%
#       filter(
#         series_id %in% db_series$id[1:n]
#       ) %>%
#       mutate(
#         date = date_trunc("day", datetime)
#       ) %>%
#       group_by(series_id, date) %>%
#       summarize(
#         min = min(value),
#         mean = mean(value),
#         max = max(value),
#         n = n()
#       ) %>%
#       arrange(series_id, date)
#     # db_values %>% show_query()
#     df_values <- collect(db_values)
#   })
#   df_elapsed <- bind_rows(df_elapsed, data_frame(n = n, elapsed = x[3]))
# }
#
# df_elapsed %>%
#   ggplot(aes(n, elapsed)) +
#   geom_point() +
#   geom_line()

# about 0.01 seconds per series

db_values <- tbl(con, "values") %>%
  filter(
    series_id %in% df_series$id
  ) %>%
  mutate(
    date = date_trunc("day", datetime)
  ) %>%
  group_by(series_id, date) %>%
  summarize(
    min = min(value),
    mean = mean(value),
    max = max(value),
    n = n()
  ) %>%
  arrange(series_id, date) %>%
  ungroup()
# db_values %>% show_query()

system.time(df_values <- collect(db_values))
#   user  system elapsed
# 14.765   4.654 538.003 (9 min)

df_values <- df_values %>%
  mutate(date = as.Date(date))

# df_values %>%
#   ggplot(aes(date, mean)) +
#   geom_line() +
#   facet_wrap(~series_id)

df_values_nested <- df_values %>%
  group_by(series_id) %>%
  nest(.key = "data_raw")

df_raw <- df_series %>%
  left_join(df_values_nested, by = c("id" = "series_id"))

# trim (remove first or last day if incomplete)
df_trim <- df_raw %>%
  mutate(
    data = map(data_raw, function (x_raw) {
      f <- median(x_raw$n) # median frequency
      x <- x_raw

      if (nrow(x) > 3) {
        if (x[["n"]][1] < f) {
          x <- x[-1, ]
        }
        if (x[["n"]][nrow(x)] < f) {
          x <- x[-nrow(x), ]
        }
      }
      x
    }),
    n_day = map_int(data, nrow),
    freq = map(data, function (x) {
      data_frame(
        freq_min = min(x$n),
        freq_q10 = quantile(x$n, probs = 0.1),
        freq_mean = mean(x$n),
        freq_median = median(x$n),
        freq_q90 = quantile(x$n, probs = 0.9),
        freq_max = max(x$n)
      )
    })
  ) %>%
  unnest(freq)

# histogram of time series durations (days)
df_trim %>%
  ggplot(aes(n_day)) +
  geom_histogram()
table(df_trim$n_day)
summary(df_trim$n_day)

# identify gaps

df_gap <- df_trim %>%
  mutate(
    gaps = map(data, function (x) {
      gaps <- data_frame(
        gap_n = 0,
        gap_min = NA_real_,
        gap_median = NA_real_,
        gap_mean = NA_real_,
        gap_max = NA_real_
      )
      if (nrow(x) > 1) {
        diff_days <- as.numeric(difftime(x$date, lag(x$date), units = "days"))[-1] - 1
        diff_days <- diff_days[diff_days > 0]
        if (length(diff_days) > 0) {
          gaps <- data_frame(
            gap_n = length(diff_days),
            gap_min = min(diff_days),
            gap_median = median(diff_days),
            gap_mean = mean(diff_days),
            gap_max = max(diff_days)
          )
        }
      }
      gaps
    })
  ) %>%
  unnest(gaps)
table(df_gap$gap_n)
table(df_gap$gap_min)

x <- filter(df_gap, gap_n == 10)[1, ]$data[[1]]

x %>%
  complete(date = seq(from = min(x$date), to = max(x$date), by = "day")) %>%
  ggplot(aes(date, mean)) +
  geom_point()
  # geom_line()



# qaqc series -------------------------------------------------------------

df_qaqc <- df_gap %>%
  mutate(
    rejected = FALSE,
    reason = ""
  )

assign_flag <- function (df, idx, reason) {
  cat("Rejected", length(idx), "series for:", reason, "\n")
  df[["rejected"]][idx] <- TRUE
  df[["reason"]][idx] <- paste0(df[["reason"]][idx], paste0(reason, "; "))
  df
}

# n_day < 10
idx <- with(df_qaqc, which(n_day < 10))
df_qaqc <- assign_flag(df_qaqc, idx, "Duration less than 10 days")

# 1 < median frequency < 24
idx <- with(df_qaqc, which(freq_median > 1 & freq_median < 24))
df_qaqc <- assign_flag(df_qaqc, idx, "Median daily frequency > 1 and < 24")




df_qaqc %>%
  filter(freq_q10 != freq_q90) %>%
  View

