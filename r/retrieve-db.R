library(RPostgreSQL)
library(tidyverse)
library(jsonlite)

config <- fromJSON("../config.json")


locations_tidal <- read_csv(
  file.path(config$wd, "locations-tidal.csv"),
  col_types = cols(
    location_id = col_integer()
  )
)

locations_impoundment <- read_csv(
  file.path(config$wd, "locations-impoundment.csv"),
  col_types = cols(
    location_id = col_integer()
  )
)

locations_exclude <- bind_rows(locations_tidal, locations_impoundment) %>%
  distinct()

cat("Excluding", nrow(locations_exclude), "locations\n")

con <- dbConnect(PostgreSQL(), host = "ecosheds.org", dbname = "sheds", user = "jeff")

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
    location_id %in% db_locations$id,
    reviewed == TRUE,
    value_count > 10
  )
df_series <- collect(db_series)

cat("Series Count:", nrow(db_series), "\n")

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
    series_id %in% db_series$id
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
db_values %>% show_query()
system.time(df_values <- collect(db_values))

# df_values %>%
#   ggplot(aes(date, mean)) +
#   geom_line() +
#   facet_wrap(~series_id)

df_values_nested <- df_values %>%
  group_by(series_id) %>%
  nest(.key = "data")

df_series_values <- df_series %>%
  left_join(df_values_nested, by = c("id" = "series_id")) %>%
  mutate(
    n_day = map_int(data, nrow),
    freq_min = map_dbl(data, ~ min(.$n)),
    freq_q10 = map_dbl(data, ~ quantile(.$n, probs = 0.1)),
    freq_mean = map_dbl(data, ~ mean(.$n)),
    freq_median = map_dbl(data, ~ median(.$n)),
    freq_q90 = map_dbl(data, ~ quantile(.$n, probs = 0.9)),
    freq_max = map_dbl(data, ~ max(.$n))
  )

df_series_values %>%
  ggplot(aes(n_day)) +
  geom_histogram()
df_series_values$n_day %>% table

df_series_values %>%
  mutate(freq_diff = freq_max - freq_min) %>%
  filter(freq_diff > 0) %>%
  arrange(freq_diff) %>%
  select(-data) %>%
  View


list(
  series = df_series,
  locations = df_locations,
  series_values = df_series_values
) %>%
  saveRDS(file.path(config$wd, "data.rds"))
