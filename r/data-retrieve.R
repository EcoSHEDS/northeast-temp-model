# retrieve raw data from database
# -> {wd}/data-db.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting data-retrieve:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))

config <- fromJSON("../config.json")

cat("reading locations-exclude.txt...")
locations_exclude <- read_table(
  file.path(config$wd, "locations-exclude.txt"),
  col_names = "location_id",
  col_types = cols(
    location_id = col_integer()
  )
)
cat("done ( n =", length(locations_exclude$location_id), ")\n")

cat("connecting to db ( host =", config$db$host, ", dbname =", config$db$dbname, ")...")
con <- dbConnect(PostgreSQL(), host = config$db$host, dbname = config$db$dbname, user = config$db$user)
cat("done\n")

cat("retrieving agencies...")
db_agencies <- tbl(con, "agencies")
df_agencies <- collect(db_agencies)

agencies_exclude <- df_agencies %>%
  filter(name %in% config$dataset$agencies$exclude) %>%
  select(agency_id = id)
cat("done ( nrow =", nrow(df_agencies), ", excluding =", nrow(agencies_exclude), ")\n")


cat("retrieving locations...")
db_locations <- tbl(con, "locations") %>%
  filter(
    !id %in% locations_exclude$location_id,
    !agency_id %in% agencies_exclude$agency_id
  )
df_locations <- collect(db_locations)
cat("done ( nrow =", nrow(df_locations), ")\n")

cat("retrieving series...")
suppressWarnings({
  db_series <- tbl(con, "series") %>%
    select(-flags) %>%
    filter(
      location_id %in% df_locations$id,
      reviewed == TRUE,
      value_count > 10
    )
  df_series <- collect(db_series)
})
cat("done ( nrow =", nrow(df_series), ")\n")

cat("Retrieving values...")
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

# system.time(df_values <- collect(db_values))
#   user  system elapsed
# 14.765   4.654 538.003 (9 min)
df_values <- collect(db_values)
cat("done ( nrow =", nrow(df_values), ")\n")

out_file <- file.path(config$wd, "data", "db.rds")
cat("saving db dataset to", out_file, "...")
list(
  agencies = df_agencies,
  locations = df_locations,
  series = df_series,
  values = df_values
) %>%
  saveRDS(out_file)
cat("done\n")

cat("disconnecting from db...")
disconnected <- dbDisconnect(con)
cat("done\n")

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))

cat("finished data-retrieve:", as.character(end, tz = "US/Eastern"), "( elapsed =", round(elapsed / 60, digits = 1),"min )\n")
