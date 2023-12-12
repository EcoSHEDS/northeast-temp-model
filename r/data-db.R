# retrieve raw data from database
# <- {wd}/locations-exclude.txt
# -> {wd}/data-db.rds
# -> {wd}/daymet-featureid_year.csv

start <- lubridate::now(tzone = "US/Eastern")
cat("starting data-db: ", as.character(start, tz = "US/Eastern"), "\n", sep = "")

suppressPackageStartupMessages(library(RPostgres))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))

source("functions.R")

config <- load_config()

EXCLUDE_AGENCY_IDS <- c("TEST")

# load data ---------------------------------------------------------------

cat("reading locations-exclude.txt...")
locations_exclude <- read_table(
  file.path(config$wd, "locations-exclude.txt"),
  col_names = "location_id",
  col_types = cols(
    location_id = col_integer()
  )
)
cat("done (n = ", length(locations_exclude$location_id), ")\n", sep = "")

cat("connecting to db (host = ", config$db$host, ", dbname = ", config$db$dbname, ")...", sep = "")
con <- dbConnect(Postgres(), host = config$db$host, dbname = config$db$dbname, user = config$db$user)
cat("done\n")

cat("retrieving agencies...")
db_agencies <- tbl(con, "agencies")
df_agencies <- collect(db_agencies)

agencies_exclude <- df_agencies %>%
  filter(name %in% EXCLUDE_AGENCY_IDS) %>%
  select(agency_id = id)
cat("done (nrow = ", nrow(df_agencies), ", excluding = ", nrow(agencies_exclude), ")\n", sep = "")

cat("retrieving locations...")
db_locations <- tbl(con, "locations") %>%
  filter(
    !id %in% !!locations_exclude$location_id,
    !agency_id %in% !!agencies_exclude$agency_id
  )
df_locations <- collect(db_locations)
cat("done (nrow = ", nrow(df_locations), ")\n", sep = "")

# fill in missing catchment_id from trout
config_trout <- load_config("../config-trout.sh")
con_trout <- dbConnect(Postgres(), host = config_trout$db$host, dbname = config_trout$db$dbname, user = config_trout$db$user)
trout_locations <- tbl(con_trout, "locations_temp") %>%
  select(id, name, featureid) |>
  collect()
dbDisconnect(con_trout)

df_locations <- df_locations |>
  left_join(trout_locations, by = c("id", "name")) |>
  mutate(catchment_id = featureid) |>
  select(-featureid)

cat("retrieving series...")
suppressWarnings({
  db_series <- tbl(con, "series") %>%
    filter(
      location_id %in% !!df_locations$id,
      reviewed == TRUE,
      value_count > 10
    )
  df_series <- collect(db_series)
})
cat("done (nrow = ", nrow(df_series), ")\n", sep = "")

cat("retrieving values...")
db_values <- tbl(con, "values") %>%
  filter(
    series_id %in% !!df_series$id
  ) %>%
  mutate(
    date = date(timezone("US/Eastern", datetime))
  ) %>%
  group_by(series_id, date) %>%
  summarize(
    min = min(value),
    mean = mean(value),
    max = max(value),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(series_id, date) %>%
  ungroup()
# db_values %>% show_query()

# system.time(df_values <- collect(db_values))
#   user  system elapsed
# 14.765   4.654 538.003 (9 min)
df_values <- collect(db_values)
cat("done (nrow = ", nrow(df_values), ")\n", sep = "")

cat("disconnecting from db...")
disconnected <- dbDisconnect(con)
cat("done\n")

# export ------------------------------------------------------------------

cat("saving db dataset to data-db.rds...")
list(
  agencies = df_agencies,
  locations = df_locations,
  series = df_series,
  values = df_values
) %>%
  saveRDS(file.path(config$wd, "data-db.rds"))
cat("done\n")

cat("writing featureid/year list to daymet_featureid_year.csv...")
df_daymet <- df_series %>%
  select(location_id, start_datetime, end_datetime) %>%
  mutate(
    years = map2(start_datetime, end_datetime, function (x, y) {
      years <- seq(year(x), year(y), by = 1)
      tibble(year = years)
    })
  ) %>%
  select(location_id, years) %>%
  unnest(years) %>%
  left_join(
    df_locations %>%
      select(id, catchment_id),
    by = c("location_id" = "id")
  ) %>%
  select(featureid = catchment_id, year) %>%
  distinct() %>%
  mutate(year = as.character(year)) %>%
  filter(!is.na(featureid))
df_daymet %>%
  write_csv(file.path(config$wd, "daymet-featureid_year.csv"))
cat("done\n")


# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))
cat("finished data-db: ", as.character(end, tz = "US/Eastern"), " (elapsed = ", round(elapsed / 60, digits = 1), " min)\n", sep = "")
