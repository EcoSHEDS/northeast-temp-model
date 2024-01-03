# export clean obs dataset for matt fuller

library(tidyverse)
library(DBI)
library(RPostgres)
library(bit64)
library(sf)
source("functions.R")

config <- load_config()


# fetch agencies ----------------------------------------------------------

con <- dbConnect(Postgres(), host = config$db$host, dbname = config$db$dbname, user = config$db$user)

db_agencies <- tbl(con, "agencies") |>
  collect()

dbDisconnect(con)


# raw data ----------------------------------------------------------------

db <- readRDS(file.path(config$wd, "data-db.rds"))
db_series <- db$series
db_values <- db$values
raw <- db_values |>
  left_join(db_series, by = c("series_id" = "id")) |>
  select(station_id = location_id, series_id, date, min_temp_c = min, mean_temp_c = mean, max_temp_c = max, n_values = n)

raw |>
  ggplot(aes(yday(date), mean_temp_c)) +
  geom_hex(bins = 100)

# stations ----------------------------------------------------------------

stn <- db$locations |>
  left_join(
    db_agencies %>%
      select(agency_id = id, agency_name = name),
    by = "agency_id"
  ) |>
  select(agency_name, station_id = id, name, description, latitude, longitude)

stn |>
  st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |>
  ggplot() +
  geom_sf()

# cleaned data ------------------------------------------------------------

clean <- read_rds(file.path(config$wd, "data-clean.rds"))$data |>
  rename(station_id = location_id) |>
  select(station_id, date, mean_temp_c = mean)

clean |>
  ggplot(aes(yday(date), mean_temp_c)) +
  geom_hex(bins = 100)

# export ------------------------------------------------------------------

raw %>%
  write_csv("notes/20231220/data-raw.csv", na = "")
clean %>%
  write_csv("notes/20231220/data-clean.csv", na = "")
stn %>%
  write_csv("notes/20231220/stations.csv", na = "")
