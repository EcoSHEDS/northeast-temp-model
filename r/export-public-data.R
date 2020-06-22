# export public data to csv
# <- {wd}/data-db.rds
# -> {wd}/public-data.csv

rm(list=ls())

start <- lubridate::now(tzone = "US/Eastern")
cat("starting export-public-data:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(tidyverse))

source("functions.R")

config <- load_config()

cat("loading data-db.rds...")
db <- readRDS(file.path(config$wd, "data-db.rds"))
db_agencies <- db$agencies
db_series <- db$series
db_locations <- db$locations
db_values <- db$values
cat("done\n")

cat("filtering out public data...")
df_locations <- db_locations %>%
  filter(public) %>%
  rename(featureid = catchment_id)
df_agencies <- db_agencies %>%
  filter(id %in% df_locations$agency_id, name != "TEST")
df_series <- db_series %>%
  filter(location_id %in% df_locations$id)
df_values <- db_values %>%
  filter(series_id %in% df_series$id)

df_series <- df_series %>%
  filter(id %in% unique(df_values$series_id))
df_locations <- df_locations %>%
  filter(id %in% unique(df_series$location_id), agency_id %in% df_agencies$id)
df_agencies <- df_agencies %>%
  filter(id %in% unique(df_locations$agency_id))
cat("done\n")

cat("appending flags...")
df_series_flags <- df_series %>%
  mutate(
    flags = coalesce(flags, "[]"),
    flags = map(flags, function(x) {
      if (x == "[]") {
        return(data_frame())
      }
      fromJSON(x)
    }),
    n_flags = map_int(flags, nrow)
  ) %>%
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

df_values <- df_values %>%
  mutate(date = as.Date(date, tz = "UTC")) %>%
  left_join(
    df_series_flags,
    by = c("series_id" = "id", "date")
  ) %>%
  mutate(
    flagged = coalesce(flagged, FALSE)
  )
cat("done\n")

# check ids
stopifnot(identical(sort(df_agencies$id), sort(unique(df_locations$agency_id))))
stopifnot(identical(sort(df_locations$id), sort(unique(df_series$location_id))))
stopifnot(identical(sort(df_series$id), sort(unique(df_values$series_id))))


# export ------------------------------------------------------------------
cat("exporting to csv...")
dir.create(file.path(config$wd, "public-data"), showWarnings = FALSE)

df_agencies %>%
  select(
    agency_id = id,
    agency_name = name,
    agency_description = description
  ) %>%
  arrange(agency_id) %>%
  write_csv(file.path(config$wd, "public-data", "agencies.csv"), na = "")
df_locations %>%
  select(
    location_id = id,
    location_name = name,
    location_description = description,
    latitude,
    longitude,
    agency_id,
    featureid
  ) %>%
  arrange(location_id) %>%
  write_csv(file.path(config$wd, "public-data", "locations.csv"), na = "")
df_series %>%
  select(
    series_id = id,
    location_id,
    agency_id,
    reviewed
  ) %>%
  arrange(series_id) %>%
  write_csv(file.path(config$wd, "public-data", "series.csv"), na = "")
df_values %>%
  mutate_at(vars(min, mean, max), round, digits = 1) %>%
  select(
    series_id,
    date,
    min,
    mean,
    max,
    n,
    flagged,
    comment
  ) %>%
  arrange(series_id, date) %>%
  write_csv(file.path(config$wd, "public-data", "values.csv"), na = "")

cat("done\n")


# zip ---------------------------------------------------------------------


zipfile <- file.path(config$wd, "public-data", "public-data.zip")
if (file.exists(zipfile)) {
  invisible(file.remove(zipfile))
}
cat("generating zip file ", zipfile, "...\n", sep = "")
export_files <- list.files(file.path(config$wd, "public-data"), full.names = TRUE)

zip(zipfile = zipfile, files = export_files, flags = "-j")
cat("done\n")

# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))
cat("finished export-public-data: ", as.character(end, tz = "US/Eastern"), " (elapsed = ", elapsed, " sec)\n", sep = "")
