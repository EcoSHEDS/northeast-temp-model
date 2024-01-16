# export predictions to csv
# <- model-predict-derived.rds
# -> csv/sheds-temp-model-v{VERSION}.csv

start <- lubridate::now(tzone = "US/Eastern")
cat("starting export-csv:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(bit64))

source("functions.R")

config <- load_config()

# load --------------------------------------------------------------------

cat("loading predictions...")
# df_huc <- readRDS(file.path(config$wd, "data-huc.rds")) %>%
#   select(featureid, huc8) %>%
#   mutate(huc2 = str_sub(huc8, 1, 2))
# df_year <- readRDS(file.path(config$wd, "model-predict-year.rds"))
df <- readRDS(file.path(config$wd, "model-predict-derived.rds"))
cat("done\n")



# export: db -------------------------------------------------------------

db <- readRDS(file.path(config$wd, "data-db.rds"))
huc <- read_rds(file.path(config$wd, "data-huc.rds"))
db_locations <- db$locations |>
  left_join(
    db$agencies |>
      select(agency_id = id, agency_name = name),
    by = "agency_id"
  ) |>
  left_join(
    huc |>
      select(catchment_id = featureid, huc12),
    by = "catchment_id"
  ) |>
  relocate(agency_name) |>
  relocate(huc12, .after = "catchment_id") |>
  select(-created_at, -updated_at, -agency_id) |>
  mutate(catchment_id = str_trim(format(catchment_id, scientific = FALSE)))
db_locations |>
  write_csv(file.path(config$wd, "db-locations.csv"))

db_values <- db$values |>
  left_join(
    db$series |>
      select(series_id = id, location_id),
    by = "series_id"
  ) |>
  arrange(location_id, date) |>
  relocate(location_id) |>
  print()
db_values |>
  write_csv(file.path(config$wd, "db-values.csv"))



# export: model inp -------------------------------------------------------

inp <- read_rds(file.path(config$wd, "model-input.rds"))

inp_values <- bind_rows(
  calid = inp$train |>
    select(featureid, date, temp),
  valid = inp$test |>
    select(featureid, date, temp),
  .id = "split"
) |>
  rename(catchment_id = featureid, temp_c = temp)
inp_values |>
  write_csv(file.path(config$wd, "model-input.csv"))

# export: year ------------------------------------------------------------
#
# dir.create(file.path(config$wd, "csv", "predict-year"), showWarnings = FALSE, recursive = TRUE)
#
# walk(c(0, 2, 4, 6), function (air) {
#   cat(glue("air: {air}"), "\n")
#
#   x_year_air <- df_year %>%
#     filter(adjust_air_temp == !!air) %>%
#     left_join(df_huc, by = "featureid")
#
#   walk(sort(unique(df_huc$huc2)), function (huc2) {
#     cat(glue("huc2: {huc2}"), "\n")
#
#     dir.create(file.path(config$wd, "csv", "predict-year", huc2), showWarnings = FALSE, recursive = TRUE)
#
#     x_year_air_huc2 <- x_year_air %>%
#       filter(huc2 == !!huc2)
#
#     if (air == 0) {
#       fname <- glue("predict-year-{huc2}-base.csv")
#     } else {
#       fname <- glue("predict-year-{huc2}-air{air}.csv")
#     }
#     cat(glue("file: {fname}"), "\n")
#
#     x_year_air_huc2 %>%
#       select(-adjust_air_temp, -huc2) %>%
#       select(featureid, huc8, year, everything()) %>%
#       mutate_at(vars(featureid, year), as.character) %>%
#       mutate_if(is.double, ~ sprintf("%.2f", .)) %>%
#       write_csv(file.path(config$wd, "csv", "predict-year", huc2, fname), na = "")
#   })
# })


# export: derived ---------------------------------------------------------

cat("dataset structure:\n")
str(df)
cat("\n")

cat("rounding values to 3 digits...")
df_out <- df %>%
  mutate_at(vars(-featureid), ~ sprintf("%.2f", .))
cat("done\n")


if (!dir.exists(file.path(config$wd, "csv"))) {
  cat("creating csv directory...")
  dir.create(file.path(config$wd, "csv"))
  cat("done\n")
}


fname <- paste0("sheds-temp-model-v", config$version, ".csv")
cat("saving to csv/", fname, "...", sep = "")
df_out |>
  mutate(featureid = str_trim(format(featureid, scientific = FALSE))) |>
  write_csv(file.path(config$wd, "csv", fname), na = "")
cat("done\n")

# done --------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
cat("finished export-csv:", as.character(end, tz = "US/Eastern"), "\n")
