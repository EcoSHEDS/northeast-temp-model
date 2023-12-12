# USGS Potomac Dataset (Chesapeake Bay Karst Sites)

library(tidyverse)
library(glue)
library(janitor)

# load: acwa ---------------------------------------------------------------

acwa_raw <- read_csv(
  "data/potomac/csv/ACWA_WT2021_SHEDS.csv",
  col_types = cols(
    .default = col_character(),
    Latitude_DD = col_double(),
    Longitude_DD = col_double(),
    DateTime = col_datetime(format = ""),
    Temp_C = col_double()
  ),
  na = "N/A"
) |>
  clean_names() |>
  select(
    site = site_code,
    latitude = latitude_dd,
    longitude = longitude_dd,
    datetime = date_time,
    temp_c
  )

acwa_sites <- acwa_raw |>
  distinct(site, latitude, longitude)
stopifnot(all(!duplicated(acwa_sites$site)))

acwa_inst <- acwa_raw |>
  select(site, datetime, temp_c)

acwa_day <- acwa_inst |>
  group_by(site, date = as_date(datetime)) |>
  summarize(temp_c = mean(temp_c, na.rm = TRUE), .groups = "drop")

acwa_day |>
  ggplot(aes(yday(date), temp_c)) +
  geom_point()


# load: ksfish ------------------------------------------------------------


ksfish_raw <- read_csv(
  "data/potomac/csv/KSfish_StreamTemperature_2021to2023_ecosheds.csv",
  col_types = cols(
    .default = col_character(),
    Latitude = col_double(),
    Longitude = col_double(),
    DateTime = col_datetime(format = ""),
    Temp_C = col_double()
  ),
  na = c("N/A", "NA")
) |>
  clean_names() |>
  select(
    site = site_code,
    latitude,
    longitude,
    datetime = date_time,
    temp_c
  )

ksfish_sites <- ksfish_raw |>
  distinct(site, latitude, longitude)
stopifnot(all(!duplicated(ksfish_sites$site)))

ksfish_inst <- ksfish_raw |>
  select(site, datetime, temp_c)

ksfish_day <- ksfish_inst |>
  group_by(site, date = as_date(datetime)) |>
  summarize(temp_c = mean(temp_c, na.rm = TRUE), .groups = "drop")

ksfish_day |>
  ggplot(aes(yday(date), temp_c)) +
  geom_point()


# merge -------------------------------------------------------------------

sites <- bind_rows(
  acwa_sites,
  ksfish_sites
)

data <- bind_rows(
  acwa_inst,
  ksfish_inst
)

out <- list(
  sites = sites,
  data = data
)

out |>
  write_rds("data/potomac/data-potomac.rds")

stopifnot(
  all(!duplicated(out$sites$site)),
  all(!is.na(out$sites))
)

out$sites |>
  transmute(name = site, description = NA_character_, latitude, longitude) |>
  write_csv("data/potomac/locations.csv", na = "")

stopifnot(
  all(out$data$site %in% out$sites$site),
  all(!is.na(select(out$data, site, datetime)))
)

out$data |>
  filter(!is.na(temp_c)) |>
  select(site, datetime, temp_c) |>
  mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S")) |>
  write_csv("data/potomac/data.csv", na = "")

list(
  dataset_type = "series",
  description = "Batch upload of USGS_POTOMAC",
  datetime = list(
    column = "datetime",
    format = "YYYY-MM-DD HH:mm:ss",
    timezone = "EST"
  ),
  locations = list(
    format = "long",
    column = "site"
  ),
  variables = list(
    format = "single"
  ),
  values = list(
    column = "temp_c",
    missing = "NA"
  )
) %>%
  jsonlite::write_json("data/potomac/config.json", auto_unbox = TRUE, pretty = TRUE)
