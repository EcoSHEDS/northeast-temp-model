# USGS Potomoc Dataset (Chesapeake Bay Karst Sites)

library(tidyverse)
library(glue)
library(janitor)

# load: acwa ---------------------------------------------------------------

acwa_raw <- read_csv(
  "data/potomoc/csv/ACWA_WT2021_SHEDS.csv",
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
  "data/potomoc/csv/KSfish_StreamTemperature_2021to2023_ecosheds.csv",
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
  acwa_day,
  ksfish_day
)

out <- list(
  sites = sites,
  data = data
)

out |>
  write_rds("data/potomoc/data-potomoc.rds")
