# NWIS Stream Temp Dataset

library(tidyverse)
library(janitor)
library(glue)
library(sf)
library(mapview)
library(dataRetrieval)

states <- c("CT", "DC", "DE", "KY", "MA", "MD", "ME", "NC", "NH", "NJ", "NY", "OH", "PA", "RI", "TN", "VA", "VT", "WV")

# sites <- tibble(
#   state = states
# ) |>
#   mutate(
#     data = map(state, function (state) {
#       cat(glue("state: {state}"), "\n")
#       dataRetrieval::whatNWISsites(
#         stateCd = state,
#         parameterCd = "00010",
#         siteTypeCd = "ST",
#         hasDataTypeCd = "dv"
#       ) |>
#         as_tibble()
#     })
#   ) |>
#   unnest(data)
# write_rds(sites, "data/nwis/fetch-sites.rds")
sites <- read_rds("data/nwis/fetch-sites.rds")

sites_sf <- sites %>%
  st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4326, remove = FALSE)

mapview(sites_sf)

# fetch data --------------------------------------------------------------

# data_raw <- sites |>
#   select(site_no) |>
#   mutate(
#     data = map(site_no, function (site_no) {
#       Sys.sleep(0.5)
#       dataRetrieval::readNWISdv(
#         siteNumbers = site_no,
#         parameterCd = "00010",
#         startDate = "1990-01-01",
#         endDate = as.character(today())
#       ) |>
#         as_tibble() |>
#         renameNWISColumns()
#     }, .progress = TRUE)
#   )
# write_rds(data_raw, "data/nwis/fetch-data.rds")
data_raw <- read_rds("data/nwis/fetch-data.rds")

data_day <- data_raw |>
  select(-site_no) |>
  unnest(data) |>
  clean_names() |>
  select(site_no, date, temp_c = wtemp, flag = wtemp_cd) |>
  filter(!is.na(temp_c))

data_day |>
  ggplot(aes(yday(date), temp_c)) +
  geom_hex()

data_day <- data_day |>
  filter(
    temp_c < 40,
    temp_c > -10,
    !(yday(date) >= 270 & yday(date) <= 300 & temp_c > 30)
  )

data_day |>
  ggplot(aes(yday(date), temp_c)) +
  geom_hex(bins = 100)

data_day |>
  ggplot(aes(yday(date), temp_c, color = flag)) +
  geom_point(alpha = 0.25, size = 1) +
  theme_bw()


# export ------------------------------------------------------------------

out <- list(
  sites = sites |>
    filter(site_no %in% data_day$site_no),
  data = data_day
)

out |>
  write_rds("data/nwis/data-nwis.rds")


stopifnot(
  all(!duplicated(out$sites$site)),
  all(!is.na(out$sites))
)

out$sites |>
  transmute(name = site_no, description = station_nm, latitude = dec_lat_va, longitude = dec_long_va) |>
  write_csv("data/nwis/locations.csv", na = "")

stopifnot(
  all(out$data$site_no %in% out$sites$site_no),
  all(!is.na(select(out$data, site_no, date)))
)

out$data |>
  filter(!is.na(temp_c)) |>
  select(site = site_no, datetime = date, temp_c) |>
  mutate(
    datetime = force_tz(as.POSIXct(datetime, tz = "UTC"), tz = "US/Eastern"),
    datetime = with_tz(datetime, "EST"),
    datetime = format(datetime, "%Y-%m-%d %H:%M:%S")
  ) |>
  write_csv("data/nwis/data.csv", na = "")

list(
  dataset_type = "series",
  description = "Batch upload of USGS_NWIS",
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
  jsonlite::write_json("data/nwis/config.json", auto_unbox = TRUE, pretty = TRUE)
