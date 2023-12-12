# Neversink Basin Dataset

library(tidyverse)
library(glue)
library(janitor)


# load data ---------------------------------------------------------------

# load only csv files containing "_WT_" in filename
files <- list.files("data/neversink/Raw_Data/", pattern = "_WT_", full.names = TRUE)
files_raw <- tibble(
  file = files,
  data = map(files, function (x) {
    skip <- 0
    y <- read_lines(x, n_max = 1)
    if (grepl("Plot Title", y)) {
      skip <- 1
    }
    read_csv(x, skip = skip, skip_empty_rows = TRUE) |>
      filter(!is.na(`#`))
  })
)


# clean data --------------------------------------------------------------

files_inst <- files_raw |>
  mutate(
    site = map_chr(file, ~ str_split(basename(.), "_")[[1]][2]),
    cols = map(data, names),
    ncol = map_dbl(cols, length),
    col_datetime = map_chr(cols, ~ .[grepl("Date", .)]),
    utc_offset = case_when(
      col_datetime == "Date Time, GMT-05:00" ~ -5,
      col_datetime == "Date-Time (EST)" ~ -5,
      col_datetime == "Date Time, GMT-04:00" ~ -4,
      col_datetime == "Date-Time (EDT)" ~ -4,
      TRUE ~ NA_real_
    ),
    col_temp = map_chr(cols, ~ .[grepl("^Temp,|^Temperature|^Avg: Temp|^Ch: 1 - Temperature", .)][1]),
    temp_units = case_when(
      str_detect(col_temp, "°C") ~ "C",
      str_detect(col_temp, "°F") ~ "F",
      TRUE ~ NA_character_
    )
  ) |>
  rowwise() |>
  mutate(
    data = list({
      data <- data |>
        select(all_of(c(col_datetime, col_temp))) |>
        rename(datetime = col_datetime, temp_c = col_temp) |>
        mutate(
          datetime_local = parse_date_time(datetime, orders = c("mdy HMS", "mdY HM")),
          datetime_utc = datetime_local - hours(utc_offset),
          datetime_est = datetime_utc - hours(5)
        ) |>
        filter(!is.na(temp_c))
      if (temp_units == "F") {
        data <- data |>
          mutate(temp_c = (temp_c - 32) * 5 / 9)
      }
      data
    })
  ) |>
  ungroup()

site_inst <- files_inst |>
  select(file, site, data) |>
  unnest(data)

summary(site_inst)

# hexbin plot
site_inst |>
  ggplot(aes(x = yday(datetime_est), y = temp_c)) +
  geom_hex(bins = 100)



# load sites --------------------------------------------------------------

sites <- read_csv("data/neversink/Raw_Data/neversink_final_Tlogger_locations.csv") |>
  clean_names() |>
  transmute(
    site,
    description = glue("{stream} ({site})"),
    latitude,
    longitude
  ) |>
  print()

# export ------------------------------------------------------------------

out <- list(
  sites = sites,
  data = site_inst |>
    select(site, datetime = datetime_est, temp_c)
)

out |>
  write_rds("data/neversink/data-neversink.rds")

stopifnot(
  all(!duplicated(out$sites$site)),
  all(!is.na(out$sites))
)

out$sites |>
  select(name = site, description, latitude, longitude) |>
  write_csv("data/neversink/locations.csv")

stopifnot(
  all(out$data$site %in% out$sites$site),
  all(!is.na(out$data))
)

out$data |>
  select(site, datetime, temp_c) |>
  mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S")) |>
  write_csv("data/neversink/data.csv")


list(
  dataset_type = "series",
  description = "Batch upload of USGS_NEVERSINK",
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
  jsonlite::write_json("data/neversink/config.json", auto_unbox = TRUE, pretty = TRUE)
