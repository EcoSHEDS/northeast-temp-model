library(tidyverse)
library(janitor)
library(glue)
library(sf)
library(DBI)

source("functions.R")

config <- load_config()
gis_dir <- "~/data/gis/wbd"

# fetch huc12s ---------------------------------------------------------------

con <- dbConnect(RPostgres::Postgres(), host = config$db$host, dbname = config$db$dbname, user = config$db$user, password = config$db$password)
catchment_huc12 <- tbl(con, "catchment_huc12") %>%
  collect()

huc12s <- unique(catchment_huc12$huc12)
huc4s <- unique(str_sub(huc12s, 1, 4))
huc2s <- unique(str_sub(huc12s, 1, 2))

dbDisconnect(con)


# download wbd ---------------------------------------------------------
# https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Hydrography/WBD/National/GDB/

wbd_path <- file.path("~/data/gis/wbd/WBD_National_GDB/WBD_National_GDB.gdb")
wbdhu12 <- read_sf(wbd_path, layer = "WBDHU12") |>
  filter(huc12 %in% huc12s)
wbdhu4 <- read_sf(wbd_path, layer = "WBDHU4") |>
  filter(huc4 %in% huc4s)
wbdhu2 <- read_sf(wbd_path, layer = "WBDHU2") |>
  filter(huc2 %in% huc2s)


write_rds(wbdhu12, "data/gis/wbdhuc12.rds")
write_rds(wbdhu4, "data/gis/wbdhuc4.rds")
write_rds(wbdhu2, "data/gis/wbdhuc2.rds")
