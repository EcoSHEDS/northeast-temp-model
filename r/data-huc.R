# retrieve featureid-huc table
# -> {wd}/huc8.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting data-huc:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(stringr))

cat("connecting to db ( host =", config$db$host, ", dbname =", config$db$dbname, ")...")
con <- dbConnect(PostgreSQL(), host = config$db$host, dbname = config$db$dbname, user = config$db$user, password = config$db$password)
cat("done\n")

cat("fetching featureid-huc12...")
df_huc <- tbl(con, "catchment_huc12") %>%
  collect()
df_huc <- df_huc %>%
  mutate(
    huc2 = str_sub(huc12, 1, 2),
    huc4 = str_sub(huc12, 1, 4),
    huc8 = str_sub(huc12, 1, 8),
    huc10 = str_sub(huc12, 1, 10)
  )
cat("done\n")

cat("disconnecting from db...")
disconnected <- dbDisconnect(con)
cat("done\n")

cat("exporting huc8.rds...")
df_huc %>%
  select(featureid, huc8) %>%
  saveRDS(file.path(config$wd, "huc8.rds"))
cat("done\n")
