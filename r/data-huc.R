# fetch featureid-huc table
# -> {wd}/data-huc.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting data-huc: ", as.character(start, tz = "US/Eastern"), "\n", sep = "")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(stringr))

source("functions.R")

config <- load_config()

# load data ---------------------------------------------------------------

cat("connecting to db (host = ", config$db$host, ", dbname = ", config$db$dbname, ")...", sep = "")
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


# export ------------------------------------------------------------------

cat("exporting data-huc.rds...")
df_huc %>%
  select(featureid, huc2, huc4, huc8, huc10, huc12) %>%
  saveRDS(file.path(config$wd, "data-huc.rds"))
cat("done\n")


# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))
cat("finished data-huc: ", as.character(end, tz = "US/Eastern"), " (elapsed = ", round(elapsed / 60, digits = 1), " min)\n", sep = "")
