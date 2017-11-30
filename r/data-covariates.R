# fetch covariates from database
# -> {wd}/covariates.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting data-prepare:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))

config <- fromJSON("../config.json")

cat("connecting to db ( host =", config$db$host, ", dbname =", config$db$dbname, ")...")
con <- dbConnect(PostgreSQL(), host = config$db$host, dbname = config$db$dbname, user = config$db$user)
cat("done\n")

# upstream covariates
cat("fetching upstream covariates...")
df_covariates_upstream <- tbl(con, "covariates") %>%
  filter(
    variable %in% c("AreaSqKM", "devel_hi", "agriculture", "allonnet"),
    zone == "upstream",
    is.na(riparian_distance_ft)
  ) %>%
  collect() %>%
  select(featureid, variable, value)
cat("done ( nrow =", nrow(df_covariates_upstream), ")\n")

# riparian
cat("fetching riparian covariates...")
df_covariates_riparian <- tbl(con, "covariates") %>%
  filter(
    variable %in% c("forest"),
    zone == "upstream",
    riparian_distance_ft == 200
  ) %>%
  collect() %>%
  select(featureid, variable, value)
cat("done ( nrow =", nrow(df_covariarates_riparian), ")\n")

cat("disconnecting from db...")
diconnected <- dbDisconnect(con)
cat("done\n")

cat("merging upstream and riparian covariates...")
df_covariates <- bind_rows(df_covariates_upstream, df_covariates_riparian) %>%
  spread(variable, value)
cat("done\n")

cat("saving to data/covariates.rds...")
df_covariates %>%
  saveRDS(file.path(config$wd, "covariates.rds"))
cat("done\n")

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))
cat("finished data-process:", as.character(end, tz = "US/Eastern"), "( elapsed =", round(elapsed / 60, digits = 1), "min )\n")

