# fetch covariates from database
# -> {wd}/data-covariates.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting data-covariates:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(RPostgres))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))

source("functions.R")

config <- load_config("../config-trout.sh")

# load data ---------------------------------------------------------------

cat("connecting to db (host = ", config$db$host, ", dbname = ", config$db$dbname, ")...", sep = "")
con <- dbConnect(Postgres(), host = config$db$host, dbname = config$db$dbname, user = config$db$user, password = config$db$password)
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
cat("done (nrow = ", nrow(df_covariates_upstream), ")\n", sep = "")

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
cat("done (nrow = ", nrow(df_covariates_riparian), ")\n", sep = "")

cat("disconnecting from db...")
diconnected <- dbDisconnect(con)
cat("done\n")

# merge -------------------------------------------------------------------

cat("merging upstream and riparian covariates...")
df_covariates <- bind_rows(df_covariates_upstream, df_covariates_riparian) %>%
  spread(variable, value)
cat("done\n")

# export ------------------------------------------------------------------

cat("saving to data-covariates.rds...")
df_covariates %>%
  saveRDS(file.path(config$wd, "data-covariates.rds"))
cat("done\n")


# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))
cat("finished data-covariates: ", as.character(end, tz = "US/Eastern"), " (elapsed = ", round(elapsed / 60, digits = 1), " min)\n", sep = "")
