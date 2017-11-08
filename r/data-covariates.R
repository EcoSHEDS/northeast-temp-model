# fetch covariates from database
# <- {wd}/data/clean.rds
# -> {wd}/data/covariates.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting data-prepare:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))

config <- fromJSON("../config.json")

cat("loading data/clean.rds...")
df <- readRDS(file.path(config$wd, "data", "clean.rds"))
featureids <- unique(df$featureid)
cat("done\n")

cat("connecting to db ( host =", config$db$host, ", dbname =", config$db$dbname, ")...")
con <- dbConnect(PostgreSQL(), host = config$db$host, dbname = config$db$dbname, user = config$db$user)
cat("done\n")

# upstream covariates
cat("fetching upstream covariates...")
covariates <- c("agriculture", "alloffnet", "allonnet", "AreaSqKM", "devel_hi", "devel_low", "devel_med", "developed", "devel_opn", "drainageclass", "elevation", "forest", "fwsopenwater", "fwswetlands", "herbaceous", "hydrogroup_a", "hydrogroup_ab", "hydrogroup_cd", "hydrogroup_d1", "hydrogroup_d4", "impervious", "openoffnet", "openonnet", "percent_sandy", "slope_pcnt", "surfcoarse", "tree_canopy", "undev_forest", "water", "wetland")
db_covariates_upstream <- tbl(con, "covariates") %>%
  filter(
    featureid %in% featureids,
    variable %in% covariates,
    zone == "upstream",
    is.na(riparian_distance_ft)
  )
df_covariates_upstream <- db_covariates_upstream %>%
  collect()
cat("done ( nrow =", nrow(df_covariates_upstream), ")\n")
# ~15 min

# riparian
cat("fetching riparian covariates...")
db_covariarates_riparian <- tbl(con, "covariates") %>%
  filter(
    featureid %in% featureids,
    variable %in% c("forest"),
    zone == "upstream",
    riparian_distance_ft == 200
  )
df_covariarates_riparian <- db_covariarates_riparian %>%
  collect()
cat("done ( nrow =", nrow(df_covariarates_riparian), ")\n")

cat("disconnecting from db...")
diconnected <- dbDisconnect(con)
cat("done\n")

cat("merging upstream and riparian covariates...")
df_covariates <- df_covariates_upstream %>%
  select(-zone, -riparian_distance_ft) %>%
  spread(variable, value) %>%
  rename(forest_all = forest) %>%
  left_join(
    df_covariarates_riparian %>%
      select(-zone, -riparian_distance_ft) %>%
      spread(variable, value),
    by = "featureid"
  )
cat("done\n")

cat("saving to data/covariates.rds...")
df_covariates %>%
  saveRDS(file.path(config$wd, "data", "covariates.rds"))
cat("done\n")

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))
cat("finished data-process:", as.character(end, tz = "US/Eastern"), "( elapsed =", round(elapsed / 60, digits = 1), "min )\n")

