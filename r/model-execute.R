# execute model
# <- {wd}/model-input.rds
# -> {wd}/model-output.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting model-execute: ", as.character(start, tz = "US/Eastern"), "\n", sep = "")

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jagsUI))

source("functions.R")

config <- load_config()

# load data ---------------------------------------------------------------

cat("loading model-input.rds...")
inp <- readRDS(file.path(config$wd, "model-input.rds"))

df_inp <- inp$train

df <- df_inp %>%
  mutate(
    intercept = 1,
    intercept.site = 1,
    intercept.huc = 1,
    intercept.year = 1
  )
first_rows <- which(df$new_deploy == 1)
eval_rows <- which(df$new_deploy == 0)
cat("done\n")

cat("setting up model params...")
cov.list <- list(
  fixed.ef = c(
    "intercept",
    "prcp2",
    "AreaSqKM",
    "prcp2.da",
    "airTemp.prcp2.da",
    "forest",
    "airTemp.forest",
    "devel_hi",
    "airTemp.devel_hi",
    "prcp30",
    "prcp30.da",
    "airTemp.da",
    "airTemp.prcp2",
    "airTemp.prcp30",
    "airTemp.prcp30.da",
    "impoundArea",
    "airTemp.impoundArea",
    "agriculture",
    "airTemp.agriculture"
  ),
  site.ef = c(
    "intercept.site",
    "airTemp",
    "temp7p"
  ),
  huc.ef = c(
    "intercept.huc",
    "airTemp",
    "temp7p"
  ),
  year.ef = c(
    "intercept.year"
  )
)

# remove stream.mu and trend to save memory
params <- c(
  "sigma",
  "B.ar1",
  "B.0",
  "B.site",
  # "stream.mu",
  # "trend",
  "sigma.b.site",
  "B.huc",
  "rho.B.huc",
  "mu.huc",
  "sigma.b.huc",
  "tau.B.huc.raw",
  "B.year",
  "mu.year",
  "sigma.b.year"
)


# FIXED EFFECTS
X.0 <- df %>%
  select(one_of(cov.list$fixed.ef))
var.0 <- names(X.0)

# RANDOM SITE EFFECTS
X.site <- df %>%
  select(one_of(cov.list$site.ef))
var.site <- names(X.site)
id.site <- df$featureid_id

# RANDOM HUC EFFECTS
X.huc <- df %>%
  select(one_of(cov.list$huc.ef))
var.huc <- names(X.huc)
id.huc <- df$huc12_id

# RANDOM YEAR EFFECTS
X.year <- df %>%
  select(one_of(cov.list$year.ef))
var.year <- names(X.year)
id.year <- df$year_id

data.list <- list(
  temp = df$temp,                    # temp observations
  n = nrow(df),                      # no. of observations

  X.0 = as.matrix(X.0),              # fixed effects data
  K.0 = length(var.0),               # no. of fixed effect variables

  site = id.site,                    # site group indices
  X.site = as.matrix(X.site),        # site effects data
  K.site = length(var.site),         # no. of site effect variables
  N.site = max(id.site),             # no. of sites
  W.site = diag(length(var.site)),   # cov matrix for site effects

  huc = id.huc,                      # huc group indices
  X.huc = as.matrix(X.huc),          # huc effects data
  K.huc = length(var.huc),           # no. of huc effect variables
  N.huc = max(id.huc),               # no. of hucs
  W.huc = diag(length(var.huc)),     # cov matrix for huc effects

  year = id.year,                    # year group indices
  X.year = as.matrix(X.year),        # year effects data
  K.year = length(var.year),         # no. of year effect variables
  N.year = max(id.year),             # no. of years
  W.year = diag(length(var.year)),   # cov matrix for year effects

  evalRows = eval_rows,              # evaluation row indices
  nEvalRows = length(eval_rows),     # no. of evaluation rows
  firstObsRows = first_rows,         # first row indices
  nFirstObsRows = length(first_rows) # no. of first row indices
)

# cat(n, J, K, Ti, L, M, K.0, "\n")

# group ids
ids <- list(
  site = df %>%
    select(featureid, featureid_id) %>%
    distinct() %>%
    arrange(featureid_id),
  huc = df %>%
    select(huc12, huc12_id) %>%
    distinct() %>%
    arrange(huc12_id),
  year = df %>%
    select(year, year_id) %>%
    distinct() %>%
    arrange(year_id)
)

cat("done\n")

# copy jags file ----------------------------------------------------------

cat("copying temp-model.jags...")
copied <- file.copy("jags/temp-model.jags", file.path(config$wd, "temp-model.jags"), overwrite = TRUE)
if (!copied) {
  stop("ERROR: Failed to copy JAGS model definition file")
}
cat("done\n")

# run model ---------------------------------------------------------------

cat("running model...")
jm <- jags(
  data = data.list,
  # inits = inits.list,
  parameters.to.save = params,
  model.file = file.path(config$wd, "temp-model.jags"),
  n.chains = 3,
  n.iter = 6000,
  n.burnin = 3000,
  n.thin = 3,
  parallel = TRUE
)
cat("done\n")

# saveRDS(jm, file.path(config$wd, "model-output.rds"))

# https://www.rdocumentation.org/packages/jagsUI/versions/1.4.4/topics/jags.basic


# export ------------------------------------------------------------------

cat("saving to model-output.rds...")
list(
  df = df,
  ids = ids,
  data = data.list,
  params = params,
  covs = cov.list,
  results = jm
) %>%
  saveRDS(file.path(config$wd, "model-output.rds"))
cat("done\n")


# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))
cat("finished model-execute: ", as.character(end, tz = "US/Eastern"), " (elapsed = ", round(elapsed / 60, digits = 1), " min)\n", sep = "")
