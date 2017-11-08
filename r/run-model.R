# process raw data (clean, qaqc)
# <- {wd}/model-input.rds
# -> {wd}/jags.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting run-model:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(stringr))

config <- fromJSON("../config.json")

inp <- readRDS(file.path(config$wd, "model-input.rds"))

df <- inp$train %>%
  mutate(
    intercept = 1,
    intercept.site = 1,
    intercept.year = 1
  )
first_rows <- which(df$new_deploy == 1)
eval_rows <- which(df$new_deploy == 0)

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
    "intercept.site",
    "airTemp",
    "temp7p"
  ),
  year.ef = c(
    "intercept.year"
  )
)
params <- c(
  "sigma",
  "B.ar1",
  "B.0",
  "B.site",
  "stream.mu",
  "trend",
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
variables.fixed <- names(X.0)
K.0 <- length(variables.fixed)

# RANDOM SITE EFFECTS
X.site <- df %>%
  select(one_of(cov.list$site.ef))
variables.site <- names(X.site)
sites <- df$site_i
J <- length(unique(sites))
K <- length(variables.site)
n <- nrow(df)
W.site <- diag(K)

# RANDOM HUC EFFECTS
hucs <- df$huc8_i
M <- length(unique(hucs))
W.huc <- diag(K)

# RANDOM YEAR EFFECTS
X.year <- df %>%
  select(one_of(cov.list$year.ef))
variables.year <- names(X.year)
years <- df$year_i
Ti <- length(unique(years))
L <- length(variables.year)
W.year <- diag(L)

data.list <- list(
  n = n,
  J = J,
  K = K,
  Ti = Ti,
  L = L,
  M = M,
  K.0 = K.0,
  X.0 = as.matrix(X.0),
  W.site = W.site,
  W.year = W.year,
  W.huc = W.huc,
  temp = df$temp,
  evalRows = eval_rows,
  nEvalRows = length(eval_rows),
  firstObsRows = first_rows,
  nFirstObsRows = length(first_rows),
  X.site = as.matrix(X.site),
  X.year = as.matrix(X.year),
  site = sites,
  huc = hucs,
  year = years
)
#
# tau.huc <- as.matrix(
#   cbind(
#     c(runif(1,3.5,4.5), runif(1,-2.5, -2), runif(1,-3,-2.5)),
#     c(0, runif(1,10, 20), runif(1,-0.5,-0.1)),
#     c(0, 0, runif(1,3,10))
#   )
# )

inits <- list(sigma = runif(1))

# jm <- jags.model(paste0(data_dir, "/modelRegionalTempAR1.txt"), data.list, inits, n.adapt = n.burn, n.chains = 3)
# fm <- coda.samples(jm, params, n.iter = n.it, thin = n.thin)

