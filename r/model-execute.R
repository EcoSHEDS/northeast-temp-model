# execute model
# <- {wd}/model-input.rds
# -> {wd}/model-output.rds

start <- lubridate::now(tzone = "US/Eastern")
cat("starting model-execute:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(jagsUI))

source("functions.R")

config <- load_config()

# load data ---------------------------------------------------------------

inp <- readRDS(file.path(config$wd, "model-input.rds"))

# HUC2 <- "02"
# df <- inp$train %>%
#   mutate(
#     intercept = 1,
#     intercept.site = 1,
#     intercept.year = 1
#   ) %>%
#   filter(str_sub(huc8, 1, 2) == HUC2) %>%
#   mutate(
#     site_i = as.numeric(as.factor(site)),
#     huc8_i = as.numeric(as.factor(huc8)),
#     year_i = as.numeric(as.factor(year))
#   ) %>%
#   arrange(huc8_i, site_i, date) %>%
#   mutate(
#     delta_date = as.numeric(difftime(date, lag(date), units = "day")),
#     new_series = delta_date != 1,
#     new_site = coalesce(site_i != lag(site_i), TRUE),
#     new_year = coalesce(year != lag(year), TRUE),
#     new_deploy = 1 * (new_series | new_site | new_year),
#     deploy_id = cumsum(new_deploy)
#   )

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
variables.fixed <- names(X.0)
K.0 <- length(variables.fixed)

# RANDOM SITE EFFECTS
X.site <- df %>%
  select(one_of(cov.list$site.ef))
variables.site <- names(X.site)
sites <- df$site_id
J <- length(unique(sites))
K <- length(variables.site)
n <- nrow(df)
W.site <- diag(K)

# RANDOM HUC EFFECTS
hucs <- df$huc8_id
M <- length(unique(hucs))
W.huc <- diag(K)

# RANDOM YEAR EFFECTS
X.year <- df %>%
  select(one_of(cov.list$year.ef))
variables.year <- names(X.year)
years <- df$year_id
Ti <- length(unique(years))
L <- length(variables.year)
W.year <- diag(L)
data.list <- list(
  n = n,     # no. of observations
  J = J,     # no. of sites
  K = K,     # no. of site effect variables
  Ti = Ti,   # no. of years
  L = L,     # no. of year effect variables
  M = M,     # no. of hucs
  K.0 = K.0, # no. of fixed effect variables
  X.0 = as.matrix(X.0),  # fixed effects data
  W.site = W.site,       # cov matrix for site effects
  W.year = W.year,       # cov matrix for year effects
  W.huc = W.huc,         # cov matrix for huc effects
  temp = df$temp,        # temp observations
  evalRows = eval_rows,               # evaluation row indices
  nEvalRows = length(eval_rows),      # no. of evaluation rows
  firstObsRows = first_rows,          # first row indices
  nFirstObsRows = length(first_rows), # no. of first row indices
  X.site = as.matrix(X.site),  # site effects data
  X.year = as.matrix(X.year),  # year effects data
  site = sites,  # site group indices
  huc = hucs,    # huc group indices
  year = years   # year group indices
)

cat(n, J, K, Ti, L, M, K.0, "\n")

# group ids
ids <- list(
  site = df %>%
    select(site, site_id) %>%
    distinct() %>%
    arrange(site_id),
  year = df %>%
    select(year, year_id) %>%
    distinct() %>%
    arrange(year_id),
  huc8 = df %>%
    select(huc8, huc8_id) %>%
    distinct() %>%
    arrange(huc8_id)
)


#
# tau.huc <- as.matrix(
#   cbind(
#     c(runif(1,3.5,4.5), runif(1,-2.5, -2), runif(1,-3,-2.5)),
#     c(0, runif(1,10, 20), runif(1,-0.5,-0.1)),
#     c(0, 0, runif(1,3,10))
#   )
# )
#
# inits.list <- list(
#   list(sigma = runif(1)),
#   list(sigma = runif(1)),
#   list(sigma = runif(1))
# )


# copy jags file ----------------------------------------------------------

copied <- file.copy("jags/temp-model.jags", file.path(config$wd, "temp-model.jags"), overwrite = TRUE)
if (!copied) {
  stop("Failed to copy JAGS model definition file")
}

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

out <- list(
  df = df,
  ids = ids,
  data = data.list,
  params = params,
  covs = cov.list,
  results = jm
)

cat("saving results to", file.path(config$wd, "model-output.rds"), "...")
out %>%
  saveRDS(file.path(config$wd, "model-output.rds"))
cat("done\n")

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))
cat("finished model-execute:", as.character(end, tz = "US/Eastern"), "( elapsed =", round(elapsed / 60, digits = 1), "min )\n")
