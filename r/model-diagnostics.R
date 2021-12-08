# generate model diagnostics
# <- {wd}/model-output.rds
# -> {wd}/model-diagnostics.rds

rm(list=ls())

start <- lubridate::now(tzone = "US/Eastern")
cat("starting model-diagnostics:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(jagsUI))
suppressPackageStartupMessages(library(coda))
suppressPackageStartupMessages(library(ggmcmc))

theme_set(theme_bw())

source("functions.R")

config <- load_config()

# load data ---------------------------------------------------------------

cat("loading model-output.rds...")

out <- readRDS(file.path(config$wd, "model-output.rds"))

cat("done\n")


# calib dataset -----------------------------------------------------------

cat("calculating calibration statistics...")

B.fixed <- out$results$mean$B.0
X.fixed <- out$data$X.0
Y.fixed <- (X.fixed %*% as.matrix(B.fixed))[, 1]

B.catchment <- out$results$mean$B.site[out$data$site, ]
X.catchment <- out$data$X.site
Y.catchment <- rowSums(X.catchment * B.catchment)

B.year <- as.matrix(out$results$mean$B.year[out$data$year, ])
X.year <- out$data$X.year
Y.year <- rowSums(X.year * B.year)

B.huc12 <- as.matrix(out$results$mean$B.huc[out$data$huc, ])
X.huc12 <- out$data$X.site
Y.huc12 <- rowSums(X.huc12 * B.huc12)

Y <- Y.fixed + Y.catchment + Y.huc12 + Y.year

df_calib <- tibble(
  dataset = "calib",
  date = out$df$date,
  huc12 = out$df$huc12,
  featureid = out$df$featureid,
  year = out$df$year,
  deploy_id = out$df$deploy_id,
  obs = out$df$temp,
  pred = Y
) %>%
  mutate(
    resid = obs - pred
  )

cat("done\n")


# valid dataset -----------------------------------------------------------

cat("calculating validation statistics...")

df_input_valid <- readRDS(file.path(config$wd, "model-input.rds"))$test %>%
  select(-featureid_id, -huc12_id, -year_id) %>%
  left_join(out$ids$site, by = "featureid") %>%
  left_join(out$ids$year, by = "year") %>%
  left_join(out$ids$huc, by = "huc12") %>%
  rename(
    obs = temp
  )
# summary(df_valid)

X.fixed_valid <- df_input_valid %>%
  mutate(intercept = 1) %>%
  select(one_of(out$covs$fixed.ef))
B.fixed_valid <- out$results$mean$B.0
Y.fixed_valid <- (as.matrix(X.fixed_valid) %*% as.matrix(B.fixed_valid))[, 1]

X.catchment_valid <- df_input_valid %>%
  mutate(intercept.site = 1) %>%
  select(one_of(out$covs$site.ef))
B.catchment.mean <- colMeans(out$results$mean$B.site)
B.catchment_valid <- out$results$mean$B.site[df_input_valid$featureid_id, ]
for (i in seq_along(B.catchment.mean)) {
  B.catchment_valid[is.na(B.catchment_valid[, i]), i] <- B.catchment.mean[i]
}
Y.catchment_valid <- rowSums(X.catchment_valid * B.catchment_valid)

X.huc12_valid <- X.catchment_valid
B.huc12.mean <- colMeans(out$results$mean$B.huc)
B.huc12_valid <- out$results$mean$B.huc[df_input_valid$huc12_id, ]
for (i in seq_along(B.huc12.mean)) {
  B.huc12_valid[is.na(B.huc12_valid[, i]), i] <- B.huc12.mean[i]
}
Y.huc12_valid <- rowSums(X.huc12_valid * B.huc12_valid)

X.year_valid <- df_input_valid %>%
  mutate(intercept.year = 1) %>%
  select(one_of(out$covs$year.ef))
B.year.mean <- colMeans(out$results$mean$B.year)
B.year_valid <- as.matrix(out$results$mean$B.year[df_input_valid$year_id, ])
for (i in seq_along(B.year.mean)) {
  B.year_valid[is.na(B.year_valid[, i]), i] <- B.year.mean[i]
}
Y.year_valid <- rowSums(X.year_valid * B.year_valid)

Y_valid <- Y.fixed_valid + Y.huc12_valid + Y.catchment_valid + Y.year_valid

df_valid <- df_input_valid %>%
  mutate(dataset = "valid") %>%
  select(dataset, date, huc12, featureid, year, deploy_id, obs) %>%
  mutate(
    pred = Y_valid,
    resid = obs - pred
  )
cat("done\n")

# merge and calculate group stats -----------------------------------------

cat("merging calibration and validation datasets...")
df <- bind_rows(df_calib, df_valid) %>%
  group_by(dataset, deploy_id) %>%
  mutate(
    resid_lag = coalesce(lag(resid), 0),
    pred_ar1 = pred + out$results$mean$B.ar1 * resid_lag,
    resid_ar1 = obs - pred_ar1
  ) %>%
  ungroup()
cat("done\n")

cat("computing stats by deployment...")
df_deploy <- df %>%
  group_by(dataset, featureid, year, deploy_id) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),
    rmse = map_dbl(data, function (x) {
      sqrt(mean(x$resid^2))
    }),
    rmse_ar1 = map_dbl(data, function (x) {
      sqrt(mean(x$resid_ar1^2))
    })
  )
cat("done\n")

cat("computing stats by catchment...")
df_catchment <- df %>%
  group_by(dataset, featureid) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),
    rmse = map_dbl(data, function (x) {
      sqrt(mean(x$resid^2))
    }),
    rmse_ar1 = map_dbl(data, function (x) {
      sqrt(mean(x$resid_ar1^2))
    })
  )
cat("done\n")

cat("computing stats by huc12...")
df_huc12 <- df %>%
  group_by(dataset, huc12) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),
    rmse = map_dbl(data, function (x) {
      sqrt(mean(x$resid^2))
    }),
    rmse_ar1 = map_dbl(data, function (x) {
      sqrt(mean(x$resid_ar1^2))
    })
  )
cat("done\n")

resid_stats <- function (x) {
  list(
    rmse = sqrt(mean(x ^ 2)),
    mean_resid = mean(x),
    median_resid = median(x),
    mean_abs_resid = mean(abs(x)),
    median_abs_resid = median(abs(x)),
    min_resid = min(x),
    max_resid = max(x),
    q01_resid = quantile(x, probs = 0.01),
    q10_resid = quantile(x, probs = 0.1),
    q25_resid = quantile(x, probs = 0.25),
    q75_resid = quantile(x, probs = 0.75),
    q90_resid = quantile(x, probs = 0.9),
    q99_resid = quantile(x, probs = 0.99)
  )
}

cat("calculating stats...")
stats <- lapply(c("calib", "valid"), function (dataset_) {
  x <- df %>% filter(dataset == dataset_)

  x_n <- list(
    n_obs = nrow(x),
    n_catchment = length(unique(x$featureid)),
    n_huc12 = length(unique(x$huc12)),
    n_deploy = length(unique(x$deploy_id)),
    n_year = length(unique(x$year))
  )
  x_resid <- resid_stats(x$resid)
  x_resid_ar1 <- resid_stats(x$resid_ar1)
  names(x_resid_ar1) <- paste0(names(x_resid_ar1), "_ar1")
  c(x_n, x_resid, x_resid_ar1)
}) %>%
  setNames(nm = c("calib", "valid"))
cat("done\n")

# export ------------------------------------------------------------------

cat("saving to model-diagnostics.rds...")
list(
  calib = list(
    values = filter(df, dataset == "calib"),
    deploy = filter(df_deploy, dataset == "calib"),
    catchment = filter(df_catchment, dataset == "calib"),
    huc12 = filter(df_huc12, dataset == "calib"),
    summary = stats$calib
  ),
  valid = list(
    values = filter(df, dataset == "valid"),
    deploy = filter(df_deploy, dataset == "valid"),
    catchment = filter(df_catchment, dataset == "valid"),
    huc12 = filter(df_huc12, dataset == "valid"),
    summary = stats$valid
  )
) %>%
  saveRDS(file.path(config$wd, "model-diagnostics.rds"))
cat("done\n")

# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))
cat("finished model-diagnostics: ", as.character(end, tz = "US/Eastern"), " (elapsed = ", round(elapsed / 60, digits = 1), " min)\n", sep = "")
