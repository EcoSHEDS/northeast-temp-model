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

# calibration -------------------------------------------------------------

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

B.huc8 <- as.matrix(out$results$mean$B.huc[out$data$huc, ])
X.huc8 <- out$data$X.site
Y.huc8 <- rowSums(X.huc8 * B.huc8)

Y <- Y.fixed + Y.catchment + Y.huc8 + Y.year

df_calib <- data_frame(
  date = out$df$date,
  huc8 = out$df$huc8,
  featureid = out$df$featureid,
  year = out$df$year,
  deploy_id = out$df$deploy_id,
  obs = out$df$temp,
  trend = Y
) %>%
  group_by(deploy_id) %>%
  mutate(
    deploy_row = row_number(),
    resid_trend = obs - trend,
    resid_lag = coalesce(lag(resid_trend), 0),
    pred = if_else(deploy_row == 1, trend, trend + out$results$mean$B.ar1 * resid_lag),
    resid = obs - pred
  ) %>%
  ungroup()

df_calib_deploy <- df_calib %>%
  group_by(featureid, year, deploy_id) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),
    rmse = map_dbl(data, function (x) {
      sqrt(mean(x$resid^2))
    }),
    rmse_trend = map_dbl(data, function (x) {
      sqrt(mean(x$resid_trend^2))
    })
  )

df_calib_catchment <- df_calib %>%
  group_by(featureid) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),
    rmse = map_dbl(data, function (x) {
      sqrt(mean(x$resid^2))
    }),
    rmse_trend = map_dbl(data, function (x) {
      sqrt(mean(x$resid_trend^2))
    })
  )

df_calib_huc8 <- df_calib %>%
  group_by(huc8) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),
    rmse = map_dbl(data, function (x) {
      sqrt(mean(x$resid^2))
    }),
    rmse_trend = map_dbl(data, function (x) {
      sqrt(mean(x$resid_trend^2))
    })
  )

df_calib_summary <- df_calib %>%
  summarise(
    n_obs = n(),
    n_catchment = length(unique(featureid)),
    n_huc8 = length(unique(huc8)),
    n_deploy = length(unique(deploy_id)),
    n_year = length(unique(year)),

    rmse = sqrt(mean(resid ^ 2)),
    mean_resid = mean(resid),
    median_resid = median(resid),
    mean_abs_resid = mean(abs(resid)),
    median_abs_resid = median(abs(resid)),
    min_resid = min(resid),
    max_resid = max(resid),
    q01_resid = quantile(resid, probs = 0.01),
    q10_resid = quantile(resid, probs = 0.1),
    q25_resid = quantile(resid, probs = 0.25),
    q75_resid = quantile(resid, probs = 0.75),
    q90_resid = quantile(resid, probs = 0.9),
    q99_resid = quantile(resid, probs = 0.99),

    rmse_trend = sqrt(mean(resid_trend ^ 2)),
    mean_resid_trend = mean(resid_trend),
    median_resid_trend = median(resid_trend),
    mean_abs_resid_trend = mean(abs(resid_trend)),
    median_abs_resid_trend = median(abs(resid_trend)),
    min_resid_trend = min(resid_trend),
    max_resid_trend = max(resid_trend),
    q01_resid_trend = quantile(resid_trend, probs = 0.01),
    q10_resid_trend = quantile(resid_trend, probs = 0.1),
    q25_resid_trend = quantile(resid_trend, probs = 0.25),
    q75_resid_trend = quantile(resid_trend, probs = 0.75),
    q90_resid_trend = quantile(resid_trend, probs = 0.9),
    q99_resid_trend = quantile(resid_trend, probs = 0.99)
  ) %>%
  as.list()

cat("done\n")

# validation --------------------------------------------------------------

cat("calculating validation statistics...")

df_valid <- readRDS(file.path(config$wd, "model-input.rds"))$test %>%
  select(-featureid_id, -huc8_id, -year_id) %>%
  left_join(out$ids$site, by = "featureid") %>%
  left_join(out$ids$year, by = "year") %>%
  left_join(out$ids$huc, by = "huc8") %>%
  rename(
    obs = temp
  )
# summary(df_valid)

X.fixed_valid <- df_valid %>%
  mutate(intercept = 1) %>%
  select(one_of(out$covs$fixed.ef))
B.fixed_valid <- out$results$mean$B.0
Y.fixed_valid <- (as.matrix(X.fixed_valid) %*% as.matrix(B.fixed_valid))[, 1]

X.catchment_valid <- df_valid %>%
  mutate(intercept.site = 1) %>%
  select(one_of(out$covs$site.ef))
B.catchment.mean <- colMeans(out$results$mean$B.site)
B.catchment_valid <- out$results$mean$B.site[df_valid$featureid_id, ]
for (i in seq_along(B.catchment.mean)) {
  B.catchment_valid[is.na(B.catchment_valid[, i]), i] <- B.catchment.mean[i]
}
Y.catchment_valid <- rowSums(X.catchment_valid * B.catchment_valid)

X.huc8_valid <- X.catchment_valid
B.huc8.mean <- colMeans(out$results$mean$B.huc)
B.huc8_valid <- out$results$mean$B.huc[df_valid$huc8_id, ]
for (i in seq_along(B.huc8.mean)) {
  B.huc8_valid[is.na(B.huc8_valid[, i]), i] <- B.huc8.mean[i]
}
Y.huc8_valid <- rowSums(X.huc8_valid * B.huc8_valid)

X.year_valid <- df_valid %>%
  mutate(intercept.year = 1) %>%
  select(one_of(out$covs$year.ef))
B.year.mean <- colMeans(out$results$mean$B.year)
B.year_valid <- as.matrix(out$results$mean$B.year[df_valid$year_id, ])
for (i in seq_along(B.year.mean)) {
  B.year_valid[is.na(B.year_valid[, i]), i] <- B.year.mean[i]
}
Y.year_valid <- rowSums(X.year_valid * B.year_valid)

Y_valid <- Y.fixed_valid + Y.huc8_valid + Y.catchment_valid + Y.year_valid

df_valid <- df_valid %>%
  mutate(
    trend = Y_valid
  ) %>%
  group_by(deploy_id) %>%
  mutate(
    deploy_row = row_number(),
    resid_trend = obs - trend,
    resid_lag = coalesce(lag(resid_trend), 0),
    pred = if_else(deploy_row == 1, trend, trend + out$results$mean$B.ar1 * resid_lag),
    resid = obs - pred
  ) %>%
  ungroup() %>%
  select(date, huc8, featureid, year, deploy_id, obs, trend, deploy_row, resid_trend, resid_lag, pred, resid)

df_valid_deploy <- df_valid %>%
  group_by(featureid, year, deploy_id) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),
    rmse = map_dbl(data, function (x) {
      sqrt(mean(x$resid^2))
    }),
    rmse_trend = map_dbl(data, function (x) {
      sqrt(mean(x$resid_trend^2))
    })
  )

df_valid_catchment <- df_valid %>%
  group_by(featureid) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),
    rmse = map_dbl(data, function (x) {
      sqrt(mean(x$resid^2))
    }),
    rmse_trend = map_dbl(data, function (x) {
      sqrt(mean(x$resid_trend^2))
    })
  )

df_valid_huc8 <- df_valid %>%
  group_by(huc8) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),
    rmse = map_dbl(data, function (x) {
      sqrt(mean(x$resid^2))
    }),
    rmse_trend = map_dbl(data, function (x) {
      sqrt(mean(x$resid_trend^2))
    })
  )

df_valid_summary <- df_valid %>%
  summarise(
    n_obs = n(),
    n_catchment = length(unique(featureid)),
    n_huc8 = length(unique(huc8)),
    n_deploy = length(unique(deploy_id)),
    n_year = length(unique(year)),

    rmse = sqrt(mean(resid ^ 2)),
    mean_resid = mean(resid),
    median_resid = median(resid),
    mean_abs_resid = mean(abs(resid)),
    median_abs_resid = median(abs(resid)),
    min_resid = min(resid),
    max_resid = max(resid),
    q01_resid = quantile(resid, probs = 0.01),
    q10_resid = quantile(resid, probs = 0.1),
    q25_resid = quantile(resid, probs = 0.25),
    q75_resid = quantile(resid, probs = 0.75),
    q90_resid = quantile(resid, probs = 0.9),
    q99_resid = quantile(resid, probs = 0.99),

    rmse_trend = sqrt(mean(resid_trend ^ 2)),
    mean_resid_trend = mean(resid_trend),
    median_resid_trend = median(resid_trend),
    mean_abs_resid_trend = mean(abs(resid_trend)),
    median_abs_resid_trend = median(abs(resid_trend)),
    min_resid_trend = min(resid_trend),
    max_resid_trend = max(resid_trend),
    q01_resid_trend = quantile(resid_trend, probs = 0.01),
    q10_resid_trend = quantile(resid_trend, probs = 0.1),
    q25_resid_trend = quantile(resid_trend, probs = 0.25),
    q75_resid_trend = quantile(resid_trend, probs = 0.75),
    q90_resid_trend = quantile(resid_trend, probs = 0.9),
    q99_resid_trend = quantile(resid_trend, probs = 0.99)
  ) %>%
  as.list()
cat("done\n")

# export ------------------------------------------------------------------

cat("saving to model-diagnostics.rds...")
list(
  calib = list(
    values = df_calib,
    deploy = df_calib_deploy,
    catchment = df_calib_catchment,
    huc8 = df_calib_huc8,
    summary = df_calib_summary
  ),
  valid = list(
    values = df_valid,
    deploy = df_valid_deploy,
    catchment = df_valid_catchment,
    huc8 = df_valid_huc8,
    summary = df_valid_summary
  )
) %>%
  saveRDS(file.path(config$wd, "model-diagnostics.rds"))
cat("done\n")

# end ---------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
elapsed <- as.numeric(difftime(end, start, tz = "US/Eastern", units = "sec"))
cat("finished model-diagnostics: ", as.character(end, tz = "US/Eastern"), " (elapsed = ", round(elapsed / 60, digits = 1), " min)\n", sep = "")
