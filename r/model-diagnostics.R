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

out <- readRDS(file.path(config$wd, "model-output.rds"))

# calibration -------------------------------------------------------------

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
  catchment = out$df$site,
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
  group_by(catchment, year, deploy_id) %>%
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
  group_by(catchment) %>%
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
    n_catchment = length(unique(catchment)),
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


# validation --------------------------------------------------------------

df_valid <- readRDS(file.path(config$wd, "model-input.rds"))$test %>%
  select(-site_id, -huc8_id, -year_id) %>%
  left_join(out$ids$site, by = "site") %>%
  left_join(out$ids$year, by = "year") %>%
  left_join(out$ids$huc8, by = "huc8") %>%
  rename(
    obs = temp,
    catchment = site,
    catchment_id = site_id,
    new_catchment = new_site
  )
summary(df_valid)

X.fixed_valid <- df_valid %>%
  mutate(intercept = 1) %>%
  select(one_of(out$covs$fixed.ef))
B.fixed_valid <- out$results$mean$B.0
Y.fixed_valid <- (as.matrix(X.fixed_valid) %*% as.matrix(B.fixed_valid))[, 1]

X.catchment_valid <- df_valid %>%
  mutate(intercept.site = 1) %>%
  select(one_of(out$covs$site.ef))
B.catchment.mean <- colMeans(out$results$mean$B.site)
B.catchment_valid <- out$results$mean$B.site[df_valid$catchment_id, ]
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
  ungroup()

df_valid_deploy <- df_valid %>%
  group_by(catchment, year, deploy_id) %>%
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
  group_by(catchment) %>%
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
    n_catchment = length(unique(catchment)),
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

# export ------------------------------------------------------------------

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

# results -----------------------------------------------------------------


# RMSE
sqrt(mean(df_calib$resid^2))
sqrt(mean(df_calib$resid_trend^2))
df_calib_site %>%
  arrange(desc(rmse))
df_calib_site %>%
  arrange(desc(rmse_trend))
df_calib_huc %>%
  arrange(desc(rmse))
df_calib_huc %>%
  arrange(desc(rmse_trend))


df_calib %>%
  ggplot(aes(resid)) +
  geom_histogram()

df_calib %>%
  ggplot(aes(resid)) +
  stat_ecdf()

df_calib %>%
  ggplot(aes(temp, pred)) +
  geom_abline() +
  geom_point(size = 1, alpha = 0.2) +
  labs(
    x = "Observed Temp (degC)",
    y = "Predicted Temp (degC)"
  ) +
  theme(aspect.ratio = 1)

df_calib %>%
  ggplot(aes(temp, Y)) +
  geom_abline() +
  geom_point(size = 1, alpha = 0.2) +
  labs(
    x = "Observed Temp (degC)",
    y = "Predicted Temp (degC) w/o Autoregressive Term"
  ) +
  theme(aspect.ratio = 1)

df_calib_site %>%
  arrange(desc(rmse)) %>%
  head(10) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = pred)) +
  geom_point(aes(y = temp), size = 1, color = "orangered") +
  facet_wrap(~ site, scales = "free")

df_calib_site %>%
  arrange(rmse) %>%
  head(10) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = pred)) +
  geom_point(aes(y = temp), size = 1, color = "orangered") +
  facet_wrap(~ site, scales = "free")

# w/o autoregressive term
df_calib_site %>%
  arrange(desc(rmse_trend)) %>%
  head(12) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = Y, group = deploy_id)) +
  geom_point(aes(y = temp), size = 1, color = "orangered", shape = 1) +
  facet_wrap(~ site, scales = "free") +
  labs(x = "Date", y = "Predicted Temperature (degC) w/o Autoregressive Term")

df_calib_site %>%
  filter(n > 100) %>%
  arrange(rmse_trend) %>%
  head(12) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = Y)) +
  geom_point(aes(y = temp), size = 1, color = "orangered", shape = 1) +
  facet_wrap(~ site, scales = "free")

table(df_calib$pred > df_calib$temp) / nrow(df_calib)
table(df_calib$Y > df_calib$temp) / nrow(df_calib)

df_calib %>%
  ungroup() %>%
  select(temp, Y, pred) %>%
  gather(var, value) %>%
  mutate(var = ordered(var, levels = c("temp", "Y", "pred"))) %>%
  ggplot(aes(value, color = var)) +
  geom_density() +
  scale_color_discrete(
    "",
    labels = c(
      "pred" = "Predict w/ Autoregressive",
      "temp" = "Observed",
      "Y" = "Predict w/o Autoregressive"
    )
  )

df_calib_huc %>%
  arrange(desc(rmse)) %>%
  head(1) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = pred)) +
  geom_point(aes(y = temp), color = "orangered", size = 1) +
  facet_wrap(~site, scales = "free")

df_calib_huc %>%
  arrange(desc(rmse_trend)) %>%
  head(2) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = Y)) +
  geom_point(aes(y = temp), color = "orangered", size = 1) +
  facet_wrap(~site, scales = "free")

# validation --------------------------------------------------------------

# RMSE
sqrt(mean(df_valid$resid^2))
sqrt(mean(df_valid$resid_trend^2))
df_valid_site %>%
  arrange(desc(rmse))
df_valid_site %>%
  arrange(desc(rmse_trend))

df_valid %>%
  select(resid, resid_trend) %>%
  gather(var, value) %>%
  ggplot(aes(value, fill = var)) +
  geom_histogram() +
  facet_wrap(~ var, labeller = labeller(
    var = c(
      "resid" = "w/ autoregressive",
      "resid_trend" = "w/o autoregressive"
    )
  ))

df_valid %>%
  select(resid, resid_trend) %>%
  gather(var, value) %>%
  ggplot(aes(value, color = var)) +
  stat_ecdf() +
  scale_color_discrete("", labels = c(
    "resid" = "w/ autoregressive",
    "resid_trend" = "w/o autoregressive"
  ))

df_valid %>%
  ggplot(aes(temp, pred)) +
  geom_abline() +
  geom_point(size = 1, alpha = 0.2) +
  labs(
    x = "Observed Temp (degC)",
    y = "Predicted Temp (degC)"
  ) +
  theme(aspect.ratio = 1)

df_valid %>%
  ggplot(aes(temp, Y)) +
  geom_abline() +
  geom_point(size = 1, alpha = 0.2) +
  labs(
    x = "Observed Temp (degC)",
    y = "Predicted Temp (degC) w/o Autoregressive Term"
  ) +
  theme(aspect.ratio = 1)


df_valid_site %>%
  arrange(desc(rmse)) %>%
  head(10) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = pred)) +
  geom_point(aes(y = temp), size = 1, color = "orangered") +
  facet_wrap(~ site, scales = "free")

df_valid_site %>%
  arrange(rmse) %>%
  head(10) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = pred)) +
  geom_point(aes(y = temp), size = 1, color = "orangered") +
  facet_wrap(~ site, scales = "free")

# w/o autoregressive term
df_valid_site %>%
  arrange(desc(rmse_trend)) %>%
  head(10) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = Y)) +
  geom_point(aes(y = temp), size = 1, color = "orangered") +
  facet_wrap(~ site, scales = "free")

df_valid_site %>%
  arrange(rmse_trend) %>%
  head(10) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = Y)) +
  geom_point(aes(y = temp), size = 1, color = "orangered") +
  facet_wrap(~ site, scales = "free")

table(df_valid$pred > df_valid$temp) / nrow(df_valid)
table(df_valid$Y > df_valid$temp) / nrow(df_valid)



# parameter labels --------------------------------------------------------

B.0_labels <- data_frame(
  Parameter = paste0("B.0[", 1:length(out$covs$fixed.ef), "]"),
  Label = paste0("B.0[", out$covs$fixed.ef, "]"),
  Group = "fixed"
)

B.huc_labels <- crossing(
  huc8_id = out$ids$huc8$huc8_id,
  cov_id = 1:length(out$covs$huc.ef)
) %>%
  left_join(out$ids$huc8, by = "huc8_id") %>%
  left_join(
    data_frame(
      cov_id = 1:length(out$covs$huc.ef),
      cov = out$covs$huc.ef
    ),
    by = "cov_id"
  ) %>%
  mutate(
    Parameter = paste0("B.huc[", huc8_id, ",", cov_id, "]"),
    Label = paste0("B.huc[", huc8, ",", cov, "]"),
    Group = "huc"
  )

B.site_labels <- crossing(
  site_id = out$ids$site$site_id,
  cov_id = 1:length(out$covs$site.ef)
) %>%
  left_join(out$ids$site, by = "site_id") %>%
  left_join(
    data_frame(
      cov_id = 1:length(out$covs$site.ef),
      cov = out$covs$site.ef
    ),
    by = "cov_id"
  ) %>%
  mutate(
    Parameter = paste0("B.site[", site_id, ",", cov_id, "]"),
    Label = paste0("B.site[", site, ",", cov, "]"),
    Group = "site"
  )

B.year_labels <- crossing(
  year_id = out$ids$year$year_id,
  cov_id = 1:length(out$covs$year.ef)
) %>%
  left_join(out$ids$year, by = "year_id") %>%
  left_join(
    data_frame(
      cov_id = 1:length(out$covs$year.ef),
      cov = out$covs$year.ef
    ),
    by = "cov_id"
  ) %>%
  mutate(
    Parameter = paste0("B.year[", year_id, ",", cov_id, "]"),
    Label = paste0("B.year[", year, ",", cov, "]"),
    Group = "year"
  )

par_labels <- bind_rows(
  B.0_labels,
  B.site_labels,
  B.huc_labels,
  B.year_labels
) %>%
  select(Parameter, Label, Group)

out_summary <- out$results$summary %>%
  as_data_frame() %>%
  mutate(
    param = rownames(out$results$summary)
  ) %>%
  left_join(par_labels, by = c("param" = "Parameter")) %>%
  rename(label = Label, group = Group) %>%
  select(group, label, param, everything())

out$results$summary %>%
  as_data_frame() %>%
  mutate(
    param = rownames(out$results$summary)
  ) %>%
  inner_join(B.huc_labels, by = c("param" = "Parameter")) %>%
  ggplot(aes(mean)) +
  geom_histogram() +
  facet_wrap(~cov, scales = "free")


out$results$summary %>%
  as_data_frame() %>%
  mutate(
    param = rownames(out$results$summary)
  ) %>%
  inner_join(B.site_labels, by = c("param" = "Parameter")) %>%
  ggplot(aes(mean)) +
  geom_histogram() +
  facet_wrap(~cov, scales = "free")

out$results$summary %>%
  as_data_frame() %>%
  mutate(
    param = rownames(out$results$summary)
  ) %>%
  inner_join(B.site_labels, by = c("param" = "Parameter")) %>%
  filter(cov == "intercept.site") %>%
  select(site, intercept_mean = mean) %>%
  left_join(df_calib_site, by = "site") %>%
  arrange(desc(intercept_mean)) %>%
  head(10) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = Y)) +
  geom_point(aes(y = temp), size = 1, color = "orangered") +
  facet_wrap(~ site, scales = "free")

df_summary <- out$results$summary %>%
  as_data_frame() %>%
  mutate(
    param = rownames(out$results$summary)
  ) %>%
  select(param, everything())

# convergence -------------------------------------------------------------

reject <- rejectionRate(out$results$samples)
reject[reject > 0] # same as 20160715

ggs_B <- ggs(out$results$samples, par_labels = par_labels, family = "^B")

ggs_B %>%
  filter(Group != "B.0[1]") %>%
  ggs_caterpillar(family = "B.0")

ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-B0.pdf"), family = "B.0", plot = c("ggs_caterpillar", "ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-mu-huc.pdf"), family = "mu.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-mu-year.pdf"), family = "B.year", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-sigma-site.pdf"), family = "sigma.b.site", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-sigma-huc.pdf"), family = "sigma.b.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-sigma-year.pdf"), family = "sigma.b.year", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-ar1-rho-huc.pdf"), family = "rho.B.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-ar1-B-ar1.pdf"), family = "B.ar1", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)


# coef --------------------------------------------------------------------

M.ar1 <- readRDS("~/models/20160715/")

mcmc_small <- mcmc.list()
for(i in 1:length(M.ar1)) {
  bar <- attr(M.ar1[[i]], which = "dimnames")[[2]] #[2000:2200]
  sna <- M.ar1[[i]][ , which(!grepl("stream.mu", bar) & !grepl("trend", bar))]
  mcmc_small[[i]] <- as.mcmc(sna)
}

reject <- rejectionRate(mcmc_small)
reject[reject > 0]

mcmc_tiny <- mcmc.list()
for(i in 1:length(M.ar1)) {
  bar <- attr(M.ar1[[i]], which = "dimnames")[[2]] #[2000:2200]
  sna <- M.ar1[[i]][ , which(grepl("B.0", bar) | grepl("sigma", bar))]
  mcmc_tiny[[i]] <- as.mcmc(sna)
}
gelman.diag(mcmc_tiny)

system.time(ggs.ar1 <- ggs(mcmc_small))

ggmcmc(ggs.ar1, file = "pdf/ggmcmc-B0.pdf", family = "B.0", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-B0.pdf"), family = "B.0", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-mu-huc.pdf"), family = "mu.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-mu-year.pdf"), family = "B.year", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-sigma-site.pdf"), family = "sigma.b.site", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-sigma-huc.pdf"), family = "sigma.b.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-sigma-year.pdf"), family = "sigma.b.year", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-ar1-rho-huc.pdf"), family = "rho.B.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-ar1-B-ar1.pdf"), family = "B.ar1", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)

# compare models ----------------------------------------------------------

m1 <- readRDS("~/Projects/sheds/model/20160715/ggs-20160715.rds")
m1 <- m1 %>%
  mutate(model = "20160715")
m2 <- ggs(out$results$samples)
m2 <- m2 %>%
  mutate(model = "20171108")

m <- bind_rows(m1, m2)

m_B0 <- m %>%
  filter(str_detect(Parameter, "^B.0"))

table(m_B0$model, m_B0$Parameter)

m_B0 <- m_B0 %>%
  left_join(B.0_labels, by = "Parameter")
m_B0 %>%
  ggplot(aes(value, color = model)) +
  geom_density() +
  facet_wrap(~ Label, scales = "free")

pdf(file.path(config$wd, "pdf", "model-comparison-fixed.pdf"), width = 11, height = 8.5)
m_B0 %>%
  ggplot(aes(value, color = model)) +
  geom_density() +
  facet_wrap(~ Label, scales = "free") +
  labs(
    title = "Density"
  )

m_B0 %>%
  ggplot(aes(Iteration, value, color = model, group = interaction(model, Chain))) +
  geom_line() +
  facet_wrap(~ Label, scales = "free") +
  labs(
    title = "Chains"
  )

dev.off()