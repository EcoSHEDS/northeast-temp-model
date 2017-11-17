# generate model diagnostics
# <- {wd}/model-output.rds

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
suppressPackageStartupMessages(library(ggmcmc))

config <- fromJSON("../config.json")

out <- readRDS(file.path(config$wd, "model-output.rds"))

B.0 <- out$results$mean$B.0
X.0 <- out$data$X.0
Y.0 <- (X.0 %*% as.matrix(B.0))[, 1]

B.site <- out$results$mean$B.site[out$data$site, ]
X.site <- out$data$X.site
Y.site <- rowSums(X.site * B.site)

B.year <- as.matrix(out$results$mean$B.year[out$data$year, ])
X.year <- out$data$X.year
Y.year <- rowSums(X.year * B.year)

B.huc <- as.matrix(out$results$mean$B.huc[out$data$huc, ])
X.huc <- out$data$X.site
Y.huc <- rowSums(X.huc * B.huc)

Y <- Y.0 + Y.site + Y.year + Y.huc

df_pred <- data_frame(
  date = out$df$date,
  site = out$df$site,
  year = out$df$year,
  huc8 = out$df$huc8,
  deploy_id = out$df$deploy_id,
  temp = out$df$temp,
  Y = Y
) %>%
  group_by(deploy_id) %>%
  mutate(
    deploy_row = row_number(),
    resid_Y = temp - Y,
    resid_lag = coalesce(lag(resid_Y), 0),
    pred = if_else(deploy_row == 1, Y, Y + out$results$mean$B.ar1 * resid_lag),
    resid = temp - pred
  )

# RMSE
sqrt(mean(df_pred$resid^2))
sqrt(mean(df_pred$resid_Y^2))

df_pred %>%
  ggplot(aes(resid)) +
  geom_histogram()

df_pred %>%
  ggplot(aes(resid)) +
  stat_ecdf()

df_pred %>%
  ggplot(aes(temp, pred)) +
  geom_abline() +
  geom_point(size = 1, alpha = 0.2) +
  labs(
    x = "Observed Temp (degC)",
    y = "Predicted Temp (degC)"
  ) +
  theme(aspect.ratio = 1)

df_pred %>%
  ggplot(aes(temp, Y)) +
  geom_abline() +
  geom_point(size = 1, alpha = 0.2) +
  labs(
    x = "Observed Temp (degC)",
    y = "Predicted Temp (degC) w/o Autoregressive Term"
  ) +
  theme(aspect.ratio = 1)


df_pred_site <- df_pred %>%
  group_by(site) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),
    rmse = map_dbl(data, function (x) {
      sqrt(mean(x$resid^2))
    })
  )

df_pred_site %>%
  arrange(desc(rmse))

df_pred_site %>%
  arrange(desc(rmse)) %>%
  head(10) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = pred)) +
  geom_point(aes(y = temp), size = 1, color = "orangered") +
  facet_wrap(~ site, scales = "free")

df_pred_site %>%
  arrange(rmse) %>%
  head(10) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = pred)) +
  geom_point(aes(y = temp), size = 1, color = "orangered") +
  facet_wrap(~ site, scales = "free")

# w/o autoregressive term
df_pred_site %>%
  arrange(desc(rmse)) %>%
  head(10) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = Y)) +
  geom_point(aes(y = temp), size = 1, color = "orangered") +
  facet_wrap(~ site, scales = "free")

df_pred_site %>%
  arrange(rmse) %>%
  head(10) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = Y)) +
  geom_point(aes(y = temp), size = 1, color = "orangered") +
  facet_wrap(~ site, scales = "free")

table(df_pred$pred > df_pred$temp) / nrow(df_pred)
table(df_pred$Y > df_pred$temp) / nrow(df_pred)



df_pred %>%
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

df_pred_huc <- df_pred %>%
  group_by(huc8) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),
    rmse = map_dbl(data, function (x) {
      sqrt(mean(x$resid^2))
    }),
    rmse_Y = map_dbl(data, function (x) {
      sqrt(mean(x$resid_Y^2))
    })
  )

df_pred_huc %>%
  arrange(desc(rmse))

df_pred_huc %>%
  arrange(desc(rmse)) %>%
  head(1) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = pred)) +
  geom_point(aes(y = temp), color = "orangered", size = 1) +
  facet_wrap(~site, scales = "free")

df_pred_huc %>%
  arrange(desc(rmse_Y)) %>%
  head(6) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = Y)) +
  geom_point(aes(y = temp), color = "orangered", size = 1) +
  facet_wrap(~site, scales = "free")

# validation --------------------------------------------------------------

df_valid <- readRDS(file.path(config$wd, "model-input.rds"))$test %>%
  select(-site_i, -huc8_i, -year_i) %>%
  left_join(out$ids$site, by = "site") %>%
  left_join(out$ids$year, by = "year") %>%
  left_join(out$ids$huc8, by = "huc8")
summary(df_valid)

X.0.pred <- df_valid %>%
  mutate(intercept = 1) %>%
  select(one_of(out$covs$fixed.ef))
B.0.pred <- out$results$mean$B.0
Y.0.pred <- (as.matrix(X.0.pred) %*% as.matrix(B.0.pred))[, 1]

X.site.pred <- df_valid %>%
  mutate(intercept.site = 1) %>%
  select(one_of(out$covs$site.ef))
B.site.mean <- colMeans(out$results$mean$B.site)
B.site.pred <- out$results$mean$B.site[df_valid$site_id, ]
for (i in seq_along(B.site.mean)) {
  B.site.pred[is.na(B.site.pred[, i]), i] <- B.site.mean[i]
}
Y.site.pred <- rowSums(X.site.pred * B.site.pred)

X.huc.pred <- X.site.pred
B.huc.mean <- colMeans(out$results$mean$B.huc)
B.huc.pred <- out$results$mean$B.huc[df_valid$huc8_id, ]
for (i in seq_along(B.huc.mean)) {
  B.huc.pred[is.na(B.huc.pred[, i]), i] <- B.huc.mean[i]
}
Y.huc.pred <- rowSums(X.huc.pred * B.huc.pred)

X.year.pred <- df_valid %>%
  mutate(intercept.year = 1) %>%
  select(one_of(out$covs$year.ef))
B.year.mean <- colMeans(out$results$mean$B.year)
B.year.pred <- as.matrix(out$results$mean$B.year[df_valid$year_id, ])
for (i in seq_along(B.year.mean)) {
  B.year.pred[is.na(B.year.pred[, i]), i] <- B.year.mean[i]
}
Y.year.pred <- rowSums(X.year.pred * B.year.pred)

Y.pred <- Y.0.pred + Y.site.pred + Y.year.pred + Y.huc.pred

df_valid <- df_valid %>%
  mutate(
    Y = Y.pred
  ) %>%
  group_by(deploy_id) %>%
  mutate(
    deploy_row = row_number(),
    resid_Y = temp - Y,
    resid_lag = coalesce(lag(resid_Y), 0),
    pred = if_else(deploy_row == 1, Y, Y + out$results$mean$B.ar1 * resid_lag),
    resid = temp - pred
  ) %>%
  ungroup()


# RMSE
sqrt(mean(df_valid$resid^2))
sqrt(mean(df_valid$resid_Y^2))

df_valid %>%
  select(resid, resid_Y) %>%
  gather(var, value) %>%
  ggplot(aes(value, fill = var)) +
  geom_histogram() +
  facet_wrap(~ var, labeller = labeller(
    var = c(
      "resid" = "w/ autoregressive",
      "resid_Y" = "w/o autoregressive"
    )
  ))

df_valid %>%
  select(resid, resid_Y) %>%
  gather(var, value) %>%
  ggplot(aes(value, color = var)) +
  stat_ecdf() +
  scale_color_discrete("", labels = c(
    "resid" = "w/ autoregressive",
    "resid_Y" = "w/o autoregressive"
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

df_valid_site <- df_valid %>%
  group_by(site) %>%
  nest() %>%
  mutate(
    n = map_int(data, nrow),
    rmse = map_dbl(data, function (x) {
      sqrt(mean(x$resid^2))
    })
  )

df_valid_site %>%
  arrange(desc(rmse))

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
  arrange(desc(rmse)) %>%
  head(10) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = Y)) +
  geom_point(aes(y = temp), size = 1, color = "orangered") +
  facet_wrap(~ site, scales = "free")

df_valid_site %>%
  arrange(rmse) %>%
  head(10) %>%
  unnest(data) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = Y)) +
  geom_point(aes(y = temp), size = 1, color = "orangered") +
  facet_wrap(~ site, scales = "free")

table(df_valid$pred > df_valid$temp) / nrow(df_valid)
table(df_valid$Y > df_valid$temp) / nrow(df_valid)



# convergence -------------------------------------------------------------

reject <- rejectionRate(out$results$samples)
reject[reject > 0] # same as 20160715

ggs_out <- ggs(out$results$samples)

ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-B0.pdf"), family = "B.0", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-mu-huc.pdf"), family = "mu.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-mu-year.pdf"), family = "B.year", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-sigma-site.pdf"), family = "sigma.b.site", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-sigma-huc.pdf"), family = "sigma.b.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-sigma-year.pdf"), family = "sigma.b.year", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-ar1-rho-huc.pdf"), family = "rho.B.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-ar1-B-ar1.pdf"), family = "B.ar1", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)


# coef --------------------------------------------------------------------

library(rjags)


M.ar1 <- readRDS("~/models/20160715/jags.rds")


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
