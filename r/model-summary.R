

# results -----------------------------------------------------------------

#
# # RMSE
# sqrt(mean(df_calib$resid^2))
# sqrt(mean(df_calib$resid_trend^2))
# df_calib_site %>%
#   arrange(desc(rmse))
# df_calib_site %>%
#   arrange(desc(rmse_trend))
# df_calib_huc %>%
#   arrange(desc(rmse))
# df_calib_huc %>%
#   arrange(desc(rmse_trend))

# df_calib %>%
#   ggplot(aes(resid)) +
#   geom_histogram()
#
# df_calib %>%
#   ggplot(aes(resid)) +
#   stat_ecdf()
#
# df_calib %>%
#   ggplot(aes(temp, pred)) +
#   geom_abline() +
#   geom_point(size = 1, alpha = 0.2) +
#   labs(
#     x = "Observed Temp (degC)",
#     y = "Predicted Temp (degC)"
#   ) +
#   theme(aspect.ratio = 1)
#
# df_calib %>%
#   ggplot(aes(temp, Y)) +
#   geom_abline() +
#   geom_point(size = 1, alpha = 0.2) +
#   labs(
#     x = "Observed Temp (degC)",
#     y = "Predicted Temp (degC) w/o Autoregressive Term"
#   ) +
#   theme(aspect.ratio = 1)
#
# df_calib_site %>%
#   arrange(desc(rmse)) %>%
#   head(10) %>%
#   unnest(data) %>%
#   ggplot(aes(date)) +
#   geom_line(aes(y = pred)) +
#   geom_point(aes(y = temp), size = 1, color = "orangered") +
#   facet_wrap(~ site, scales = "free")
#
# df_calib_site %>%
#   arrange(rmse) %>%
#   head(10) %>%
#   unnest(data) %>%
#   ggplot(aes(date)) +
#   geom_line(aes(y = pred)) +
#   geom_point(aes(y = temp), size = 1, color = "orangered") +
#   facet_wrap(~ site, scales = "free")
#
# # w/o autoregressive term
# df_calib_site %>%
#   arrange(desc(rmse_trend)) %>%
#   head(12) %>%
#   unnest(data) %>%
#   ggplot(aes(date)) +
#   geom_line(aes(y = Y, group = deploy_id)) +
#   geom_point(aes(y = temp), size = 1, color = "orangered", shape = 1) +
#   facet_wrap(~ site, scales = "free") +
#   labs(x = "Date", y = "Predicted Temperature (degC) w/o Autoregressive Term")
#
# df_calib_site %>%
#   filter(n > 100) %>%
#   arrange(rmse_trend) %>%
#   head(12) %>%
#   unnest(data) %>%
#   ggplot(aes(date)) +
#   geom_line(aes(y = Y)) +
#   geom_point(aes(y = temp), size = 1, color = "orangered", shape = 1) +
#   facet_wrap(~ site, scales = "free")
#
# table(df_calib$pred > df_calib$temp) / nrow(df_calib)
# table(df_calib$Y > df_calib$temp) / nrow(df_calib)
#
# df_calib %>%
#   ungroup() %>%
#   select(temp, Y, pred) %>%
#   gather(var, value) %>%
#   mutate(var = ordered(var, levels = c("temp", "Y", "pred"))) %>%
#   ggplot(aes(value, color = var)) +
#   geom_density() +
#   scale_color_discrete(
#     "",
#     labels = c(
#       "pred" = "Predict w/ Autoregressive",
#       "temp" = "Observed",
#       "Y" = "Predict w/o Autoregressive"
#     )
#   )
#
# df_calib_huc %>%
#   arrange(desc(rmse)) %>%
#   head(1) %>%
#   unnest(data) %>%
#   ggplot(aes(date)) +
#   geom_line(aes(y = pred)) +
#   geom_point(aes(y = temp), color = "orangered", size = 1) +
#   facet_wrap(~site, scales = "free")
#
# df_calib_huc %>%
#   arrange(desc(rmse_trend)) %>%
#   head(2) %>%
#   unnest(data) %>%
#   ggplot(aes(date)) +
#   geom_line(aes(y = Y)) +
#   geom_point(aes(y = temp), color = "orangered", size = 1) +
#   facet_wrap(~site, scales = "free")
#
# # validation --------------------------------------------------------------
#
# # RMSE
# sqrt(mean(df_valid$resid^2))
# sqrt(mean(df_valid$resid_trend^2))
# df_valid_site %>%
#   arrange(desc(rmse))
# df_valid_site %>%
#   arrange(desc(rmse_trend))
#
# df_valid %>%
#   select(resid, resid_trend) %>%
#   gather(var, value) %>%
#   ggplot(aes(value, fill = var)) +
#   geom_histogram() +
#   facet_wrap(~ var, labeller = labeller(
#     var = c(
#       "resid" = "w/ autoregressive",
#       "resid_trend" = "w/o autoregressive"
#     )
#   ))
#
# df_valid %>%
#   select(resid, resid_trend) %>%
#   gather(var, value) %>%
#   ggplot(aes(value, color = var)) +
#   stat_ecdf() +
#   scale_color_discrete("", labels = c(
#     "resid" = "w/ autoregressive",
#     "resid_trend" = "w/o autoregressive"
#   ))
#
# df_valid %>%
#   ggplot(aes(temp, pred)) +
#   geom_abline() +
#   geom_point(size = 1, alpha = 0.2) +
#   labs(
#     x = "Observed Temp (degC)",
#     y = "Predicted Temp (degC)"
#   ) +
#   theme(aspect.ratio = 1)
#
# df_valid %>%
#   ggplot(aes(temp, Y)) +
#   geom_abline() +
#   geom_point(size = 1, alpha = 0.2) +
#   labs(
#     x = "Observed Temp (degC)",
#     y = "Predicted Temp (degC) w/o Autoregressive Term"
#   ) +
#   theme(aspect.ratio = 1)
#
#
# df_valid_site %>%
#   arrange(desc(rmse)) %>%
#   head(10) %>%
#   unnest(data) %>%
#   ggplot(aes(date)) +
#   geom_line(aes(y = pred)) +
#   geom_point(aes(y = temp), size = 1, color = "orangered") +
#   facet_wrap(~ site, scales = "free")
#
# df_valid_site %>%
#   arrange(rmse) %>%
#   head(10) %>%
#   unnest(data) %>%
#   ggplot(aes(date)) +
#   geom_line(aes(y = pred)) +
#   geom_point(aes(y = temp), size = 1, color = "orangered") +
#   facet_wrap(~ site, scales = "free")
#
# # w/o autoregressive term
# df_valid_site %>%
#   arrange(desc(rmse_trend)) %>%
#   head(10) %>%
#   unnest(data) %>%
#   ggplot(aes(date)) +
#   geom_line(aes(y = Y)) +
#   geom_point(aes(y = temp), size = 1, color = "orangered") +
#   facet_wrap(~ site, scales = "free")
#
# df_valid_site %>%
#   arrange(rmse_trend) %>%
#   head(10) %>%
#   unnest(data) %>%
#   ggplot(aes(date)) +
#   geom_line(aes(y = Y)) +
#   geom_point(aes(y = temp), size = 1, color = "orangered") +
#   facet_wrap(~ site, scales = "free")
#
# table(df_valid$pred > df_valid$temp) / nrow(df_valid)
# table(df_valid$Y > df_valid$temp) / nrow(df_valid)


#
# # parameter labels --------------------------------------------------------
#
# B.0_labels <- data_frame(
#   Parameter = paste0("B.0[", 1:length(out$covs$fixed.ef), "]"),
#   Label = paste0("B.0[", out$covs$fixed.ef, "]"),
#   Group = "fixed"
# )
#
# B.huc_labels <- crossing(
#   huc8_id = out$ids$huc8$huc8_id,
#   cov_id = 1:length(out$covs$huc.ef)
# ) %>%
#   left_join(out$ids$huc8, by = "huc8_id") %>%
#   left_join(
#     data_frame(
#       cov_id = 1:length(out$covs$huc.ef),
#       cov = out$covs$huc.ef
#     ),
#     by = "cov_id"
#   ) %>%
#   mutate(
#     Parameter = paste0("B.huc[", huc8_id, ",", cov_id, "]"),
#     Label = paste0("B.huc[", huc8, ",", cov, "]"),
#     Group = "huc"
#   )
#
# B.site_labels <- crossing(
#   site_id = out$ids$site$site_id,
#   cov_id = 1:length(out$covs$site.ef)
# ) %>%
#   left_join(out$ids$site, by = "site_id") %>%
#   left_join(
#     data_frame(
#       cov_id = 1:length(out$covs$site.ef),
#       cov = out$covs$site.ef
#     ),
#     by = "cov_id"
#   ) %>%
#   mutate(
#     Parameter = paste0("B.site[", site_id, ",", cov_id, "]"),
#     Label = paste0("B.site[", site, ",", cov, "]"),
#     Group = "site"
#   )
#
# B.year_labels <- crossing(
#   year_id = out$ids$year$year_id,
#   cov_id = 1:length(out$covs$year.ef)
# ) %>%
#   left_join(out$ids$year, by = "year_id") %>%
#   left_join(
#     data_frame(
#       cov_id = 1:length(out$covs$year.ef),
#       cov = out$covs$year.ef
#     ),
#     by = "cov_id"
#   ) %>%
#   mutate(
#     Parameter = paste0("B.year[", year_id, ",", cov_id, "]"),
#     Label = paste0("B.year[", year, ",", cov, "]"),
#     Group = "year"
#   )
#
# par_labels <- bind_rows(
#   B.0_labels,
#   B.site_labels,
#   B.huc_labels,
#   B.year_labels
# ) %>%
#   select(Parameter, Label, Group)
#
# out_summary <- out$results$summary %>%
#   as_data_frame() %>%
#   mutate(
#     param = rownames(out$results$summary)
#   ) %>%
#   left_join(par_labels, by = c("param" = "Parameter")) %>%
#   rename(label = Label, group = Group) %>%
#   select(group, label, param, everything())
#
# out$results$summary %>%
#   as_data_frame() %>%
#   mutate(
#     param = rownames(out$results$summary)
#   ) %>%
#   inner_join(B.huc_labels, by = c("param" = "Parameter")) %>%
#   ggplot(aes(mean)) +
#   geom_histogram() +
#   facet_wrap(~cov, scales = "free")
#
#
# out$results$summary %>%
#   as_data_frame() %>%
#   mutate(
#     param = rownames(out$results$summary)
#   ) %>%
#   inner_join(B.site_labels, by = c("param" = "Parameter")) %>%
#   ggplot(aes(mean)) +
#   geom_histogram() +
#   facet_wrap(~cov, scales = "free")
#
# out$results$summary %>%
#   as_data_frame() %>%
#   mutate(
#     param = rownames(out$results$summary)
#   ) %>%
#   inner_join(B.site_labels, by = c("param" = "Parameter")) %>%
#   filter(cov == "intercept.site") %>%
#   select(site, intercept_mean = mean) %>%
#   left_join(df_calib_site, by = "site") %>%
#   arrange(desc(intercept_mean)) %>%
#   head(10) %>%
#   unnest(data) %>%
#   ggplot(aes(date)) +
#   geom_line(aes(y = Y)) +
#   geom_point(aes(y = temp), size = 1, color = "orangered") +
#   facet_wrap(~ site, scales = "free")
#
# df_summary <- out$results$summary %>%
#   as_data_frame() %>%
#   mutate(
#     param = rownames(out$results$summary)
#   ) %>%
#   select(param, everything())
#
# # convergence -------------------------------------------------------------
#
# reject <- rejectionRate(out$results$samples)
# reject[reject > 0] # same as 20160715
#
# ggs_B <- ggs(out$results$samples, par_labels = par_labels, family = "^B")
#
# ggs_B %>%
#   filter(Group != "B.0[1]") %>%
#   ggs_caterpillar(family = "B.0")
#
# ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-B0.pdf"), family = "B.0", plot = c("ggs_caterpillar", "ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
# ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-mu-huc.pdf"), family = "mu.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
# ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-mu-year.pdf"), family = "B.year", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
# ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-sigma-site.pdf"), family = "sigma.b.site", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
# ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-sigma-huc.pdf"), family = "sigma.b.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
# ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-sigma-year.pdf"), family = "sigma.b.year", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
# ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-ar1-rho-huc.pdf"), family = "rho.B.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
# ggmcmc(ggs_out, file = file.path(config$wd, "pdf", "ggmcmc-ar1-B-ar1.pdf"), family = "B.ar1", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
#
#
# # coef --------------------------------------------------------------------
#
# M.ar1 <- readRDS("~/models/20160715/")
#
# mcmc_small <- mcmc.list()
# for(i in 1:length(M.ar1)) {
#   bar <- attr(M.ar1[[i]], which = "dimnames")[[2]] #[2000:2200]
#   sna <- M.ar1[[i]][ , which(!grepl("stream.mu", bar) & !grepl("trend", bar))]
#   mcmc_small[[i]] <- as.mcmc(sna)
# }
#
# reject <- rejectionRate(mcmc_small)
# reject[reject > 0]
#
# mcmc_tiny <- mcmc.list()
# for(i in 1:length(M.ar1)) {
#   bar <- attr(M.ar1[[i]], which = "dimnames")[[2]] #[2000:2200]
#   sna <- M.ar1[[i]][ , which(grepl("B.0", bar) | grepl("sigma", bar))]
#   mcmc_tiny[[i]] <- as.mcmc(sna)
# }
# gelman.diag(mcmc_tiny)
#
# system.time(ggs.ar1 <- ggs(mcmc_small))
#
# ggmcmc(ggs.ar1, file = "pdf/ggmcmc-B0.pdf", family = "B.0", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
# ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-B0.pdf"), family = "B.0", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
# ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-mu-huc.pdf"), family = "mu.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
# ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-mu-year.pdf"), family = "B.year", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
# ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-sigma-site.pdf"), family = "sigma.b.site", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
# ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-sigma-huc.pdf"), family = "sigma.b.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
# ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-sigma-year.pdf"), family = "sigma.b.year", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
# ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-ar1-rho-huc.pdf"), family = "rho.B.huc", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
# ggmcmc(ggs.ar1, file = paste0(data_dir, "/figures/ggmcmc-ar1-B-ar1.pdf"), family = "B.ar1", plot = c("ggs_traceplot", "ggs_compare_partial", "ggs_autocorrelation", "ggs_Rhat"), param_page = 4)
#
# # compare models ----------------------------------------------------------
#
# m1 <- readRDS("~/Projects/sheds/model/20160715/ggs-20160715.rds")
# m1 <- m1 %>%
#   mutate(model = "20160715")
# m2 <- ggs(out$results$samples)
# m2 <- m2 %>%
#   mutate(model = "20171108")
#
# m <- bind_rows(m1, m2)
#
# m_B0 <- m %>%
#   filter(str_detect(Parameter, "^B.0"))
#
# table(m_B0$model, m_B0$Parameter)
#
# m_B0 <- m_B0 %>%
#   left_join(B.0_labels, by = "Parameter")
# m_B0 %>%
#   ggplot(aes(value, color = model)) +
#   geom_density() +
#   facet_wrap(~ Label, scales = "free")
#
# pdf(file.path(config$wd, "pdf", "model-comparison-fixed.pdf"), width = 11, height = 8.5)
# m_B0 %>%
#   ggplot(aes(value, color = model)) +
#   geom_density() +
#   facet_wrap(~ Label, scales = "free") +
#   labs(
#     title = "Density"
#   )
#
# m_B0 %>%
#   ggplot(aes(Iteration, value, color = model, group = interaction(model, Chain))) +
#   geom_line() +
#   facet_wrap(~ Label, scales = "free") +
#   labs(
#     title = "Chains"
#   )
#
# dev.off()