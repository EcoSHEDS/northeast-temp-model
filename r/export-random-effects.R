
params_huc8 <- bind_rows(
  setNames(as_data_frame(m_out$results$mean$B.huc), m_out$covs$huc.ef) %>%
    mutate(stat = "mean") %>%
    bind_cols(m_out$ids$huc),
  setNames(as_data_frame(m_out$results$sd$B.huc), m_out$covs$huc.ef) %>%
    mutate(stat = "sd") %>%
    bind_cols(m_out$ids$huc)
) %>%
  gather(var, value, -stat, -huc8, -huc8_id) %>%
  mutate(
    var = factor(var, levels = m_out$covs$huc.ef, ordered = TRUE)
  ) %>%
  spread(stat, value) %>%
  arrange(var, huc8_id) %>%
  select(-huc8_id)

params_catchment <- bind_rows(
  setNames(as_data_frame(m_out$results$mean$B.site), m_out$covs$site.ef) %>%
    mutate(stat = "mean") %>%
    bind_cols(m_out$ids$site),
  setNames(as_data_frame(m_out$results$sd$B.site), m_out$covs$site.ef) %>%
    mutate(stat = "sd") %>%
    bind_cols(m_out$ids$site)
) %>%
  gather(var, value, -stat, -featureid, -featureid_id) %>%
  mutate(
    var = factor(var, levels = m_out$covs$site.ef, ordered = TRUE)
  ) %>%
  spread(stat, value) %>%
  arrange(var, featureid_id) %>%
  select(-featureid_id)


params_huc8 %>%
  write_csv("~/stream-temp-v1.1.1-huc8-randeff.csv")
params_catchment %>%
  write_csv("~/stream-temp-v1.1.1-catchment-randeff.csv")
