suppressPackageStartupMessages(library(RPostgres))
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



mout <- readRDS(file.path(config$wd, "model-output.rds"))
mdiag <- readRDS(file.path(config$wd, "model-diagnostics.rds"))



# catchment ---------------------------------------------------------------

B_catchment_mean <- as_data_frame(mout$results$mean$B.site) %>%
  set_names(nm = mout$covs$site.ef) %>%
  mutate(featureid = mout$ids$site$featureid) %>%
  gather(var, mean, -featureid)
B_catchment_sd <- as_data_frame(mout$results$sd$B.site) %>%
  set_names(nm = mout$covs$site.ef) %>%
  mutate(featureid = mout$ids$site$featureid) %>%
  gather(var, sd, -featureid)

B_catchment <- inner_join(B_catchment_mean, B_catchment_sd, by = c("featureid", "var")) %>%
  gather(stat, value, mean, sd) %>%
  unite(var, var, stat) %>%
  spread(var, value)

mdiag$calib$catchment %>%
  select(-dataset, -data) %>%
  left_join(B_catchment, by = "featureid") %>%
  rename(
    intercept_mean = intercept.site_mean,
    intercept_sd = intercept.site_sd
  ) %>%
  mutate(
    featureid = sprintf("%.0f", featureid)
  ) %>%
  write_csv("model-catchment.csv")

# psql -h trout.local sheds -c "\copy model_catchment from '/Users/jeff/Projects/sheds/temp-model/r/model-catchment.csv' with csv header"

# huc8 --------------------------------------------------------------------

B_huc8_mean <- as_data_frame(mout$results$mean$B.huc) %>%
  set_names(nm = mout$covs$huc.ef) %>%
  mutate(huc8 = mout$ids$huc$huc8) %>%
  gather(var, mean, -huc8)
B_huc8_sd <- as_data_frame(mout$results$sd$B.huc) %>%
  set_names(nm = mout$covs$huc.ef) %>%
  mutate(huc8 = mout$ids$huc$huc8) %>%
  gather(var, sd, -huc8)

B_huc8 <- inner_join(B_huc8_mean, B_huc8_sd, by = c("huc8", "var")) %>%
  gather(stat, value, mean, sd) %>%
  unite(var, var, stat) %>%
  spread(var, value)

mdiag$calib$huc8 %>%
  select(-dataset, -data) %>%
  left_join(B_huc8, by = "huc8") %>%
  rename(
    intercept_mean = intercept.huc_mean,
    intercept_sd = intercept.huc_sd
  ) %>%
  write_csv("model-huc8.csv")

# psql -h trout.local sheds -c "\copy model_huc8 from '/Users/jeff/Projects/sheds/temp-model/r/model-huc8.csv' with csv header"
