# compute final derived metrics from annual predictions
# <- model-predict-year.rds
# -> model-predict-derived.[rds,csv]

rm(list=ls())

start <- lubridate::now(tzone = "US/Eastern")
cat("starting model-predict-derived:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))

config <- fromJSON("../config.json")

cat("loading model-predict-year.rds...")
df_site_year <- readRDS(file.path(config$wd, "model-predict-year.rds")) %>%
  arrange(site, year)
cat("done\n")

cat("computing derived metrics by site...")
df_site <- df_site_year %>%
  group_by(site) %>%
  summarise(
    mean_max_temp = mean(max_temp),
    max_max_temp = max(max_temp),
    mean_jun_temp = mean(mean_jun_temp),
    mean_jul_temp = mean(mean_jul_temp),
    mean_aug_temp = mean(mean_aug_temp),
    mean_summer_temp = mean(mean_summer_temp),
    max_temp_30d = mean(max_temp_30d),
    n_day_temp_gt_18 = mean(n_day_temp_gt_18),
    n_day_temp_gt_20 = mean(n_day_temp_gt_20),
    n_day_temp_gt_22 = mean(n_day_temp_gt_22),
    freq_temp_gt_18 = mean(max_temp > 18) * n(),
    freq_temp_gt_20 = mean(max_temp > 20) * n(),
    freq_temp_gt_22 = mean(max_temp > 22) * n(),
    resist = mean(resist)
  )
cat("done (nrow = ", nrow(df_site), ")\n", sep = "")

summary(df_site)
# NA's for 105 catchments due to missing daymet data
# these catchments tend to be along coastline and associated with daymet points that have NA values
# replace with nearest neighbor catchment that does have data ?
df_site %>% filter(is.na(mean_max_temp)) %>% pull(site)

cat("dropping ", sum(is.na(df_site$mean_max_temp)), " catchments with null values (coastline)...", sep = "")
df_site <- df_site %>%
  filter(!is.na(mean_max_temp))
cat("done (nrow = ", nrow(df_site), ")\n", sep = "")

# plot --------------------------------------------------------------------

df_site %>%
  gather(var, value, -site) %>%
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~ var, scales = "free")

# exporting results -------------------------------------------------------

df_site %>%
  saveRDS(file.path(config$wd, "model-predict-derived.rds"))

df_site %>%
  write_csv(file.path(config$wd, "model-predict-derived.csv"), na = "")

end <- lubridate::now(tzone = "US/Eastern")
cat("finished model-predict-derived:", as.character(end, tz = "US/Eastern"), "\n")

# compare to dan's model --------------------------------------------------

# df_2016 <- read_csv(
#   "~/Projects/sheds/model/20160715/derived_site_metrics.csv",
#   col_types = cols(
#     .default = col_double()
#   ),
#   na = "NA"
# ) %>%
#   mutate(featureid = as.integer(featureid)) %>%
#   select(
#     site = featureid,
#     mean_max_temp = meanMaxTemp,
#     max_max_temp = maxMaxTemp,
#     mean_jul_temp = meanJulyTemp,
#     mean_aug_temp = meanAugTemp,
#     mean_summer_temp = meanSummerTemp,
#     max_temp_30d = mean30DayMax,
#     n_day_temp_gt_18 = meanDays.18,
#     n_day_temp_gt_20 = meanDays.20,
#     n_day_temp_gt_22 = meanDays.22,
#     freq_temp_gt_18 = yearsMaxTemp.18,
#     freq_temp_gt_20 = yearsMaxTemp.20,
#     freq_temp_gt_22 = yearsMaxTemp.22,
#     resist = meanResist
#   ) %>%
#   filter(
#     site %in% unique(df_site$site)
#   )
#
# summary(df_2016)
#
# df <- bind_rows(
#   df_site %>% mutate(model = "20180501"),
#   df_2016 %>% mutate(model = "20160715")
# ) %>%
#   gather(var, value, -site, -model)
#
# df %>%
#   spread(model, value) %>%
#   ggplot(aes(`20160715`, `20180501`)) +
#   geom_abline() +
#   geom_point(size = 1, alpha = 0.5) +
#   facet_wrap(~ var, scales = "free")

