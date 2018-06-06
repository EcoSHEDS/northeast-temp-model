# generate predictions for all catchments
# <- covariates.rds
# <- huc.rds
# <- model-input.rds
# <- model-output.rds
# -> model-predict-year.rds

rm(list=ls())

start <- lubridate::now(tzone = "US/Eastern")
cat("starting model-predict-year:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(doParallel))

source("functions.R")

config <- load_config()

# setup cluster -----------------------------------------------------------

cl <- makeCluster(12)
registerDoParallel(cl)

# load --------------------------------------------------------------------

cat("loading hucs...")
df_huc <- readRDS(file.path(config$wd, "huc.rds"))
cat("done (nrow = ", nrow(df_huc), ")\n", sep = "")

cat("loading covariates...")
df_covariates <- readRDS(file.path(config$wd, "covariates.rds")) %>%
  filter(
    AreaSqKM <= 200,
    allonnet < 70
  ) %>%
  mutate(
    impoundArea = AreaSqKM * allonnet / 100
  )
df_covariates <- df_covariates[complete.cases(df_covariates), ]
cat("done (nrow = ", nrow(df_covariates), ")\n", sep = "")

cat("loading model input/output...")
m_in <- readRDS(file.path(config$wd, "model-input.rds"))
m_out <- readRDS(file.path(config$wd, "model-output.rds"))

df_cov_std <- m_in$std
cov_list <- m_out$covs
ids_list <- m_in$ids

B.site.mean <- colMeans(m_out$results$mean$B.site)
B.huc.mean <- colMeans(m_out$results$mean$B.huc)
B.year.mean <- colMeans(m_out$results$mean$B.year)

coef_list <- list(
  fixed = m_out$results$mean$B.0,
  site = m_out$results$mean$B.site,
  huc = m_out$results$mean$B.huc,
  year = m_out$results$mean$B.year
)
cat("done\n")

predict_daily <- function (featureids) {
  # con <- dbConnect(PostgreSQL(), host = config$db$host, dbname = config$db$dbname, user = config$db$user, password = config$db$password)
  con <- dbConnect(PostgreSQL(), host = "localhost", dbname = "daymet")
  sql_daymet <- paste0("
                       WITH t1 AS (
                       SELECT
                       featureid, year,
                       unnest(tmax) AS tmax,
                       unnest(tmin) AS tmin,
                       unnest(prcp) AS prcp
                       FROM daymet
                       WHERE featureid IN (",
                       paste0("$", seq_along(featureids), collapse = ", "),
                       ")), t2 AS (
                       SELECT
                       featureid, year,
                       row_number() OVER () as i,
                       tmax, tmin, prcp
                       FROM t1
                       )
                       SELECT
                       featureid, year,
                       (DATE (year || '-01-01')) + ((row_number() OVER (PARTITION BY featureid, year ORDER BY i)) - 1)::integer AS date,
                       tmax, tmin, prcp
                       FROM t2
                       ")
  rs <- dbSendQuery(con, sql_daymet, featureids)

  df_daymet <- dbFetch(rs) %>%
    mutate(
      airTemp = (tmin + tmax) / 2,
      airTempLagged1 = lag(airTemp, n = 1, fill = NA),
      temp7p = rollapply(
        data = airTempLagged1,
        width = 7,
        FUN = mean,
        align = "right",
        fill = NA,
        na.rm = TRUE
      ),
      prcp2 = rollsum(x = prcp, 2, align = "right", fill = NA),
      prcp30 = rollsum(x = prcp, 30, align = "right", fill = NA)
    ) %>%
    select(-tmin, -tmax)
  dbClearResult(rs)

  # df_huc <- tbl(con, "catchment_huc12") %>%
  #   filter(featureid %in% featureids) %>%
  #   collect() %>%
  #   mutate(
  #     huc8 = str_sub(huc12, 1, 8)
  #   ) %>%
  #   select(featureid, huc8)

  dbDisconnect(con)

  # merge covariates
  df <- df_covariates %>%
    filter(featureid %in% featureids) %>%
    left_join(df_huc, by = "featureid") %>%
    left_join(df_daymet, by = "featureid") %>%
    mutate(
      site = as.character(featureid),
      spring_bp = 75,
      fall_bp = 330
    ) %>%
    mutate(
      dOY = yday(date)
    ) %>%
    filter(
      dOY >= spring_bp,
      dOY <= fall_bp
    ) %>%
    select(
      site, huc8, year, date,
      airTemp, prcp2, prcp30, temp7p,
      AreaSqKM, forest, devel_hi, agriculture, impoundArea
    )

  # standardize covariates
  df <- df %>%
    gather(var, value, -site, -huc8, -year, -date) %>%
    left_join(df_cov_std, by = "var") %>%
    mutate(value = (value - mean) / sd) %>%
    select(-mean, -sd) %>%
    spread(var, value)

  # compute derived covariates
  df <- df %>%
    mutate(
      prcp2.da = prcp2 * AreaSqKM,
      prcp30.da = prcp30 * AreaSqKM,
      airTemp.prcp2 = airTemp * prcp2,
      airTemp.prcp2.da = airTemp * prcp2 * AreaSqKM,
      airTemp.prcp30 = airTemp * prcp30,
      airTemp.prcp30.da = airTemp * prcp30 * AreaSqKM,
      airTemp.forest = airTemp * forest,
      airTemp.devel_hi = airTemp * devel_hi,
      airTemp.da = airTemp * AreaSqKM,
      airTemp.impoundArea = airTemp * impoundArea,
      airTemp.agriculture = airTemp * agriculture,
      intercept = 1,
      intercept.site = 1,
      intercept.year = 1
    )

  # add id columns
  df <- df %>%
    left_join(ids_list$site, by = "site") %>%
    left_join(ids_list$huc8, by = "huc8") %>%
    left_join(ids_list$year, by = "year")

  # compute predictions
  X.0 <- df %>%
    select(one_of(cov_list$fixed.ef)) %>%
    as.matrix()
  B.0 <- as.matrix(coef_list$fixed)
  Y.0 <- (X.0 %*% B.0)[, 1]

  X.site <- df %>%
    select(one_of(cov_list$site.ef)) %>%
    as.matrix()
  B.site <- coef_list$site[df$site_id, ]
  for (i in seq_along(B.site.mean)) {
    B.site[is.na(B.site[, i]), i] <- B.site.mean[i]
  }
  Y.site <- rowSums(X.site * B.site)

  X.huc <- df %>%
    select(one_of(cov_list$huc.ef)) %>%
    as.matrix()
  B.huc <- coef_list$huc[df$huc8_id, ]
  for (i in seq_along(B.huc.mean)) {
    B.huc[is.na(B.huc[, i]), i] <- B.huc.mean[i]
  }
  Y.huc <- rowSums(X.huc * B.huc)

  X.year <- df %>%
    select(one_of(cov_list$year.ef)) %>%
    as.matrix()
  B.year <- as.matrix(coef_list$year[df$year_id, ])
  for (i in seq_along(B.year.mean)) {
    B.year[is.na(B.year[, i]), i] <- B.year.mean[i]
  }
  Y.year <- rowSums(X.year * B.year)

  Y <- Y.0 + Y.site + Y.year + Y.huc

  df %>%
    mutate(
      temp = Y
    ) %>%
    left_join(
      df_daymet %>%
        select(site = featureid, date, airTemp_degC = airTemp) %>%
        mutate(site = as.character(site)),
      by = c("site", "date")
    ) %>%
    group_by(site, year) %>%
    mutate(
      temp_30d = rollapply(
        data = temp,
        width = 30,
        FUN = mean,
        align = "right",
        fill = NA,
        na.rm = TRUE
      )
    ) %>%
    ungroup()
}

# subset
# set.seed(12345)
# n_featureids <- 101
# featureids <- as.integer(df_covariates$featureid) %>% sample(size = n_featureids, replace = FALSE)

featureids <- as.integer(df_covariates$featureid)
n <- length(featureids)
chunk_size <- 10
n_chunks <- ceiling(n / chunk_size)

# predict_daily(featureids[1:3]) %>% summary

cat("generated predictions for ", n, " featureids (chunk_size = ", chunk_size, ", n_chunks = ", n_chunks, ")...", sep = "")
st <- system.time({
  df_predict_year <- foreach(i = 1:n_chunks, .combine = rbind, .packages = c("RPostgreSQL", "DBI", "dplyr", "tidyr", "purrr", "zoo", "lubridate", "stringr")) %dopar% {
    # sink(log_file, append = TRUE)

    start_i <- ((i - 1) * chunk_size) + 1
    end_i <- i * chunk_size
    if (end_i > length(featureids)) {
      end_i <- length(featureids)
    }
    x_featureids <- featureids[start_i:end_i]
    cat(as.character(Sys.time()), " - i = ", i, " | ", paste(x_featureids, collapse = ","), "\n", sep = "")

    df <- predict_daily(x_featureids)

    # compute derived metrics
    df_nest <- df %>%
      mutate(month = month(date)) %>%
      select(site, year, month, date, airTemp_degC, temp, temp_30d) %>%
      group_by(site, year) %>%
      nest()

    df_derived <- df_nest %>%
      mutate(
        metrics = map(data, function (x) {
          x_summer <- x[x$month %in% 6:8, ]

          data_frame(
            max_temp = max(x[["temp"]]),
            mean_jun_temp = mean(x[["temp"]][x$month == 6]),
            mean_jul_temp = mean(x[["temp"]][x$month == 7]),
            mean_aug_temp = mean(x[["temp"]][x$month == 8]),
            mean_summer_temp = mean(x_summer[["temp"]]),
            max_temp_30d = max(x[["temp_30d"]], na.rm = TRUE),
            n_day_temp_gt_18 = sum(x[["temp"]] > 18),
            n_day_temp_gt_20 = sum(x[["temp"]] > 20),
            n_day_temp_gt_22 = sum(x[["temp"]] > 22),
            resist = sum(abs(x_summer[["airTemp_degC"]] - x_summer[["temp"]]))
          )
        })
      ) %>%
      select(-data) %>%
      unnest(metrics)

    df_derived
  }
})
cat("done (elapsed = ", round(unname(st[3]) / 60, 1), " min, ", round(unname(st[3]) / 60 / 60, 1), " hr)\n", sep = "")
# 20171117 - 32 hours

# df_predict_year
stopCluster(cl)
saveRDS(df_predict_year, file.path(config$wd, "model-predict-year.rds"))
# df_predict_year %>% write_csv(file.path(config$wd, "model-predict-year.csv"))

# df_predict_year %>%
#   gather(var, value, -site, -year) %>%
#   group_by(site, var) %>%
#   summarise(
#     mean = mean(value)
#   ) %>%
#   ungroup() %>%
#   ggplot(aes(mean)) +
#   geom_histogram() +
#   facet_wrap(~var, scales = "free")

end <- lubridate::now(tzone = "US/Eastern")
cat("finished model-predict-year:", as.character(end, tz = "US/Eastern"), "\n")