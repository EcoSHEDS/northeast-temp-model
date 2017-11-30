# generate predictions for all catchments

rm(list=ls())

start <- lubridate::now(tzone = "US/Eastern")
cat("starting model-predictions:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(doParallel))

cl <- makeCluster(3)
registerDoParallel(cl)

config <- fromJSON("../config.json")

cat("loading covariates...")
df_covariates <- readRDS(file.path(config$wd, "covariates.rds")) %>%
  filter(
    AreaSqKM <= 200,
    allonnet < 70
  )
df_covariates <- df_covariates[complete.cases(df_covariates), ]
cat("done ( nrow =", nrow(df_covariates), ")\n")

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

cat("connecting to db ( host =", config$db$host, ", dbname =", config$db$dbname, ")...")
con <- dbConnect(PostgreSQL(), host = config$db$host, dbname = config$db$dbname, user = config$db$user)
cat("done\n")

featureids <- df_covariates$featureid
n <- length(featureids)
chunk_size <- 10
n_chunks <- ceiling(n / chunk_size)

system.time({
  df_derived_all <- foreach(i = 1:n_chunks, .combine = rbind, .packages = c("RPostgreSQL", "DBI", "dplyr", "tidyr", "purrr", "zoo", "lubridate", "stringr")) %dopar% {
    con <- dbConnect(PostgreSQL(), host = config$db$host, dbname = config$db$dbname, user = config$db$user)
    start_i <- ((i - 1) * n_chunks) + 1
    end_i <- i * n_chunks
    if (end_i > length(featureids)) {
      end_i <- length(featureids)
    }
    x_featureids <- featureids[start_i:end_i]

    df_covariates_upstream <- tbl(con, "covariates") %>%
      filter(
        featureid %in% x_featureids,
        variable %in% c("AreaSqKM", "devel_hi", "agriculture", "allonnet"),
        zone == "upstream",
        is.na(riparian_distance_ft)
      ) %>%
      collect() %>%
      select(featureid, variable, value)
    df_covariates_riparian <- tbl(con, "covariates") %>%
      filter(
        featureid %in% x_featureids,
        variable %in% c("forest"),
        zone == "upstream",
        riparian_distance_ft == 200
      ) %>%
      collect() %>%
      select(featureid, variable, value)

    df_covariates <- bind_rows(df_covariates_upstream, df_covariates_riparian) %>%
      spread(variable, value) %>%
      mutate(
        impoundArea = AreaSqKM * allonnet / 100
      )

    sql_daymet <- paste0("
      WITH t1 AS (
         SELECT
         featureid, year,
         unnest(tmax) AS tmax,
         unnest(tmin) AS tmin,
         unnest(prcp) AS prcp
         FROM daymet
         WHERE featureid IN (",
      paste0("$", seq_along(x_featureids), collapse = ", "),
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
    rs <- dbSendQuery(con, sql_daymet, x_featureids)

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

    df_huc <- tbl(con, "catchment_huc12") %>%
      filter(featureid %in% x_featureids) %>%
      collect() %>%
      mutate(
        huc8 = str_sub(huc12, 1, 8)
      ) %>%
      select(featureid, huc8)

    dbDisconnect(con)

    # merge covariates
    df <- df_covariates %>%
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

    df <- df %>%
      mutate(
        temp = Y,
        temp_30d = rollapply(
          data = temp,
          width = 30,
          FUN = mean,
          align = "right",
          fill = NA,
          na.rm = TRUE
        )
      )

    # compute derived metrics
    df_nest <- df %>%
      mutate(month = month(date)) %>%
      select(site, year, month, date, airTemp, temp, temp_30d) %>%
      group_by(site, year) %>%
      nest()

    df_derived <- df_nest %>%
      mutate(
        metrics = map(data, function (x) {
          x_7 <- x %>%
            filter(month == 7)
          x_8 <- x %>%
            filter(month == 8)
          x_summer <- x %>%
            filter(month %in% c(6:8))
          x_resist <- x %>%
            filter(yday(date) >= 152, yday(date) < 244)

          data_frame(
            max_temp = max(x$temp),
            mean_jul_airtemp = mean(x_7$airTemp),
            mean_jul_temp = mean(x_7$temp),
            mean_aug_airtemp = mean(x_8$airTemp),
            mean_aug_temp = mean(x_8$temp),
            mean_summer_airtemp = mean(x_summer$airTemp),
            mean_summer_temp = mean(x_summer$temp),
            max_temp_30d = max(x$temp_30d),
            n_day_18 = sum(x$temp > 18),
            n_day_20 = sum(x$temp > 20),
            n_day_22 = sum(x$temp > 22),
            resist = sum(abs(x_resist$airTemp - x_resist$temp))
          )
        })
      ) %>%
      select(-data) %>%
      unnest(metrics)

    df_derived
  }
})
df_derived

df_derived %>%
  gather(var, value, -site, -year) %>%
  group_by(site, var) %>%
  summarise(
    mean = mean(value)
  ) %>%
  ungroup() %>%
  ggplot(aes(mean)) +
  geom_histogram() +
  facet_wrap(~var, scales = "free")

saveRDS(df_derived, file.path(config$wd, "derived.rds"))

stopCluster(cl)
dbDisconnect(con)

