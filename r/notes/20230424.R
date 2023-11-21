# temperature slopes for fixed effects
# for Jenny Rogers
library(tidyverse)

source("functions.R")

config <- load_config()

airtemp_jul <- read_csv("notes/20230424/airtemp_jul.csv", col_types = cols(.default = col_double()))
covariates <- read_rds(file.path(config$wd, "data-covariates.rds"))
m_inp <- read_rds(file.path(config$wd, "model-input.rds"))
m_out <- read_rds(file.path(config$wd, "model-output.rds"))

df_covs <- m_inp$std |>
  filter(var %in% c("agriculture", "devel_hi", "forest", "impoundArea", "airTemp")) |>
  left_join(
    tibble(
      var = m_out$covs$fixed.ef,
      beta = m_out$results$mean$B.0
    ),
    by = "var"
  ) |>
  left_join(
    tibble(
      var = m_out$covs$fixed.ef,
      beta = m_out$results$mean$B.0
    ) |>
      filter(
        str_starts(var, "airTemp."),
        !str_ends(var, ".da"),
        !str_detect(var, "prcp")
      ) |>
      transmute(
        var = str_remove_all(var, "airTemp."),
        beta.airTemp = beta
      ),
    by = "var"
  )
covs <- split(select(df_covs, -var), df_covs$var)

write_rds(covs, "/Users/jeff/Dropbox/Work/ecosheds/temp-model/jenny-rogers/20230425/model.rds")

covariates |>
  left_join(airtemp_jul, by = "featureid") |>
  write_rds("/Users/jeff/Dropbox/Work/ecosheds/temp-model/jenny-rogers/20230425/covariates.rds")

df <- covariates |>
  filter(AreaSqKM <= 200) |>
  mutate(
    impoundArea = AreaSqKM * allonnet / 100 # allonnet = % of drainage area (km2) that is impounded
  ) |>
  left_join(airtemp_jul, by = "featureid") |>
  mutate(
    # compute standardized value for airTemp
    airTemp_z = (airtemp_jul - covs$airTemp$mean) / covs$airTemp$sd,

    # increase forest cover by a flat 10% (e.g. 40% to 50%, 75% to 85%)
    # note: forest_delta=10 for all catchments
    forest_new = forest + 10,
    forest_delta = forest_new - forest,
    forest_degC = forest_delta / covs$forest$sd *
      (covs$forest$beta + covs$forest$beta.airTemp * airTemp_z),

    # increase impoundedArea by factor of 2 (double each value)
    # note: delta varies by catchment
    impoundArea_new = impoundArea * 2,
    impoundArea_delta = impoundArea_new - impoundArea,
    impoundArea_degC = impoundArea_delta / covs$impoundArea$sd *
      (covs$impoundArea$beta + covs$impoundArea$beta.airTemp * airTemp_z),

    # compute total effect of increasing forest cover by 10% AND doubling impounded area
    # total effect = sum of each separate effect since they can be linearly combined
    total_degC = forest_degC + impoundArea_degC
  )

df |>
  ggplot(aes(total_degC)) +
  geom_histogram()
