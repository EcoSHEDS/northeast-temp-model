# export residuals

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

m_diag <- readRDS(file.path(config$wd, "model-diagnostics.rds"))

df_values <- bind_rows(
  m_diag$calib$values,
  m_diag$valid$values
)

df_deploy <- bind_rows(
  m_diag$calib$deploy,
  m_diag$valid$deploy
) %>%
  select(-data)

df_catchment <- bind_rows(
  m_diag$calib$catchment,
  m_diag$valid$catchment
) %>%
  select(-data)

dir.create(file.path(config$wd, "residuals"), showWarnings = FALSE)

df_values %>%
  mutate_at(vars(obs:resid_ar1), signif, digits = 4) %>%
  write_csv(file.path(config$wd, "residuals/residuals-daily.csv"), na = "")
df_deploy %>%
  write_csv(file.path(config$wd, "residuals/residuals-deployments.csv"), na = "")
df_catchment %>%
  write_csv(file.path(config$wd, "residuals/residuals-catchments.csv"), na = "")
