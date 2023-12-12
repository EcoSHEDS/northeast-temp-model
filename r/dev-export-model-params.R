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

model <- mout$results

