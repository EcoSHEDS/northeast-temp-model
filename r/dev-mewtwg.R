# model evaluation for MEWTWG

library(tidyverse)

theme_set(theme_bw())

source("functions.R")

config <- load_config()

# load catchments gis, convert to points
#

con <- DBI::dbConnect(
  RPostgreSQL::PostgreSQL(),
  host = config$db$host,
  port = config$db$port,
  dbname = config$db$dbname,
  user = config$db$user
)

m_diag <- read_rds(file.path(config$wd, "model-diagnostics.rds"))
