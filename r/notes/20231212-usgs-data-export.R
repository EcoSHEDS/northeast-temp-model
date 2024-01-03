# export usgs datasets for importing to database

library(tidyverse)

# neversink ---------------------------------------------------------------

out <- read_rds("data/neversink/data-neversink.rds")

# potomoc -----------------------------------------------------------------

potomoc <- read_rds("data/potomoc/data-potomoc.rds")

# nwis --------------------------------------------------------------------

nwis <- read_rds("data/nwis/data-nwis.rds")
