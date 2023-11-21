library(tidyverse)
library(glue)

fetch <- function(x) {
  featureids <- x$featureid
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "daymet",
    user = "jeff",
    password = "HYAB3Vuucwo!"
  )
  sql <- glue_sql("
    WITH t1 AS (
      SELECT
        featureid, year,
        unnest(tmax) AS tmax,
        unnest(tmin) AS tmin
      FROM daymet
      WHERE featureid IN ({featureids*})
    ), t2 AS (
      SELECT
        featureid, year,
        row_number() OVER () as i,
        (tmax + tmin) / 2 as airtemp
      FROM t1
    ), t3 AS (
      SELECT
        featureid,
        (DATE (year || '-01-01')) + ((row_number() OVER (PARTITION BY featureid, year ORDER BY i)) - 1)::integer AS date,
        airtemp
      FROM t2
    )
    select featureid, avg(airtemp) as airtemp_jul
    from t3
    where date_part('month', date) = 7
    group by featureid;
  ", .con = con)
  y <- DBI::dbGetQuery(con, sql)
  DBI::dbDisconnect(con)
  y
}

# con <- DBI::dbConnect(
#   RPostgres::Postgres(),
#   dbname = "daymet",
#   user = "jeff",
#   password = "HYAB3Vuucwo!"
# )
# df_featureids <- DBI::dbGetQuery(con, "SELECT DISTINCT featureid FROM daymet")
# write_rds(df_featureids, "/home/jeff/featureids.rds")
# DBI::dbDisconnect(con)

df_featureids <- read_rds("/home/jeff/featureids.rds")



library(furrr)
plan(multisession, workers = 12)
df_nested <- df_featureids %>% 
  mutate(
    chunk = ceiling(row_number() / 1000)
  ) %>% 
  nest_by(chunk)

# pt <- proc.time()
# fetch(df_nested$data[[1]])
# proc.time() - pt

x <- future_map(df_nested$data, fetch)

bind_rows(x) %>% 
  write_rds("/home/jeff/airtemp_7.rds")

# y <- read_rds("/home/jeff/airtemp_7.rds")
# y %>%
#   ggplot(aes(airtemp_jul)) +
#   geom_histogram()
