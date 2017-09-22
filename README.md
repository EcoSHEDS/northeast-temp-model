SHEDS Stream Temperature Model
==============================

Jeffrey D. Walker, PhD
[Walker Environmental Research LLC](https://walkerenvres.com)

## About

This repo contains the production version of the SHEDS Stream Temperature Model, which is based on the Northeast Temperature Model originally developed by Dan Hocking.

## Scripts

```txt
# --------------
# KEY ----------
# --------------
# db: dbname@host
# <- input file
# -> output file

# --------------
# WORKING DIR --
# --------------

modelRun/modelRun_$(date +"%Y-%m-%d")

# --------------
# MAIN SCRIPT --
# --------------

run_mode.sh - primary script

# --------------
# SCRIPTS ------
# --------------

current_model_run.txt - model run id
status_log.txt - logfile

id_impoundment_sites.sh - identify impounded locations
  <- current_model_run.txt
  -> impoundment_sites.csv - list of location.id that intersect with impoundments layer
id_tidal_sites.sh - identify tidal locations
  <- current_model_run.txt
  -> tidal_sites.csv - list of location.id that intersect with tidal layer
retrieve_db.R - fetch streamtemp data from db (exclude locations, qaqc)
  <- current_model_run.txt
  -> temperatureData.RData
  -> covariateData.RData
  -> climateData.RData
daymet_query.sql - fetch daymet data
  <- current_model_run.txt
  -> daymet_results.csv
breakpoints.R - determine breakpoints
  <- current_model_run.txt
  <- temperatureData.RData
  <- daymet_results.csv
  -> springFallBPs.RData
prepare_model_data.R - prepare input data
  <- current_model_run.txt
  <- temperatureData.RData
  <- daymet_results.csv
  <- covariateData.RData
  <- springFallBPs.RData
  -> tempDataSync.RData
run_model.R - run model
  <- current_model_run.txt
  <- tempDataSync.RData
  -> jags.RData
  -> covariate_list.RData
mcmc_diagnostics.R - generate MCMC diagnostics
  <- current_model_run.txt
  -> jags.RData
summarize_iterations.R - generate model summary
  <- current_model_run.txt
  <- tempDataSync.RData
  <- jags.RData
  <- covariate_list.RData
  -> coef.RData
validate_model.R - validate model
  <- current_model_run.txt
  <- model_config.json
  <- tempDataSync.RData
  <- covariate_list.RData
  <- coef.RData
  -> rmse_table.RData
  -> obs_predicted.RData
  -> valid_results.RData
  -> RMSE_table.Rds
  -> Obs_Fitted.pdf
  -> Obs_Valid.pdf
predict_temperatures_parallel.R - calculate derived metrics
  db: sheds_new@osensei
  <- current_model_run.txt
  <- coef.RData
  <- tempDataSync.RData
  <- covariate_list.RData
  <- springFallBPs.RData
  -> db_pull_for_predictions.RData (testing)
  -> log_file.txt
  -> derived_site_metrics_full.RData
  -> derived_site_metrics_full.csv
  -> derived_site_metrics_arc.csv
  -> derived_site_metrics_arc.dbf
  -> derived_site_metrics.RData
  -> derived_site_metrics.csv
  -> derived_metrics_ct.csv
  -> derived_metrics_ct_arc.dbf
  -> derived_metrics_huc[##].csv
data_summary.R - calculate summary by state and agency
  db: sheds@felek
  <- current_model_run.txt
  <- series_used.csv
  <- df_values.RData
  -> agency_summary.Rds
  -> state_summary.Rds
  -> data_totals.Rds
```

## Step-By-Step

### 1: Identify Impoundment Locations

Creates file `impoundment_sites.csv` containing the id of all `locations` that intersect the `gis.impoundment_zones_100m` table.

```sh
psql -h felek.cns.umass.edu -d $DB -w -c "{SQL}" > $FOLDER/impoundment_sites.csv
```

```sql
-- Create temporary table
SELECT * INTO TEMPORARY locations_temp FROM public.locations;

-- Add geometry
ALTER TABLE locations_temp ADD COLUMN geom geometry(POINT,4326);
UPDATE locations_temp SET geom = ST_SetSRID(ST_MakePoint(longitude,latitude),4326);
CREATE INDEX idx_locations_temp_geom ON locations_temp USING GIST(geom);

ALTER TABLE locations_temp ADD COLUMN buffer geometry(POLYGON,4326);
UPDATE locations_temp SET buffer = ST_Buffer(locations_temp.geom::geography, 10)::geometry;
CREATE INDEX idx_locations_temp_buffer ON locations_temp USING GIST(buffer);

-- Select points near impoundment zones
COPY (
SELECT id
  FROM locations_temp, gis.impoundment_zones_100m
  WHERE ST_Intersects(locations_temp.buffer, gis.impoundment_zones_100m.geom)
) TO STDOUT WITH CSV HEADER
```

Ideas:

- Use materialized view to save code for creating locations_temp
- Move `locations_tmp` to `gis.locations`
- Why create buffer (radius = 10?) on `locations_tmp`?

### 2: Identify Tidal Locations

Creates file `impoundment_sites.csv` containing the id of all `locations` that intersect the `gis.impoundment_zones_100m` table.

```sh
psql -h felek.cns.umass.edu -d $DB -w -c "{SQL}" > $FOLDER/tidal_sites.csv
```

```sql
-- Create temporary table
SELECT * INTO TEMPORARY locations_temp FROM public.locations;

-- Add geometry
ALTER TABLE locations_temp ADD COLUMN geom geometry(POINT,4326);
UPDATE locations_temp SET geom = ST_SetSRID(ST_MakePoint(longitude,latitude),4326);
CREATE INDEX idx_loc_dum_geom ON locations_temp USING GIST(geom);

-- Find and output intersection
COPY (
  SELECT locations_temp.id
  FROM locations_temp, tidal_zones
  WHERE ST_Intersects(locations_temp.geom, tidal_zones.geom)
) TO STDOUT WITH CSV HEADER
```

Ideas:

- Creates locations_temp table, could have saved from step 1
- Combine location.id scripts into one script (or one sql statement, faster by scanning entire table once?)

### Step 3: Retrieve Temperature Data

Fetch stream temperature data from database excluding impoundment and tidal locations.

```
Rscript code/retrieve_db.R $dirname"/temperatureData.RData" $dirname"/covariateData.RData" $dirname"/climateData.RData"
```

Output saved to `retreive_log.txt`

```
get table references (locations, series, ...)
save reviewed series ids to series_reviewed
save impoundment/tidal location ids to exclude_locations
fetch values from values_flags
  join series
  join variables
  filter variable.name = "TEMP"
         series.id IN series_reviewed
         series.flagged = false
  set datetime tz to EST
  rename temp = value
fetch locations
  filter agency_name != "TEST"
  rename featureid = catchment_id
join values and locations
  filter is.na(feature)
         location_id IN exclude_locations

qaqc hourly values
  obs_freq()
  flag_incomplete()
  flag_hourly_rise()
  flag_cold_obs()
  flag_hot_obs()
  flag_extreme_obs()
  flag_interval()
  convert_neg()

create sd_flags df from values ->
  flag_incomplete == TRUE
  flag_hourly_rise == TRUE
  flag_cold_obs == TRUE
  flag_hot_obs == TRUE
  flag_extremes == TRUE
  flag_interval == TRUE
save flagged values to subdaily_flags.csv

use Median Absolute Deviation (MAD) to flag plots for manual inspection (generates png)

filter subdaily values -> df_values2
  flag_incomplete == "FALSE",
  flag_cold_obs == "FALSE",
  flag_hot_obs == "FALSE",
  flag_interval == "FALSE" | is.na(flag_interval),
  abs(d_temp) < 5 | is.na(d_temp)
  if mad_tf
    MAD_normalized < 5

compute daily[min, max, mean, n] -> df_values_3

qaqc daily values
  flag_daily_rise()
  flag_cold_days()
  flag_hot_days()
  flag_extreme_days()

filter flagged daily values
  flag_daily_rise == TRUE |
  flag_cold_days == TRUE |
  flag_hot_days == TRUE |
  flag_extreme_days == TRUE

compute MAD_normalized = MAD.roller(temp, median(median_freq)*10)


```

Ideas:

- Use environmental variables for things like current_model_run
- Faster to get data by first excluding series that have excluded locations, then joining values
