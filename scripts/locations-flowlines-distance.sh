#!/bin/bash
# Export location_flowline table with distance of each location to nearest truncated_flowline
# output: locations-flowlines-distance.csv
# usage: $ ./locations-flowlines-distance.sh

set -eu
set -o pipefail

. ./load_config.sh

echo Exporting locations-flowlines-distance.csv...

psql -h $SHEDS_STM_DB_HOST -d $SHEDS_STM_DB_DBNAME -w -c "
COPY (
  WITH t1 AS (
    SELECT
      l.id AS location_id,
      l.catchment_id AS featureid,
      l.latitude,
      l.longitude,
      ST_SetSRID(ST_MakePoint(l.longitude, l.latitude), 4326) as geom,
      f.geom AS geom_line,
      f.geom_pour AS geom_pour
    FROM locations l
    LEFT JOIN gis.truncated_flowlines f
    ON l.catchment_id=f.featureid
  ), t2 AS (
    SELECT location_id, latitude, longitude, featureid, geom, ST_Transform(ST_ShortestLine(geom_line, geom), 26918) AS geom_line, ST_Transform(ST_ShortestLine(geom_pour, geom), 26918) AS geom_pour
    FROM t1
  )
  SELECT location_id, featureid, ST_Length(geom_line) AS line_distance_m, ST_Length(geom_pour) AS pour_distance_m
  FROM t2
) TO STDOUT WITH CSV HEADER " > $SHEDS_STM_WD/locations-flowlines-distance.csv
