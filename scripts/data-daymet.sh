#!/bin/bash
# Retrieve daymet data for [featureid,year] listed in daymet-featureid_year.csv
# output: data-daymet.csv
# usage: $ ./data-daymet.sh

set -eu
set -o pipefail

. ./load-config.sh

psql -h $SHEDS_STM_DB_HOST -d $SHEDS_STM_DB_DBNAME -w -c "
CREATE TEMPORARY TABLE daymet_featureid_year (featureid bigint, year int);
COPY daymet_featureid_year (featureid, year) FROM STDIN WITH CSV HEADER;

COPY (
  WITH t0 AS (
    SELECT d.*
    FROM daymet d
    INNER JOIN daymet_featureid_year dt
      ON d.featureid=dt.featureid AND d.year=dt.year
  ), t1 AS (
    SELECT
      featureid, year,
      unnest(tmax) AS tmax,
      unnest(tmin) AS tmin,
      unnest(prcp) AS prcp
    FROM t0
  ), t2 AS (
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
) TO STDOUT WITH CSV HEADER;
" < ${SHEDS_STM_WD}/daymet-featureid_year.csv > ${SHEDS_STM_WD}/data-daymet.csv
