#!/bin/bash
# Creates a text file listing location_id's that are tidally influenced
# output: locations-tidal.txt
# usage: $ ./locations-tidal.sh

set -eu
set -o pipefail

. ./load-config.sh

echo Identifying locations in tidal zone...

psql -h $SHEDS_STM_DB_HOST -d $SHEDS_STM_DB_DBNAME -w -c "
SELECT * INTO TEMPORARY locations_temp FROM public.locations;

ALTER TABLE locations_temp ADD COLUMN geom geometry(POINT,4326);
UPDATE locations_temp SET geom = ST_SetSRID(ST_MakePoint(longitude,latitude),4326);

CREATE INDEX idx_loc_dum_geom ON locations_temp USING GIST(geom);

COPY (
  SELECT locations_temp.id as location_id
  FROM locations_temp, tidal_zones
  WHERE ST_Intersects(locations_temp.geom, tidal_zones.geom)
) TO STDOUT " > $SHEDS_STM_WD/locations-tidal.txt
