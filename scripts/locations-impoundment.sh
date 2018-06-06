#!/bin/bash
# Creates a text file listing location_id's that are near impoundments
# output: locations-impoundment.txt
# usage: $ ./locations-impoundment.sh

set -eu
set -o pipefail

. ./load_config.sh

echo Identifying locations near impoundments...

psql -h $SHEDS_STM_DB_HOST -d $SHEDS_STM_DB_DBNAME -w -c "
SELECT * INTO TEMPORARY locations_temp FROM public.locations;

-- Add geometry
ALTER TABLE locations_temp ADD COLUMN geom geometry(POINT,4326);
UPDATE locations_temp SET geom = ST_SetSRID(ST_MakePoint(longitude,latitude),4326);

ALTER TABLE locations_temp ADD COLUMN buffer geometry(POLYGON,4326);
UPDATE locations_temp SET buffer = ST_Buffer(locations_temp.geom::geography, 10)::geometry;

-- Create indices
CREATE INDEX idx_locations_temp_geom ON locations_temp USING GIST(geom);
CREATE INDEX idx_locations_temp_buffer ON locations_temp USING GIST(buffer);

-- Select points near impoundment zones
COPY (
  SELECT locations_temp.id as location_id
  FROM locations_temp, gis.impoundment_zones_100m
  WHERE ST_Intersects(locations_temp.buffer, gis.impoundment_zones_100m.geom)
) TO STDOUT" > $SHEDS_STM_WD/locations-impoundment.txt
