DROP TABLE IF EXISTS gis.location_flowline;
CREATE TABLE gis.location_flowline AS (
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
  SELECT *, ST_Length(geom_line) AS line_distance_m, ST_Length(geom_pour) AS pour_distance_m
  FROM t2
);
CREATE INDEX location_flowline_geom_idx ON gis.location_flowline USING gist(geom);
CREATE INDEX location_flowline_geom_loc_idx ON gis.location_flowline USING gist(geom_line);
CREATE INDEX location_flowline_geom_pour_idx ON gis.location_flowline USING gist(geom_pour);
