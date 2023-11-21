create table daymet_tmp as
(WITH t1 AS (
  SELECT
    featureid, year,
    unnest(tmax) AS tmax,
    unnest(tmin) AS tmin,
    unnest(prcp) AS prcp
  FROM daymet
), t2 AS (
  SELECT
    featureid, year,
    row_number() OVER () as i,
    tmax, tmin, prcp
  FROM t1
), t3 as (
	SELECT
	  featureid, year, 
	  (DATE (year || '-01-01')) + ((row_number() OVER (PARTITION BY 
featureid, year ORDER BY i)) - 1)::integer AS date,
	  (tmax + tmin) / 2 as airTemp, prcp
	FROM t2
)
select featureid, avg(airtemp) as airtemp
from t3
where date_part('month', date) = 7
group by featureid);



