DROP TABLE IF EXISTS public.model_catchment;
CREATE TABLE public.model_catchment (
  featureid BIGINT,
  n INT,
  rmse REAL,
  rmse_ar1 REAL,
  airTemp_mean REAL,
  airTemp_sd REAL,
  intercept_mean REAL,
  intercept_sd REAL,
  temp7p_mean REAL,
  temp7p_sd REAL
);

DROP TABLE IF EXISTS public.model_huc8;
CREATE TABLE public.model_huc8 (
  huc8 VARCHAR,
  n INT,
  rmse REAL,
  rmse_ar1 REAL,
  airTemp_mean REAL,
  airTemp_sd REAL,
  intercept_mean REAL,
  intercept_sd REAL,
  temp7p_mean REAL,
  temp7p_sd REAL
);