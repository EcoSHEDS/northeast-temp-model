#!/bin/bash
# Run the stream temperature model from start to finish
# usage: ./run-model.sh

set -eu

cd scripts
# create list of locations to exclude (tidal + impounded)
./locations-exclude.sh            # -> locations-tidal.txt, locations-impoundment.txt, locations-exclude.txt

# calculate distances from each location to the nearest flowline and catchment pour point
./locations-flowlines-distance.sh # -> locations-flowlines-distance.csv


cd ../r
# retrieve catchment-huc8 list from database
Rscript data-huc.R                # -> data-huc.rds

# retrieve all stream temperature observations from database
Rscript data-db.R                 # -> data-db.rds, daymet-featureid_year.csv


cd ../scripts
# retrieve daymet data from database
./data-daymet.sh                  # -> data-daymet.csv


cd ../r
# retrieve covariate data from database
Rscript data-covariates.R               # -> data-covariates.rds

# merge, qaqc, and process input datasets
Rscript data-clean.R                    # -> data-clean.rds

# determine spring/fall breakpoints
Rscript data-breakpoints.R              # -> data-breakpoints.rds

# prepare model input dataset
Rscript model-input.R                   # -> model-input.rds

# fit the model using JAGS
Rscript model-execute.R                 # -> model-output.rds

# generate diagnostics dataset and plots
Rscript model-diagnostics.R             # -> model-diagnostics.rds

# generate annual derived metrics of predictions for each catchment
Rscript model-predict-year.R            # -> model-predict-year.rds

# calculate overall mean derived metrics of predictions for each catchment
Rscript model-predict-derived.R         # -> model-predict-derived.[rds,csv]

# save derived metrics to database
Rscript export-db.R                     # -> db{stm_predict}
