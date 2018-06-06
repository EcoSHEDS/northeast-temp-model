#!/bin/bash
# Run the stream temperature model from start to finish
# usage: ./run-model.sh

set -eu

# create list of locations to exclude (tidal + impounded)
./scripts/locations-exclude.sh            # -> locations-tidal.txt, locations-impoundment.txt, locations-exclude.txt

# calculate distances from each location to the nearest flowline and catchment pour point
./scripts/locations-flowlines-distance.sh # -> locations-flowlines-distance.csv

# retrieve catchment-huc8 list from database
Rscript r/data-huc.R                      # -> data-huc.rds

# retrieve all stream temperature observations from database
Rscript r/data-db.R                       # -> data-db.rds, daymet-featureid_year.csv

# retrieve daymet data from database
./scripts/data-daymet.sh                  # -> data-daymet.csv

# retrieve covariate data from database
Rscript r/data-covariates.R               # -> covariates.rds

# merge, qaqc, and process input datasets
Rscript r/data-clean.R                    # -> data-clean.rds

# determine spring/fall breakpoints
script r/data-breakpoints.R               # -> data-breakpoints.rds

# prepare model input dataset
Rscript r/model-input.R                   # -> model-input.rds

# fit the model using JAGS
Rscript r/model-execute.R                 # -> model-output.rds

# generate diagnostics dataset and plots
Rscript r/model-diagnostics.R             # -> model-diagnostics.rds

# generate annual derived metrics of predictions for each catchment
Rscript r/model-predict-year.R            # -> model-predict-year.rds

# calculate overall mean derived metrics of predictions for each catchment
Rscript r/model-predict-derived.R         # -> model-predict-derived.[rds,csv]
