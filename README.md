SHEDS Stream Temperature Model
==============================

Jeffrey D. Walker, PhD  
[Walker Environmental Research LLC](https://walkerenvres.com/)

Dan Hocking, PhD  
[Frostburg State University](http://hockinglab.weebly.com/)

Ben Letcher, PhD  
[USGS](https://www.lsc.usgs.gov/?q=cafb-research), [UMass](https://eco.umass.edu/people/faculty/letcher-ben/)

## About

This repo contains the production version of the SHEDS Stream Temperature Model, which is based on the [Northeast Temperature Model](https://github.com/Conte-Ecology/conteStreamTemperature_northeast) originally developed by Dan Hocking.

## Model Versioning

The model versioning approach is loosely based on [semantic versioning](https://semver.org/). Each model version contains three numbers of the form `X.Y.Z`:

- `X`: Major version incremented when there is a major change to the underlying model theory or code.
- `Y`: Minor version incremented when a new set of model inputs and outputs are created. This can either be due to an update of the input datasets or a (minor) change in the code.
- `Z`: Patch version incremented when there is a minor change to the documentation or output files, but no change to the model calibration or prediction datasets.

When a new version of the model is complete, a tag should be created in github of the form `vX.Y.Z`, and a title containing both the version and the date (e.g. `vX.Y.Z (MMM DD, YYYY)`).

## Configuration

Most of the scripts in this repo rely on configuration variables set within the `config.sh` file. Because some of these variables contain sensitive information (e.g. database passwords), the `config.sh` file is not tracked in git. 

The configuration includes database connection parameters, and the local path to the model data directory.

However, a template (`config.template.sh`) is provided, with which a new `config.sh` can be set up. 

```bash
cp config.template.sh config.sh
nano config.sh
```

The current (or active) model version is set within the `version.sh` script, which is tracked by git and should be consistent with the repo tags.

The `version.sh` script sets the environmental variable `SHEDS_STM_VERSION` to the **minor** version only, and **does not include the patch number**. In other words, it is only of the form `X.Y` (see Model Versioning above) and does not include a `v` prefix.


## Working Directory

Each version of the model saves and loads all data files from its own working directory. None of the input or output files are stored within this repo.

The working directory for the current model version is determined by combining the environmental variables for the root directory (`SHEDS_STM_ROOT`) and model version (`SHEDS_STM_VERSION`):

`SHEDS_STM_WD = SHEDS_STM_ROOT/SHEDS_STM_VERSION/` (e.g. `/path/to/models/0.9/`)


## Documentation

As the model evolves over time, so too must the documentation.

The model documentation contains the theory, data processing steps, calibration/validation procedures, and results of each model version.

To facilitate updates to the documentation, it is generated using the [bookdown R package](https://bookdown.org/yihui/bookdown/) for authoring technical documents with R Markdown.

The documentation source code can be found in the `r/docs` folder. See `r/docs/README.md` for more details about updating, compiling and deploying the model documentation.


## Workflow

### Data Processing

Identify locations near impoundments and in the tidal zone, which need to be excluded. List of location.id's saved to `$WD/locations-exclude.txt`.

```bash
./scripts/locations-exclude.sh            # -> locations-tidal.txt, locations-impoundment.txt, locations-exclude.txt
```

Calculate minimum distance of each location from nearest flowline and catchment pour point.

```bash
./scripts/locations-flowlines-distance.sh # -> locations-flowlines-distance.csv
```

Retrieve huc8 list from db

```bash
Rscript r/data-huc.R                      # -> data-huc.rds
```

Retrieve stream temperature data from database

```bash
Rscript r/data-db.R                       # -> data-db.rds, daymet-featureid_year.csv
```

Retrieve daymet data from database for featureid

```bash
./scripts/data-daymet.sh                  # -> data-daymet.csv
```

Retrieve covariates from database for all featureids

```bash
Rscript r/data-covariates.R               # -> covariates.rds
```

Process data (QAQC, split, filter)

```bash
Rscript r/data-clean.R                    # -> data-clean.rds
```

1. Remove values within user-defined flags  
2. Remove series missing featureid  
3. Remove series with area_km2 >= 200  
4. Remove series with allonnet >= 50  
5. Remove values with mean < -25  
6. Remove values with mean > 35  
7. Remove values with max > 35  
8. Remove series suspected to be air temperature (slope ~ 1, intercept ~ 0, R2 > 0.95)  
9. Remove series with count < 5  
10. Remove values with persist(mean) > 5 and mean > 3  
11. Remove first/last day of each series if n < median(n)  
12. Remove series with count < 5  
13. Average duplicate dates at each location (overlapping series)  
14. Remove series where location > 100 m from main flowline  
15. Choose location with most summer data in each catchment

Determine spring/fall breakspoints

```bash
Rscript r/data-breakpoints.R      # -> data-breakpoints.rds
```

Prepare model input dataset

```bash
Rscript r/model-input.R           # -> model-input.rds
```

### Run Model

Run the model using JAGS

```bash
Rscript r/model-execute.R         # -> model-output.rds
```

Generate diagnostics dataset and plots

```bash
Rscript r/model-diagnostics.R     # -> model-diagnostics.rds
```

Generate annual predictions

```bash
Rscript r/model-predict-year.R    # -> model-predict-year.rds
```

Calculate derived metrics by site

```bash
Rscript r/model-predict-derived.R # -> model-predict-derived.[rds,csv]
```
