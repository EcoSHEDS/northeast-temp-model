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

The model documentation contains the theory, data processing steps, calibration/validation procedures, and results of each model version. As the model evolves over time, so too must the documentation.

To facilitate updates to the documentation, it is generated using the [bookdown R package](https://bookdown.org/yihui/bookdown/) for authoring technical documents with R Markdown.

The documentation source code can be found in the `r/docs` folder. See `r/docs/README.md` for more details about updating, compiling and deploying the model documentation.


## Model Execution

Use the `run-model.sh` script to run the full sequence of model steps. **Remember** to change the model version within `version.sh` to create the working directory prior to running this script or you will overwrite previous results.

```
./run-model.sh
```

Currently, I recommend that you **do not run this script directly**, but rather run each command within that script individually so you can check the output along the way. However, in theory you should be able to run this single command to perform all steps from gathering and processing the input data to fitting the model to generating the final prediction outputs.

The results are exported to the database table `temp_model` (via `r/export-db.R`) and to a csv file `r/csv/sheds-temp-model-v{VERSION}.csv` (via `r/export-csv.R`).
