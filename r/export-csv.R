# export predictions to csv
# <- model-predict-derived.rds
# -> csv/sheds-temp-model-v{VERSION}.csv

start <- lubridate::now(tzone = "US/Eastern")
cat("starting export-csv:", as.character(start, tz = "US/Eastern"), "\n")

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(jsonlite))

source("functions.R")

config <- load_config()

# load --------------------------------------------------------------------

cat("loading predictions...")
df <- readRDS(file.path(config$wd, "model-predict-derived.rds"))
cat("done\n")

# export ------------------------------------------------------------------

cat("dataset structure:\n")
str(df)
cat("\n")

cat("rounding values to 3 digits...")
df_out <- df %>%
  mutate_at(vars(-featureid), ~ sprintf("%.2f", .))
cat("done\n")

fname <- paste0("sheds-temp-model-v", config$version, ".csv")
cat("saving to csv/", fname, "...", sep = "")
write_csv(df_out, file.path("csv", fname), na = "")
cat("done\n")

# done --------------------------------------------------------------------

end <- lubridate::now(tzone = "US/Eastern")
cat("finished export-csv:", as.character(end, tz = "US/Eastern"), "\n")
