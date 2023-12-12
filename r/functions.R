load_config <- function(config_file = "../config.sh", version_file = "../version.sh") {
  # path: path to root of repo (where config.sh and version.sh are located)
  readRenviron(config_file)
  readRenviron(version_file)

  wd <- file.path(Sys.getenv("SHEDS_STM_ROOT"), Sys.getenv("SHEDS_STM_VERSION"))

  if (!file.exists(wd)) {
    stop(paste0("ERROR: could not find working directory (", wd, ")"))
  }

  list(
    db = list(
      dbname = Sys.getenv("SHEDS_STM_DB_DBNAME"),
      host = Sys.getenv("SHEDS_STM_DB_HOST"),
      password = Sys.getenv("SHEDS_STM_DB_PASSWORD"),
      port = Sys.getenv("SHEDS_STM_DB_PORT"),
      user = Sys.getenv("SHEDS_STM_DB_USER")
    ),
    version = Sys.getenv("SHEDS_STM_VERSION"),
    root = Sys.getenv("SHEDS_STM_ROOT"),
    wd = wd
  )
}

