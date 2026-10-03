
rm(list = ls())

restore_environment <- function(project = ".") {

  project <- normalizePath(
    project,
    winslash = "/",
    mustWork = TRUE
  )

  lockfile <- file.path(
    project,
    "renv.lock"
  )

  activate_file <- file.path(
    project,
    "renv",
    "activate.R"
  )

  if (!file.exists(lockfile)) {
    stop(
      "renv.lock was not found: ",
      lockfile
    )
  }

  if (!file.exists(activate_file)) {
    stop(
      "renv activation script was not found: ",
      activate_file
    )
  }

  old_wd <- getwd()

  on.exit(
    setwd(old_wd),
    add = TRUE
  )

  setwd(project)

  # Bootstrap and activate the project's own renv environment.
  #
  # renv/activate.R can bootstrap renv itself on a fresh machine,
  # so renv does not need to be installed globally beforehand.
  source(
    activate_file,
    local = .GlobalEnv
  )

  if (!requireNamespace(
    "renv",
    quietly = TRUE
  )) {
    stop(
      "renv could not be bootstrapped from renv/activate.R."
    )
  }

  # Restore exactly the package versions recorded in renv.lock.
  renv::restore(
    project = project,
    lockfile = lockfile,
    prompt = FALSE,
    retry = FALSE
  )

  # Explicitly ensure that this running R process is using the
  # restored project library.
  renv::load(
    project = project,
    quiet = TRUE
  )

  invisible(TRUE)
}


project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

restore_environment(project_root)

start_time <- Sys.time()
source("./project_support.r")
tic.clearlog()

##############

tic("fit models")
dir_init("./1_fit_models/inputs")
file.copy("./inputs/final_regression_data.csv", "./1_fit_models/inputs")
setwd("./1_fit_models")
source("./fit_models.r")
setwd("..")
toc(log = TRUE)

##############

tic("explore fitted models")
dir_init("./2_explore_models/inputs")
files <- list.files("./1_fit_models/output", pattern = "*.robj",
  full.names = TRUE)
file.copy(files, "./2_explore_models/inputs")
setwd("./2_explore_models")
source("./explore_models.r")
setwd("..")
toc(log = TRUE)

##############

tic("simulate population using model fits")
dir_init("./3_simulate_ppr/inputs")
file.copy("./inputs/Mosuo_pop_reg.csv", "./3_simulate_ppr/inputs")
files <- list.files("./1_fit_models/output", pattern = "*.robj",
  full.names = TRUE)
file.copy(files, "./3_simulate_ppr/inputs")
setwd("./3_simulate_ppr")
source("./simulate_ppr.r")
setwd("..")
toc(log = TRUE)

##############

tic("explore simulations and calculate ppr")
dir_init("./4_explore_simulation/inputs")
files <- list.files("./3_simulate_ppr/output", pattern = "*.csv",
  full.names = TRUE)
file.copy(files, "./4_explore_simulation/inputs")
setwd("./4_explore_simulation")
source("./explore_simulation.r")
setwd("..")
toc(log = TRUE)

##############

dir_init("./output")
files <- list.files(
  c(
    "./2_explore_models/output",
    "./4_explore_simulation/output"
  ), full.names = TRUE)
file.copy(files, "./output")

##############

if (!exists("start_time")) start_time <- "unknown"
write_log(title = "project: Mosuo parity-progression ratio analysis",
  path = "./output/log.txt", start_time = start_time)
