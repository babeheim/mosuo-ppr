
rm(list = ls())

source("./project_support.r")

start.time <- Sys.time()

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
files <- list.files("./1_fit_models/output", pattern = "*.robj", full.names = TRUE)
file.copy(files, "./2_explore_models/inputs")
setwd("./2_explore_models")
source("./explore_models.r")
setwd("..")
toc(log = TRUE)

##############

tic("simulate population using model fits")
dir_init("./3_simulate_ppr/inputs")
file.copy("./inputs/Mosuo_pop_reg.csv", "./3_simulate_ppr/inputs")
files <- list.files("./1_fit_models/output", pattern = "*.robj", full.names = TRUE)
file.copy(files, "./3_simulate_ppr/inputs")
setwd("./3_simulate_ppr")
source("./simulate_ppr.r")
setwd("..")
toc(log = TRUE)

##############

tic("explore simulations and calculate ppr")
dir_init("./4_explore_simulation/inputs")
files <- list.files("./3_simulate_ppr/output", pattern = "*.csv", full.names = TRUE)
file.copy(files, "./4_explore_simulation/inputs")
setwd("./4_explore_simulation")
source("./explore_simulation.r")
setwd("..")
toc(log = TRUE)

##############

dir_init("./output")

tic.log(format = TRUE)
msg_log <- unlist(tic.log())
msg_log <- paste0("- ", msg_log)
if(!exists("start_time")) start_time <- NA
header <- c(
  "project: Mosuo parity-progression ratio analysis",
  paste("start_time:", start_time),
  paste("finish_time:", Sys.time()),
  "events:")
msg_log <- c(header, msg_log)
writeLines(msg_log, "./output/log.txt")

files <- list.files(c("./2_explore_models/output", "./4_explore_simulation/output"
    ), full.names = TRUE)
file.copy(files, "./output")
