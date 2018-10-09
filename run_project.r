
rm(list = ls())

source("./project_support.r")

# fit models
dir_init("./1_fit_models/inputs")
file.copy("./inputs/final_regression_data.csv", "./1_fit_models/inputs")
setwd("./1_fit_models")
source("./fit_models.r")
setwd("..")

# explore models
dir_init("./2_explore_models/inputs")
files <- list.files("./1_fit_models/output", pattern = "*.robj", full.names = TRUE)
file.copy(files, "./2_explore_models/inputs")
setwd("./2_explore_models")
source("./explore_models.r")
setwd("..")

# simulate ppr
dir_init("./3_simulate_ppr/inputs")
file.copy("./inputs/Mosuo_pop_reg.csv", "./3_simulate_ppr/inputs")
files <- list.files("./1_fit_models/output", pattern = "*.robj", full.names = TRUE)
file.copy(files, "./3_simulate_ppr/inputs")
setwd("./3_simulate_ppr")
source("./simulate_ppr.r")
setwd("..")

# explore simulation
dir_init("./4_explore_simulation/inputs")
files <- list.files("./3_simulate_ppr/output", pattern = "*.csv", full.names = TRUE)
file.copy(files, "./4_explore_simulation/inputs")
setwd("./4_explore_simulation")
source("./explore_simulation.r")
setwd("..")

# pass final outputs
dir_init("./output")
files <- list.files(c("./2_explore_models/output", "./4_explore_simulation/output"
    ), full.names = TRUE)
file.copy(files, "./output")
