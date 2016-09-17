
rm(list=ls())

set.seed(1868)

# fit models
source('./code/project_functions.r')
dir_init('./fitmodels')
dir_init('./fitmodels/code')
files <- c('./code/project_functions.r', 
    './code/project_variables.r', 
    './code/fitmodels_code.r')
file.copy(files, './fitmodels/code')
dir_init('./fitmodels/inputs')
file.copy('./inputs/final_regression_data.csv', './fitmodels/inputs')
setwd('./fitmodels')
source('./code/fitmodels_code.r')
setwd('..')

# explore models
source('./code/project_functions.r')
dir_init('./exploremodels')
dir_init('./exploremodels/code')
files <- c('./code/project_functions.r', 
    './code/project_variables.r', 
    './code/exploremodels_code.r')
file.copy(files, './exploremodels/code')
dir_init('./exploremodels/inputs')
files <- list.files('./fitmodels/output', pattern='*.robj', full.names=TRUE)
file.copy(files, './exploremodels/inputs')
setwd('./exploremodels')
source('./code/exploremodels_code.r')
setwd('..')

# simulate ppr
source('./code/project_functions.r')
dir_init('./simulateppr')
dir_init('./simulateppr/code')
files <- c('./code/project_functions.r', 
    './code/project_variables.r', 
    './code/simulateppr_code.r')
file.copy(files, './simulateppr/code')
dir_init('./simulateppr/inputs')
file.copy('./inputs/Mosuo_pop_reg.csv', './simulateppr/inputs')
files <- list.files('./fitmodels/output', pattern='*.robj', full.names=TRUE)
file.copy(files, './simulateppr/inputs')
setwd('./simulateppr')
source('./code/simulateppr_code.r')
setwd('..')

# explore simulation
source('./code/project_functions.r')
dir_init('./exploresimulation')
dir_init('./exploresimulation/code')
files <- c('./code/project_functions.r', 
    './code/project_variables.r', 
    './code/exploresimulation_code.r')
file.copy(files, './exploresimulation/code')
dir_init('./exploresimulation/inputs')
files <- list.files('./simulateppr/output', pattern='*.csv', full.names=TRUE)
file.copy(files, './exploresimulation/inputs')
setwd('./exploresimulation')
source('./code/exploresimulation_code.r')
setwd('..')

# log
source('./code/project_functions.r')
dir_init('./log')
dir_init('./log/code')
file.copy('./code/log_code.r', './log/code')
dir_init('./log/inputs')
files <- list.files('.', pattern='*log.txt', full.names=TRUE, recursive=TRUE)
file.copy(files, './log/inputs')
setwd('./log')
source('./code/log_code.r')
setwd('..')

# pass final outputs
source('./code/project_functions.r')
dir_init('./output')
files <- list.files(c(
    './exploremodels/output', 
    './exploresimulation/output'
    ), full.names=TRUE)
tar <- grep('*log.txt', files)
if(length(tar) > 0) files <- files[-tar]
file.copy(files, './output')
files <- list.files('./log/output', full.names=TRUE)
file.copy(files, './output')

# final housekeeping
source('./code/project_variables.r')
if(!save_temp){
    folders <- c(
        './fitmodels',
        './exploremodels', 
        './simulateppr',
        './exploresimulation',
        './log'
    )
    if(length(folders)>0) unlink(folders, recursive=TRUE)
}