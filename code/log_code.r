
rm(list=ls())

logs <- list.files('./inputs', pattern='*log.txt', full.names=TRUE)
projectlog <- character(0)
for(i in 1:length(logs)) projectlog <- c(projectlog, readLines(logs[i]))
o <- order(substr(projectlog, 1, 19))
projectlog <- projectlog[o]

diff <- as.POSIXlt(substr(projectlog[length(projectlog)], 1, 19)) - as.POSIXlt(substr(projectlog[1], 1, 19))

final <- paste('total project time:', round(as.numeric(diff), 2), attr(diff, 'units'), sep=" ")

projectlog <- c(projectlog, final)

if(dir.exists('./output')) unlink('./output', recursive=TRUE)
dir.create('./output')

datestamp <- substr(as.character(Sys.time()), 1, 19)
datestamp <- gsub(' ', '_', datestamp)
datestamp <- gsub(':', '_', datestamp)
my_filename <- paste('./output/', 'pprmodel_log_', datestamp, '.txt', sep='')
writeLines(projectlog, my_filename)