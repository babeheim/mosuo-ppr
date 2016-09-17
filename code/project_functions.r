
task_timer <- function(task="", start=start.time, stop=stop.time){
	time.diff <- round(as.numeric(difftime(stop, start, units="mins")), 1)
	output <- paste(Sys.time(), ": ", task, " (", time.diff, " mins)\n", sep="")
	print(output)
	return(output)
}

gradient_maker <- function(start=NA, stop=NA, cols=c("darkorange", "white", "darkcyan"), vis=FALSE, n=1000){
	if(is.na(start) | is.na(stop)) stop("need to specify start and stop points on a numerical scale")
	colfunc <- colorRampPalette(cols)
	color.list <- colfunc(n)
	color.locations <- seq(start, stop, length=n)
	names(color.locations) <- color.list
	if(vis==TRUE) plot(color.locations, rep(1, n), col=color.list, pch="|", ylim=c(0.9, 1.1), cex=5)
	return(color.locations)
}

col_alpha <- function (acol, alpha = 0.2){
	acol <- col2rgb(acol)
	acol.red <- acol["red",]/255
	acol.green <- acol["green",]/255
	acol.blue <- acol["blue",]/255
	acol <- mapply(function(red, green, blue, alphas) rgb(red, green, blue, alphas), acol.red, acol.green, acol.blue, alpha)
	return(as.character(acol))
}

pid_maker <- function(n, reserved=""){
	my.let <- LETTERS
	my.num <- 0:9
	output <- replicate(n, paste(sample(c(my.let, my.num), 4, replace=TRUE), collapse=""))
	while(any(duplicated(output) | output %in% reserved)){
		output <- output[-which(output %in% reserved | duplicated(output))]
		remaining <- n-length(output)
		output <- c(output, replicate(remaining, paste(sample(c(my.let, my.num), 4, replace=TRUE), collapse="")))
	}
	return(output)
}

dir_init <- function(path, verbose=FALSE){
	if(substr(path, 1, 2)!='./') stop('path argument must be formatted
		with "./" at beginning')
	contents <- dir(path)
	if(verbose){
		if(length(contents)==0) print(paste('folder ', path, ' created.', sep=""))
		if(length(contents)>0) print(paste('folder ', path, ' wiped of ', length(contents), ' files/folders.', sep=""))
	}
	if(dir.exists(path)) unlink(path, recursive=TRUE)
	dir.create(path)
}

texttab <- function(input.matrix, alignment=NA, hlines=NA, caption='', scale=NA){
	output <- character(nrow(input.matrix))
	for(i in 1:nrow(input.matrix)){
		add.amps <- paste(input.matrix[i,], collapse=' & ')
		output[i] <- paste(add.amps, '\\\\', sep=' ')
	}
	if(all(!is.na(hlines))){
		for(i in 1:length(hlines)) output <- append(output, '\\hline', hlines[i]+(i-1))
	}
	return(output)
}
