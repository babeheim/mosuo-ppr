
rm(list=ls())

start.time <- Sys.time()

source('./code/project_functions.r')
source('./code/project_variables.r')

dir_init('./temp')

# honeycomb plot
	
### using the sims but with a loop

available.files <- list.files(path='./inputs',pattern='.csv', full.names=TRUE)

n.sims <- length(available.files)

parity0.n.tab <- matrix(NA, nrow=n.sims, ncol=1)
parity1.n.tab <- matrix(NA, nrow=n.sims, ncol=2)
parity2.n.tab <- matrix(NA, nrow=n.sims, ncol=3)
parity3.n.tab <- matrix(NA, nrow=n.sims, ncol=4)
parity4.n.tab <- matrix(NA, nrow=n.sims, ncol=5)
parity5.n.tab <- matrix(NA, nrow=n.sims, ncol=6)
parity6.n.tab <- matrix(NA, nrow=n.sims, ncol=7)
parity7.n.tab <- matrix(NA, nrow=n.sims, ncol=8)
parity8.n.tab <- matrix(NA, nrow=n.sims, ncol=9)
parity9.n.tab <- matrix(NA, nrow=n.sims, ncol=10)

parity0.n.tab.pat <- matrix(NA, nrow=n.sims, ncol=1)
parity1.n.tab.pat <- matrix(NA, nrow=n.sims, ncol=2)
parity2.n.tab.pat <- matrix(NA, nrow=n.sims, ncol=3)
parity3.n.tab.pat <- matrix(NA, nrow=n.sims, ncol=4)
parity4.n.tab.pat <- matrix(NA, nrow=n.sims, ncol=5)
parity5.n.tab.pat <- matrix(NA, nrow=n.sims, ncol=6)
parity6.n.tab.pat <- matrix(NA, nrow=n.sims, ncol=7)
parity7.n.tab.pat <- matrix(NA, nrow=n.sims, ncol=8)
parity8.n.tab.pat <- matrix(NA, nrow=n.sims, ncol=9)
parity9.n.tab.pat <- matrix(NA, nrow=n.sims, ncol=10)

parity0.n.tab.mat <- matrix(NA, nrow=n.sims, ncol=1)
parity1.n.tab.mat <- matrix(NA, nrow=n.sims, ncol=2)
parity2.n.tab.mat <- matrix(NA, nrow=n.sims, ncol=3)
parity3.n.tab.mat <- matrix(NA, nrow=n.sims, ncol=4)
parity4.n.tab.mat <- matrix(NA, nrow=n.sims, ncol=5)
parity5.n.tab.mat <- matrix(NA, nrow=n.sims, ncol=6)
parity6.n.tab.mat <- matrix(NA, nrow=n.sims, ncol=7)
parity7.n.tab.mat <- matrix(NA, nrow=n.sims, ncol=8)
parity8.n.tab.mat <- matrix(NA, nrow=n.sims, ncol=9)
parity9.n.tab.mat <- matrix(NA, nrow=n.sims, ncol=10)

parity0.pr.tab <- matrix(NA, nrow=n.sims, ncol=1)
parity1.pr.tab <- matrix(NA, nrow=n.sims, ncol=2)
parity2.pr.tab <- matrix(NA, nrow=n.sims, ncol=3)
parity3.pr.tab <- matrix(NA, nrow=n.sims, ncol=4)
parity4.pr.tab <- matrix(NA, nrow=n.sims, ncol=5)
parity5.pr.tab <- matrix(NA, nrow=n.sims, ncol=6)
	
parity0.pr.tab.pat <- matrix(NA, nrow=n.sims, ncol=1)
parity1.pr.tab.pat <- matrix(NA, nrow=n.sims, ncol=2)
parity2.pr.tab.pat <- matrix(NA, nrow=n.sims, ncol=3)
parity3.pr.tab.pat <- matrix(NA, nrow=n.sims, ncol=4)
parity4.pr.tab.pat <- matrix(NA, nrow=n.sims, ncol=5)
parity5.pr.tab.pat <- matrix(NA, nrow=n.sims, ncol=6)
	
parity0.pr.tab.mat <- matrix(NA, nrow=n.sims, ncol=1)
parity1.pr.tab.mat <- matrix(NA, nrow=n.sims, ncol=2)
parity2.pr.tab.mat <- matrix(NA, nrow=n.sims, ncol=3)
parity3.pr.tab.mat <- matrix(NA, nrow=n.sims, ncol=4)
parity4.pr.tab.mat <- matrix(NA, nrow=n.sims, ncol=5)
parity5.pr.tab.mat <- matrix(NA, nrow=n.sims, ncol=6)
	
for(j in 1:n.sims){

	reg <- read.csv(available.files[j], as.is=TRUE)

	## patrilineal

	mom.rows <- which(reg$male==0 & reg$age.today >= 40 & reg$patrilineal=='1')

	d <- reg[mom.rows,]
	d$pcode <- NA
	d$n.kids <- NA
	d$n.sons <- NA

	for(i in 1:nrow(d)){

	  my.kid.rows <- which(reg$m.pid==d$pid[i])
	  d$n.kids[i] <- length(my.kid.rows)
	  d$n.sons[i] <- sum(reg$male[my.kid.rows])
	  d$pcode[i] <- paste(reg$male[my.kid.rows], collapse='')

	}

	d$n.sons <- as.factor(d$n.sons)
	levels(d$n.sons) <- as.character(0:9)

	parity0.n <- sum(d$n.kids==0)
	parity1.n <- table(d$n.sons[d$n.kids==1])[1:2]
	parity2.n <- table(d$n.sons[d$n.kids==2])[1:3]
	parity3.n <- table(d$n.sons[d$n.kids==3])[1:4]
	parity4.n <- table(d$n.sons[d$n.kids==4])[1:5]
	parity5.n <- table(d$n.sons[d$n.kids==5])[1:6]
	parity6.n <- table(d$n.sons[d$n.kids==6])[1:7]
	parity7.n <- table(d$n.sons[d$n.kids==7])[1:8]
	parity8.n <- table(d$n.sons[d$n.kids==8])[1:9]
	parity9.n <- table(d$n.sons[d$n.kids==9])[1:10]


	parity0.was <- nrow(d)

	parity1.was <- c(sum(substr(d$pcode, 1, 1)=='0'), sum(substr(d$pcode, 1, 1)=='1'))

	tar <- which(d$n.kids>=2)
	parity2.was <- c(
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 2), ''), function(z) sum(as.numeric(z))==0))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 2), ''), function(z) sum(as.numeric(z))==1))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 2), ''), function(z) sum(as.numeric(z))==2))))

	tar <- which(d$n.kids>=3)
	parity3.was <- c(
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 3), ''), function(z) sum(as.numeric(z))==0))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 3), ''), function(z) sum(as.numeric(z))==1))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 3), ''), function(z) sum(as.numeric(z))==2))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 3), ''), function(z) sum(as.numeric(z))==3))))

	tar <- which(d$n.kids>=4)
	parity4.was <- c(
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 4), ''), function(z) sum(as.numeric(z))==0))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 4), ''), function(z) sum(as.numeric(z))==1))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 4), ''), function(z) sum(as.numeric(z))==2))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 4), ''), function(z) sum(as.numeric(z))==3))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 4), ''), function(z) sum(as.numeric(z))==4))))
		
	tar <- which(d$n.kids>=5)
	parity5.was <- c(
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 5), ''), function(z) sum(as.numeric(z))==0))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 5), ''), function(z) sum(as.numeric(z))==1))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 5), ''), function(z) sum(as.numeric(z))==2))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 5), ''), function(z) sum(as.numeric(z))==3))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 5), ''), function(z) sum(as.numeric(z))==4))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 5), ''), function(z) sum(as.numeric(z))==5))))

	parity0.pr <- round(parity0.n/parity0.was*100, 1)
	parity1.pr <- round(parity1.n/parity1.was*100, 1)
	parity2.pr <- round(parity2.n/parity2.was*100, 1)
	parity3.pr <- round(parity3.n/parity3.was*100, 1)
	parity4.pr <- round(parity4.n/parity4.was*100, 1)
	parity5.pr <- round(parity5.n/parity5.was*100, 1)

	parity0.n.tab.pat[j,] <- parity0.n
	parity1.n.tab.pat[j,] <- parity1.n
	parity2.n.tab.pat[j,] <- parity2.n
	parity3.n.tab.pat[j,] <- parity3.n
	parity4.n.tab.pat[j,] <- parity4.n
	parity5.n.tab.pat[j,] <- parity5.n
	parity6.n.tab.pat[j,] <- parity6.n
	parity7.n.tab.pat[j,] <- parity7.n
	parity8.n.tab.pat[j,] <- parity8.n
	parity9.n.tab.pat[j,] <- parity9.n

	parity0.pr.tab.pat[j,] <- parity0.pr
	parity1.pr.tab.pat[j,] <- parity1.pr
	parity2.pr.tab.pat[j,] <- parity2.pr
	parity3.pr.tab.pat[j,] <- parity3.pr
	parity4.pr.tab.pat[j,] <- parity4.pr
	parity5.pr.tab.pat[j,] <- parity5.pr

	## matrilineal

	mom.rows <- which(reg$male==0 & reg$age.today >= 40 & reg$patrilineal=='0')

	d <- reg[mom.rows,]
	d$pcode <- NA
	d$n.kids <- NA
	d$n.sons <- NA

	for(i in 1:nrow(d)){

	  my.kid.rows <- which(reg$m.pid==d$pid[i])
	  d$n.kids[i] <- length(my.kid.rows)
	  d$n.sons[i] <- sum(reg$male[my.kid.rows])
	  d$pcode[i] <- paste(reg$male[my.kid.rows], collapse='')

	}

	d$n.sons <- as.factor(d$n.sons)
	levels(d$n.sons) <- as.character(0:9)

	parity0.n <- sum(d$n.kids==0)
	parity1.n <- table(d$n.sons[d$n.kids==1])[1:2]
	parity2.n <- table(d$n.sons[d$n.kids==2])[1:3]
	parity3.n <- table(d$n.sons[d$n.kids==3])[1:4]
	parity4.n <- table(d$n.sons[d$n.kids==4])[1:5]
	parity5.n <- table(d$n.sons[d$n.kids==5])[1:6]
	parity6.n <- table(d$n.sons[d$n.kids==6])[1:7]
	parity7.n <- table(d$n.sons[d$n.kids==7])[1:8]
	parity8.n <- table(d$n.sons[d$n.kids==8])[1:9]
	parity9.n <- table(d$n.sons[d$n.kids==9])[1:10]


	parity0.was <- nrow(d)

	parity1.was <- c(sum(substr(d$pcode, 1, 1)=='0'), sum(substr(d$pcode, 1, 1)=='1'))

	tar <- which(d$n.kids>=2)
	parity2.was <- c(
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 2), ''), function(z) sum(as.numeric(z))==0))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 2), ''), function(z) sum(as.numeric(z))==1))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 2), ''), function(z) sum(as.numeric(z))==2))))

	tar <- which(d$n.kids>=3)
	parity3.was <- c(
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 3), ''), function(z) sum(as.numeric(z))==0))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 3), ''), function(z) sum(as.numeric(z))==1))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 3), ''), function(z) sum(as.numeric(z))==2))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 3), ''), function(z) sum(as.numeric(z))==3))))

	tar <- which(d$n.kids>=4)
	parity4.was <- c(
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 4), ''), function(z) sum(as.numeric(z))==0))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 4), ''), function(z) sum(as.numeric(z))==1))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 4), ''), function(z) sum(as.numeric(z))==2))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 4), ''), function(z) sum(as.numeric(z))==3))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 4), ''), function(z) sum(as.numeric(z))==4))))

	tar <- which(d$n.kids>=5)
	parity5.was <- c(
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 5), ''), function(z) sum(as.numeric(z))==0))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 5), ''), function(z) sum(as.numeric(z))==1))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 5), ''), function(z) sum(as.numeric(z))==2))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 5), ''), function(z) sum(as.numeric(z))==3))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 5), ''), function(z) sum(as.numeric(z))==4))),
	sum(as.numeric(lapply(strsplit(substr(d$pcode[tar], 1, 5), ''), function(z) sum(as.numeric(z))==5))))
		
	parity0.pr <- round(parity0.n/parity0.was*100, 1)
	parity1.pr <- round(parity1.n/parity1.was*100, 1)
	parity2.pr <- round(parity2.n/parity2.was*100, 1)
	parity3.pr <- round(parity3.n/parity3.was*100, 1)
	parity4.pr <- round(parity4.n/parity4.was*100, 1)
	parity5.pr <- round(parity5.n/parity5.was*100, 1)

	parity0.n.tab.mat[j,] <- parity0.n
	parity1.n.tab.mat[j,] <- parity1.n
	parity2.n.tab.mat[j,] <- parity2.n
	parity3.n.tab.mat[j,] <- parity3.n
	parity4.n.tab.mat[j,] <- parity4.n
	parity5.n.tab.mat[j,] <- parity5.n
	parity6.n.tab.mat[j,] <- parity6.n
	parity7.n.tab.mat[j,] <- parity7.n
	parity8.n.tab.mat[j,] <- parity8.n
	parity9.n.tab.mat[j,] <- parity9.n

	parity0.pr.tab.mat[j,] <- parity0.pr
	parity1.pr.tab.mat[j,] <- parity1.pr
	parity2.pr.tab.mat[j,] <- parity2.pr
	parity3.pr.tab.mat[j,] <- parity3.pr
	parity4.pr.tab.mat[j,] <- parity4.pr
	parity5.pr.tab.mat[j,] <- parity5.pr
		
	# if(j %% 100==0) print(j)

}


# now take the averages and plot

pdf(file='./temp/honeycomb_revised.pdf', height=3.7, width=2*3.7)
	
par(mar=c(1, 1, 1, 1))

par(mfrow=c(1,2))

# patrilineal honeycomb

parity0.n <- round(colMeans(parity0.n.tab.pat),0)
parity1.n <- round(colMeans(parity1.n.tab.pat),0)
parity2.n <- round(colMeans(parity2.n.tab.pat),0)
parity3.n <- round(colMeans(parity3.n.tab.pat),0)
	
parity0.pr <- 100-round(colMeans(parity0.pr.tab.pat),1)
parity1.pr <- 100-round(colMeans(parity1.pr.tab.pat),1)
parity2.pr <- 100-round(colMeans(parity2.pr.tab.pat),1)
parity3.pr <- round(100-round(colMeans(parity3.pr.tab.pat, na.rm=TRUE),1), 1)

parity0.pr.sd <- round(apply(parity0.pr.tab.pat, 2, sd),1)
parity1.pr.sd <- round(apply(parity1.pr.tab.pat, 2, sd),1)
parity2.pr.sd <- round(apply(parity2.pr.tab.pat, 2, sd),1)
parity3.pr.sd <- round(apply(parity3.pr.tab.pat, 2, sd),1)
		
total.n <- max(parity0.n, parity1.n, parity2.n, parity3.n, parity4.n, parity5.n)
	
# conditional on ending in parity 2, proportional difference

# directly scaling with the number, so the unconditional proportion who end up there
parity0.frq <- (parity0.pr/100)
parity1.frq <- (parity1.pr/100)
parity2.frq <- (parity2.pr/100)
parity3.frq <- (parity3.pr/100)

hex <- function(x, y, col, ...) for(i in 1:length(x)) polygon(c(x[i], x[i]-sqrt(3)/2, x[i]-sqrt(3)/2, x[i], x[i]+sqrt(3)/2, x[i]+sqrt(3)/2), c(y[i]-1, y[i]-1/2, y[i]+1/2, y[i]+1, y[i]+1/2, y[i]-1/2), col=col[i], ...)

parity0.cols <- col.alpha('darkorange2', parity0.frq)
parity1.cols <- col.alpha('darkorange2', parity1.frq)
parity2.cols <- col.alpha('darkorange2', parity2.frq)
parity3.cols <- col.alpha('darkorange2', parity3.frq)

plot(NA, ylim=c(-7,1), xlim=c(-4,4), frame.plot=FALSE, 
	axes=FALSE, ylab='', xlab='', main='patrilineal')

hex(-sqrt(3)*c(0), 0, col=parity0.cols, border='black')
hex(-sqrt(3)*c(-0.5, 0.5), rep(-1.5, 2), col=parity1.cols, border='black')
hex(-sqrt(3)*c(-1, 0, 1), rep(-3, 3), col=parity2.cols, border='black')
hex(-sqrt(3)*c(-1.5, -0.5, 0.5, 1.5), rep(-4.5, 4), col=parity3.cols, border='black')

parity0.pr.txt <- paste(parity0.pr, '\n± ', parity0.pr.sd, sep='')
parity1.pr.txt <- paste(parity1.pr, '\n± ', parity1.pr.sd, sep='')
parity2.pr.txt <- paste(parity2.pr, '\n± ', parity2.pr.sd, sep='')
parity3.pr.txt <- paste(parity3.pr, '\n± ', parity3.pr.sd, sep='')
	
text(0, 0, parity0.pr.txt, col='black')
text(-sqrt(3)*c(-0.5, 0.5), rep(-1.5, 2), parity1.pr.txt, col='black')
text(-sqrt(3)*c(-1, 0, 1), rep(-3, 3), parity2.pr.txt, col='black')
text(-sqrt(3)*c(-1.5, -0.5, 0.5, 1.5), rep(-4.5, 4), parity3.pr.txt, col='black')

text(sqrt(3)*1.5, -5.8, 'all girls')
text(-sqrt(3)*1.5, -5.8, 'all boys')

n <- 1000
cols <- c('darkorange2', 'white')
colfunc <- colorRampPalette(cols)
color.list <- colfunc(n)
left.end <- -1.5
right.end <- 1.5
line.height <- -6.3
points(seq(right.end,left.end,length.out=n), rep(line.height+(.1), n), col=color.list, pch=20)
lines(c(left.end, right.end), c(line.height, line.height)) 
lines(c(left.end, left.end), c(line.height, line.height-(.2)))
lines(c(right.end, right.end), c(line.height, line.height-(.2)))
lines(c(0,0), c(line.height, line.height-(.2)))
text(left.end, line.height-(.4), '0')
text(0, line.height-(.4), '')
text(right.end, line.height-(.4), '1')
text(0, line.height-(.8), 'parity progression probability')
	
my.x0 <- 1.69
my.y0 <- 0.09
arrows(x0=my.x0, y0=my.y0, x1=my.x0+sqrt(3)/2, y1=my.y0-3/2, length=0.2, angle=15)
arrows(x0=-my.x0, y0=my.y0, x1=-my.x0-sqrt(3)/2, y1=my.y0-3/2, length=0.2, angle=15)

text(1.91, 0.34, '+girl')
text(-1.91, 0.34, '+boy')
	
# matrilineal honeycomb

parity0.n <- round(colMeans(parity0.n.tab.mat),0)
parity1.n <- round(colMeans(parity1.n.tab.mat),0)
parity2.n <- round(colMeans(parity2.n.tab.mat),0)
parity3.n <- round(colMeans(parity3.n.tab.mat),0)

parity0.pr <- 100-round(colMeans(parity0.pr.tab.mat),1)
parity1.pr <- 100-round(colMeans(parity1.pr.tab.mat),1)
parity2.pr <- 100-round(colMeans(parity2.pr.tab.mat),1)
parity3.pr <- round(100-round(colMeans(parity3.pr.tab.mat, na.rm=TRUE),1), 1)
	
parity0.pr.sd <- round(apply(parity0.pr.tab.mat, 2, sd),1)
parity1.pr.sd <- round(apply(parity1.pr.tab.mat, 2, sd),1)
parity2.pr.sd <- round(apply(parity2.pr.tab.mat, 2, sd),1)
parity3.pr.sd <- round(apply(parity3.pr.tab.mat, 2, sd),1)
		
total.n <- max(parity0.n, parity1.n, parity2.n, parity3.n, parity4.n, parity5.n)
		
# directly scaling with the number, so the unconditional proportion who end up there
parity0.frq <- (parity0.pr/100)
parity1.frq <- (parity1.pr/100)
parity2.frq <- (parity2.pr/100)
parity3.frq <- (parity3.pr/100)
	
hex <- function(x, y, col, ...) for(i in 1:length(x)) polygon(c(x[i], x[i]-sqrt(3)/2, x[i]-sqrt(3)/2, x[i], x[i]+sqrt(3)/2, x[i]+sqrt(3)/2), c(y[i]-1, y[i]-1/2, y[i]+1/2, y[i]+1, y[i]+1/2, y[i]-1/2), col=col[i], ...)

my.cols <- names(gradient_maker(0, 1, cols=c('darkorange2', 'steelblue'), n=8))

parity0.cols <- col.alpha('steelblue', parity0.frq)
parity1.cols <- col.alpha('steelblue', parity1.frq)
parity2.cols <- col.alpha('steelblue', parity2.frq)
parity3.cols <- col.alpha('steelblue', parity3.frq)

plot(NA, ylim=c(-7,1), xlim=c(-4,4), frame.plot=FALSE, 
	axes=FALSE, ylab='', xlab='', main='matrilineal')

hex(-sqrt(3)*c(0), 0, col=parity0.cols, border='black')
hex(-sqrt(3)*c(-0.5, 0.5), rep(-1.5, 2), col=parity1.cols, border='black')
hex(-sqrt(3)*c(-1, 0, 1), rep(-3, 3), col=parity2.cols, border='black')
hex(-sqrt(3)*c(-1.5, -0.5, 0.5, 1.5), rep(-4.5, 4), col=parity3.cols, border='black')

parity0.pr.txt <- paste(parity0.pr, '\n± ', parity0.pr.sd, sep='')
parity1.pr.txt <- paste(parity1.pr, '\n± ', parity1.pr.sd, sep='')
parity2.pr.txt <- paste(parity2.pr, '\n± ', parity2.pr.sd, sep='')
parity3.pr.txt <- paste(parity3.pr, '\n± ', parity3.pr.sd, sep='')
	
text(0, 0, parity0.pr.txt, col='black')
text(-sqrt(3)*c(-0.5, 0.5), rep(-1.5, 2), parity1.pr.txt, col='black')
text(-sqrt(3)*c(-1, 0, 1), rep(-3, 3), parity2.pr.txt, col='black')
text(-sqrt(3)*c(-1.5, -0.5, 0.5, 1.5), rep(-4.5, 4), parity3.pr.txt, col='black')

text(sqrt(3)*1.5, -5.8, 'all girls')
text(-sqrt(3)*1.5, -5.8, 'all boys')

n <- 1000
cols <- c('steelblue', 'white')
colfunc <- colorRampPalette(cols)
color.list <- colfunc(n)
left.end <- -1.5
right.end <- 1.5
line.height <- -6.3
points(seq(right.end,left.end,length.out=n), rep(line.height+(.1), n), col=color.list, pch=20)
lines(c(left.end, right.end), c(line.height, line.height)) 
lines(c(left.end, left.end), c(line.height, line.height-(.2)))
lines(c(right.end, right.end), c(line.height, line.height-(.2)))
lines(c(0,0), c(line.height, line.height-(.2)))
text(left.end, line.height-(.4), '0')
text(0, line.height-(.4), '')
text(right.end, line.height-(.4), '1')
text(0, line.height-(.8), 'parity progression probability')
	
my.x0 <- 1.69
my.y0 <- 0.09
arrows(x0=my.x0, y0=my.y0, x1=my.x0+sqrt(3)/2, y1=my.y0-3/2, length=0.2, angle=15)
arrows(x0=-my.x0, y0=my.y0, x1=-my.x0-sqrt(3)/2, y1=my.y0-3/2, length=0.2, angle=15)

text(1.91, 0.34, '+girl')
text(-1.91, 0.34, '+boy')
	
dev.off()
	
stop.time <- Sys.time()

cat(task_timer('generate honeycomb figure'), file='./temp/exploresimulation_log.txt')

# move all temp to output
dir_init('./output')
files <- list.files('./temp', full.names=TRUE)
file.copy(files, './output')

# housekeeping - delete temp
if(!save_temp) unlink('./temp', recursive=TRUE)
