
if (scaffold) {
  rm(list = ls())
  source("../project_support.r")
}

dir_init("./temp")

load("./inputs/parity1_re.robj")

extract_rethinking_table <- function(model){
  if(.hasSlot(precis(model), ".S3Class")){
    output <- as.data.frame(precis(model))
    colnames(output) <- c("Mean", "SE", "lower",
      "upper", "n_eff", "Rhat")
  } else {
    output <- precis(model)@output
  }
  return(output)
}

# old/new precis bug

output <- extract_rethinking_table(m2re)

rownames(output) <- gsub("a_mu", "Intercept", rownames(output))
rownames(output) <- gsub("a_sigma", "Mother Random Effect", rownames(output))
rownames(output) <- gsub("b2", "Patrilineal", rownames(output))
rownames(output) <- gsub("b3", "Son x Patrilineal", rownames(output))
rownames(output) <- gsub("b4", "Age 15-19", rownames(output))
rownames(output) <- gsub("b6", "Cohort 1930-1939", rownames(output))
rownames(output) <- gsub("b7", "Cohort 1940-1949", rownames(output))
rownames(output) <- gsub("b8", "Cohort 1950-1959", rownames(output))
rownames(output) <- gsub("b9", "Cohort 1970-1979", rownames(output))
rownames(output) <- gsub("b10", "Cohort 1920-1929", rownames(output))
rownames(output) <- gsub("b11", "Cohort 1980-1989", rownames(output))
rownames(output) <- gsub("b12", "Market Integrated Job", rownames(output))
rownames(output) <- gsub("b13", "Highest Grade Level", rownames(output))
rownames(output) <- gsub("b14", "Birth Last Year", rownames(output))
rownames(output) <- gsub("b15", "Age 25-29", rownames(output))
rownames(output) <- gsub("b16", "Age 30-34", rownames(output))
rownames(output) <- gsub("b17", "Age 35-39", rownames(output))
rownames(output) <- gsub("b18", "Age 40+", rownames(output))
rownames(output) <- gsub("b1", "Son", rownames(output))
  
variable.order <- c("Intercept", "Son", "Son&Daughter", "Son&Son", 
  "Patrilineal", "Age 15-19", "Age 20-24", "Age 25-29", "Age 30-34", 
  "Age 35-39", "Age 40+", "Cohort 1920-1929", "Cohort 1930-1939", 
  "Cohort 1940-1949", "Cohort 1950-1959", "Cohort 1960-1969", 
  "Cohort 1970-1979", "Cohort 1980-1989", "Market Integrated Job", 
  "Highest Grade Level", "Birth Last Year", "Son x Patrilineal", 
  "Son&Daughter x Patrilineal", "Son&Son x Patrilineal", "Mother Random Effect")
  
output <- output[variable.order,]
  
output$mean <- as.character(round(output$Mean, 2))
output$ci <- paste("(", sprintf("%.2f", output$lower), ", ", 
  sprintf("%.2f", output$upper), ")", sep="")

rownames(output) <- variable.order

output$mean[is.na(output$mean)] <- ""
output$ci[grep("NA", output$ci)] <- ""

parity1_output <- output[,c("mean", "ci")]

post2 <- extract.samples(m2re)
  
pr.0M.chain <- logistic(post2$a_mu + post2$b1 * 0 +
  post2$b2 * 0 + post2$b3 * 0 + post2$b9)
pr.0P.chain <- logistic(post2$a_mu + post2$b1 * 0 +
   post2$b2 * 1 + post2$b3 * 0 + post2$b9)
pr.1M.chain <- logistic(post2$a_mu + post2$b1 * 1 +
   post2$b2 * 0 + post2$b3 * 0 + post2$b9)
pr.1P.chain <- logistic(post2$a_mu + post2$b1 * 1 +
   post2$b2 * 1 + post2$b3 * 1 + post2$b9)
  
pr.0M.mean <- mean(pr.0M.chain)
pr.0P.mean <- mean(pr.0P.chain)
pr.1M.mean <- mean(pr.1M.chain)
pr.1P.mean <- mean(pr.1P.chain)

pr.0M.lb <- HPDI(pr.0M.chain)[1]
pr.0P.lb <- HPDI(pr.0P.chain)[1]
pr.1M.lb <- HPDI(pr.1M.chain)[1]
pr.1P.lb <- HPDI(pr.1P.chain)[1]

pr.0M.ub <- HPDI(pr.0M.chain)[2]
pr.0P.ub <- HPDI(pr.0P.chain)[2]
pr.1M.ub <- HPDI(pr.1M.chain)[2]
pr.1P.ub <- HPDI(pr.1P.chain)[2]

# parity 1 figure:
  
pdf("./temp/parity1.pdf", height=8, width=10)

plot(1:6, ylim=c(0,0.5), ylab="Annual Probability of Birth", 
  xlab="Sex of First Child", xaxt="n", col=NULL, xlim=c(0, 7),
  frame.plot=FALSE, las=1)

abline(h=0:10/10, col=gray(0.8))

polygon(c(0.7, 0.7, 1.3, 1.3), c(0, pr.0M.mean, pr.0M.mean, 0),
  col="steelblue", border=NA)
polygon(c(1.7, 1.7, 2.3, 2.3), c(0, pr.1M.mean, pr.1M.mean, 0),
  col="steelblue", border=NA)

polygon(c(4.7, 4.7, 5.3, 5.3), c(0, pr.0P.mean, pr.0P.mean, 0),
  col="sandybrown", border=NA)
polygon(c(5.7, 5.7, 6.3, 6.3), c(0, pr.1P.mean, pr.1P.mean, 0),
  col="sandybrown", border=NA)

points(c(1, 2, 5, 6), c(pr.0M.mean, pr.1M.mean, pr.0P.mean, pr.1P.mean), pch=20)
lines(c(1, 1), c(pr.0M.lb, pr.0M.ub))
lines(c(2, 2), c(pr.1M.lb, pr.1M.ub))
lines(c(5, 5), c(pr.0P.lb, pr.0P.ub))
lines(c(6, 6), c(pr.1P.lb, pr.1P.ub))

axis(1, at=c(1, 2), labels=c("Daughter", "Son"))

axis(1, at=c(5, 6), labels=c("Daughter", "Son"))

text(1.3, 0.4, "Matrilineal\nMosuo", col="steelblue", cex=1.3)

text(6.3, 0.4, "Patrilineal\nMosuo", col="sandybrown", cex=1.3)
  
dev.off()

  
  
# b1 pcode10
# b2 pcode11
# b3 patri
# b4 pat_10
# b5 pat_11

load("./inputs/parity2_re.robj")  
  
output <- extract_rethinking_table(m3re)
  
rownames(output) <- gsub("a_mu", "Intercept", rownames(output))
rownames(output) <- gsub("a_sigma", "Mother Random Effect", rownames(output))
rownames(output) <- gsub("b2", "Son&Son", rownames(output))
rownames(output) <- gsub("b3", "Patrilineal", rownames(output))
rownames(output) <- gsub("b4", "Son&Daughter x Patrilineal", rownames(output))
rownames(output) <- gsub("b5", "Son&Son x Patrilineal", rownames(output))
rownames(output) <- gsub("b8", "Cohort 1930-1939", rownames(output))
rownames(output) <- gsub("b9", "Cohort 1940-1949", rownames(output))
rownames(output) <- gsub("b10", "Cohort 1950-1959", rownames(output))
rownames(output) <- gsub("b11", "Cohort 1970-1979", rownames(output))
rownames(output) <- gsub("b12", "Cohort 1920-1929", rownames(output))
rownames(output) <- gsub("b14", "Market Integrated Job", rownames(output))
rownames(output) <- gsub("b15", "Highest Grade Level", rownames(output))
rownames(output) <- gsub("b16", "Birth Last Year", rownames(output))
rownames(output) <- gsub("b6", "Age 15-19", rownames(output))
rownames(output) <- gsub("b7", "Age 20-24", rownames(output))
rownames(output) <- gsub("b17", "Age 30-34", rownames(output))
rownames(output) <- gsub("b18", "Age 35-39", rownames(output))
rownames(output) <- gsub("b19", "Age 40+", rownames(output))
rownames(output) <- gsub("b1", "Son&Daughter", rownames(output))
  
variable.order <- c("Intercept", "Son", "Son&Daughter", "Son&Son", "Patrilineal", 
  "Age 15-19", "Age 20-24", "Age 25-29", "Age 30-34", "Age 35-39", "Age 40+", 
  "Cohort 1920-1929", "Cohort 1930-1939", "Cohort 1940-1949", "Cohort 1950-1959", 
  "Cohort 1960-1969", "Cohort 1970-1979", "Cohort 1980-1989", "Market Integrated Job", 
  "Highest Grade Level", "Birth Last Year", "Son x Patrilineal", 
  "Son&Daughter x Patrilineal", "Son&Son x Patrilineal", "Mother Random Effect")
  
output <- output[variable.order,]
  
output$mean <- round(output$Mean, 2)
output$ci <- paste("(", sprintf("%.2f", output$lower), ", ", 
  sprintf("%.2f", output$upper), ")", sep="")

rownames(output) <- variable.order

output$mean[is.na(output$mean)] <- ""
output$ci[grep("NA", output$ci)] <- ""

parity2_output <- output[,c("mean", "ci")]

output <- cbind(parity1_output, parity2_output)

write.csv(output, "./temp/parity1_2.csv", row.names=TRUE)

output <- cbind(rownames(output), output)
output[,1] <- as.character(output[,1])

output <- rbind(c("", "Est.", "89\\% HPDI", "Est.", "89\\% HPDI"), output)

output.tex <- texttab(output)

writeLines(output.tex, "./temp/parity1_2.txt")

  
  
post3 <- extract.samples(m3re)
  
pr.00M.chain <- logistic(post3$a_mu + post3$b1 * 0 + post3$b2 * 0 +
  post3$b3 * 0 + post3$b4 * 0 + post3$b5 * 0 + post3$b11)
pr.00P.chain <- logistic(post3$a_mu + post3$b1 * 0 + post3$b2 * 0 +
  post3$b3 * 1 + post3$b4 * 0 + post3$b5 * 0 + post3$b11)
pr.10M.chain <- logistic(post3$a_mu + post3$b1 * 1 + post3$b2 * 0 +
  post3$b3 * 0 + post3$b4 * 0 + post3$b5 * 0 + post3$b11)
pr.10P.chain <- logistic(post3$a_mu + post3$b1 * 1 + post3$b2 * 0 +
  post3$b3 * 1 + post3$b4 * 1 + post3$b5 * 0 + post3$b11)
pr.11M.chain <- logistic(post3$a_mu + post3$b1 * 0 + post3$b2 * 1 +
  post3$b3 * 0 + post3$b4 * 0 + post3$b5 * 0 + post3$b11)
pr.11P.chain <- logistic(post3$a_mu + post3$b1 * 0 + post3$b2 * 1 +
  post3$b3 * 1 + post3$b4 * 0 + post3$b5 * 1 + post3$b11)
  
pr.00M.mean <- mean(pr.00M.chain)
pr.00P.mean <- mean(pr.00P.chain)
pr.10M.mean <- mean(pr.10M.chain)
pr.10P.mean <- mean(pr.10P.chain)
pr.11M.mean <- mean(pr.11M.chain)
pr.11P.mean <- mean(pr.11P.chain)

pr.00M.lb <- HPDI(pr.00M.chain)[1]
pr.00P.lb <- HPDI(pr.00P.chain)[1]
pr.10M.lb <- HPDI(pr.10M.chain)[1]
pr.10P.lb <- HPDI(pr.10P.chain)[1]
pr.11M.lb <- HPDI(pr.11M.chain)[1]
pr.11P.lb <- HPDI(pr.11P.chain)[1]

pr.00M.ub <- HPDI(pr.00M.chain)[2]
pr.00P.ub <- HPDI(pr.00P.chain)[2]
pr.10M.ub <- HPDI(pr.10M.chain)[2]
pr.10P.ub <- HPDI(pr.10P.chain)[2]
pr.11M.ub <- HPDI(pr.11M.chain)[2]
pr.11P.ub <- HPDI(pr.11P.chain)[2]
  
  
# these look like ass...how are we going to do this?
# a first step: recenter age and cohort, that might cause the models to change...
# possibly need to change again

# parity 2 figure:
  
pdf("./temp/parity1_2.pdf", height=8, width=20)
  
par(mfrow=c(1,2))
  
plot(1:6, ylim=c(0,0.5), ylab="Annual Probability of Birth",
  xlab="Sex of First Child", xaxt="n", col=NULL, xlim=c(0, 7), frame.plot=FALSE, las=1)

abline(h=0:10/10, col=gray(0.8))

polygon(c(0.7, 0.7, 1.3, 1.3), c(0, pr.0M.mean, pr.0M.mean, 0),
   col="steelblue", border=NA)
polygon(c(1.7, 1.7, 2.3, 2.3), c(0, pr.1M.mean, pr.1M.mean, 0),
   col="steelblue", border=NA)

polygon(c(4.7, 4.7, 5.3, 5.3), c(0, pr.0P.mean, pr.0P.mean, 0),
   col="sandybrown", border=NA)
polygon(c(5.7, 5.7, 6.3, 6.3), c(0, pr.1P.mean, pr.1P.mean, 0),
   col="sandybrown", border=NA)

points(c(1, 2, 5, 6), c(pr.0M.mean, pr.1M.mean, pr.0P.mean, pr.1P.mean), pch=20)
lines(c(1, 1), c(pr.0M.lb, pr.0M.ub))
lines(c(2, 2), c(pr.1M.lb, pr.1M.ub))
lines(c(5, 5), c(pr.0P.lb, pr.0P.ub))
lines(c(6, 6), c(pr.1P.lb, pr.1P.ub))

axis(1, at=c(1, 2), labels=c("Daughter", "Son"))

axis(1, at=c(5, 6), labels=c("Daughter", "Son"))

text(1.3, 0.4, "Matrilineal\nMosuo", col="steelblue", cex=1.3)

text(6.3, 0.4, "Patrilineal\nMosuo", col="sandybrown", cex=1.3)
  
  

plot(1:8, ylim=c(0,0.05), ylab="Annual Probability of Birth", 
  xlab="Sex Combination at Parity 2", xaxt="n", col=NULL, xlim=c(0, 9),
  frame.plot=FALSE, las=1)

abline(h=0:10/10, col=gray(0.8))

polygon(c(0.7, 0.7, 1.3, 1.3), c(0, pr.00M.mean, pr.00M.mean, 0),
  col="steelblue", border=NA)
polygon(c(1.7, 1.7, 2.3, 2.3), c(0, pr.10M.mean, pr.10M.mean, 0),
  col="steelblue", border=NA)
polygon(c(2.7, 2.7, 3.3, 3.3), c(0, pr.11M.mean, pr.11M.mean, 0),
  col="steelblue", border=NA)

polygon(c(5.7, 5.7, 6.3, 6.3), c(0, pr.00P.mean, pr.00P.mean, 0),
  col="sandybrown", border=NA)
polygon(c(6.7, 6.7, 7.3, 7.3), c(0, pr.10P.mean, pr.10P.mean, 0),
  col="sandybrown", border=NA)
polygon(c(7.7, 7.7, 8.3, 8.3), c(0, pr.11P.mean, pr.11P.mean, 0),
  col="sandybrown", border=NA)

points(c(1, 2, 3, 6, 7, 8), c(pr.00M.mean, pr.10M.mean, pr.11M.mean, 
  pr.00P.mean, pr.10P.mean, pr.11P.mean), pch=20)

lines(c(1, 1), c(pr.00M.lb, pr.00M.ub))
lines(c(2, 2), c(pr.10M.lb, pr.10M.ub))
lines(c(3, 3), c(pr.11M.lb, pr.11M.ub))
  
lines(c(6, 6), c(pr.00P.lb, pr.00P.ub))
lines(c(7, 7), c(pr.10P.lb, pr.10P.ub))
lines(c(8, 8), c(pr.11P.lb, pr.11P.ub))

axis(1, at=c(1, 2, 3, 6, 7, 8), labels=c("D/D", "D/S", "S/S", "D/D", "D/S", "S/S"))

text(2, 0.25, "Matrilineal\nMosuo", col="steelblue", cex=1.3)
  
text(7.5, 0.25, "Patrilineal\nMosuo", col="sandybrown", cex=1.3)
  
dev.off()
  
  
load("./inputs/parity2_pooled1_re.robj") # model m4re

output <- extract_rethinking_table(m4re)
  
rownames(output) <- gsub("a_mu", "Intercept", rownames(output))
rownames(output) <- gsub("a_sigma", "Mother Random Effect", rownames(output))
rownames(output) <- gsub("b2", "Patrilineal", rownames(output))
rownames(output) <- gsub("b3", "No Lineal Children x Patrilineal", rownames(output))
rownames(output) <- gsub("b4", "Age 15-19", rownames(output))
rownames(output) <- gsub("b5", "Age 20-24", rownames(output))
rownames(output) <- gsub("b6", "Cohort 1930-1939", rownames(output))
rownames(output) <- gsub("b7", "Cohort 1940-1949", rownames(output))
rownames(output) <- gsub("b8", "Cohort 1950-1959", rownames(output))
rownames(output) <- gsub("b9", "Cohort 1970-1979", rownames(output))
rownames(output) <- gsub("b10", "Cohort 1920-1929", rownames(output))
rownames(output) <- gsub("b12", "Market Integrated Job", rownames(output))
rownames(output) <- gsub("b13", "Highest Grade Level", rownames(output))
rownames(output) <- gsub("b14", "Birth Last Year", rownames(output))
rownames(output) <- gsub("b15", "Age 30-34", rownames(output))
rownames(output) <- gsub("b16", "Age 35-39", rownames(output))
rownames(output) <- gsub("b17", "Age 40+", rownames(output))
rownames(output) <- gsub("b1", "No Lineal Children", rownames(output))
  
variable.order <- c("Intercept", "No Lineal Children", "Balanced Sex Ratio", 
  "Patrilineal", "Age 15-19", "Age 20-24", "Age 25-29", "Age 30-34", "Age 35-39", 
  "Age 40+", "Cohort 1920-1929", "Cohort 1930-1939", "Cohort 1940-1949", 
  "Cohort 1950-1959", "Cohort 1960-1969", "Cohort 1970-1979", "Cohort 1980-1989", 
  "Market Integrated Job", "Highest Grade Level", "Birth Last Year", 
  "No Lineal Children x Patrilineal", "Balanced Sex Ratio x Patrilineal", 
  "Mother Random Effect")
  
output <- output[variable.order,]
  
output$mean <- round(output$Mean, 2)
output$ci <- paste("(", sprintf("%.2f", output$lower), ", ", 
  sprintf("%.2f", output$upper), ")", sep="")

rownames(output) <- variable.order

output$mean[is.na(output$mean)] <- ""
output$ci[grep("NA", output$ci)] <- ""

parity2_rp1_output <- output[,c("mean", "ci")]

post4 <- extract.samples(m4re)
  
pr.lineal.mat.chain <- logistic(post4$a_mu + post4$b1 * 0 + post4$b2 * 0 +
  post4$b3 * 0 + post4$b9)
pr.nolineal.mat.chain <- logistic(post4$a_mu + post4$b1 * 1 + post4$b2 * 0 +
  post4$b3 * 0 + post4$b9)
pr.lineal.pat.chain <- logistic(post4$a_mu + post4$b1 * 0 + post4$b2 * 1 +
  post4$b3 * 0 + post4$b9)
pr.nolineal.pat.chain <- logistic(post4$a_mu + post4$b1 * 1 + post4$b2 * 1 +
  post4$b3 * 1 + post4$b9)
  
pr.lineal.mat.mean <- mean(pr.lineal.mat.chain)
pr.nolineal.mat.mean <- mean(pr.nolineal.mat.chain)
pr.lineal.pat.mean <- mean(pr.lineal.pat.chain)
pr.nolineal.pat.mean <- mean(pr.nolineal.pat.chain)

pr.lineal.mat.lb <- HPDI(pr.lineal.mat.chain)[1]
pr.nolineal.mat.lb <- HPDI(pr.nolineal.mat.chain)[1]
pr.lineal.pat.lb <- HPDI(pr.lineal.pat.chain)[1]
pr.nolineal.pat.lb <- HPDI(pr.nolineal.pat.chain)[1]
  
pr.lineal.mat.ub <- HPDI(pr.lineal.mat.chain)[2]
pr.nolineal.mat.ub <- HPDI(pr.nolineal.mat.chain)[2]
pr.lineal.pat.ub <- HPDI(pr.lineal.pat.chain)[2]
pr.nolineal.pat.ub <- HPDI(pr.nolineal.pat.chain)[2]
  
  
load("./inputs/parity2_pooled2_re.robj") # model m5re
  
output <- extract_rethinking_table(m5re)
  
rownames(output) <- gsub("a_mu", "Intercept", rownames(output))
rownames(output) <- gsub("a_sigma", "Mother Random Effect", rownames(output))
rownames(output) <- gsub("b2", "Patrilineal", rownames(output))
rownames(output) <- gsub("b3", "Balanced Sex Ratio x Patrilineal", rownames(output))
rownames(output) <- gsub("b4", "Age 15-19", rownames(output))
rownames(output) <- gsub("b5", "Age 20-24", rownames(output))
rownames(output) <- gsub("b6", "Cohort 1930-1939", rownames(output))
rownames(output) <- gsub("b7", "Cohort 1940-1949", rownames(output))
rownames(output) <- gsub("b8", "Cohort 1950-1959", rownames(output))
rownames(output) <- gsub("b9", "Cohort 1970-1979", rownames(output))
rownames(output) <- gsub("b10", "Cohort 1920-1929", rownames(output))
rownames(output) <- gsub("b12", "Market Integrated Job", rownames(output))
rownames(output) <- gsub("b13", "Highest Grade Level", rownames(output))
rownames(output) <- gsub("b14", "Birth Last Year", rownames(output))
rownames(output) <- gsub("b15", "Age 30-34", rownames(output))
rownames(output) <- gsub("b16", "Age 35-39", rownames(output))
rownames(output) <- gsub("b17", "Age 40+", rownames(output))
rownames(output) <- gsub("b1", "Balanced Sex Ratio", rownames(output))
  
variable.order <- c("Intercept", "No Lineal Children", "Balanced Sex Ratio", 
  "Patrilineal", "Age 15-19", "Age 20-24", "Age 25-29", "Age 30-34", "Age 35-39",
   "Age 40+", "Cohort 1920-1929", "Cohort 1930-1939", "Cohort 1940-1949", 
   "Cohort 1950-1959", "Cohort 1960-1969", "Cohort 1970-1979", "Cohort 1980-1989", 
   "Market Integrated Job", "Highest Grade Level", "Birth Last Year", 
   "No Lineal Children x Patrilineal", "Balanced Sex Ratio x Patrilineal", 
   "Mother Random Effect")
  
output <- output[variable.order,]
  
output$mean <- round(output$Mean, 2)
output$ci <- paste("(", sprintf("%.2f", output$lower), ", ", 
  sprintf("%.2f", output$upper), ")", sep="")

rownames(output) <- variable.order

output$mean[is.na(output$mean)] <- ""
output$ci[grep("NA", output$ci)] <- ""

parity2_rp2_output <- output[,c("mean", "ci")]

output <- cbind(parity2_rp1_output, parity2_rp2_output)

write.csv(output, "./temp/parity2_rp1_rp2.csv", row.names=TRUE)

output <- cbind(rownames(output), output)
output[,1] <- as.character(output[,1])

output <- rbind(c("", "Est.", "89\\% HPDI", "Est.", "89\\% HPDI"), output)

output.tex <- texttab(output)

writeLines(output.tex, "./temp/parity2_rp.txt")


  
post5 <- extract.samples(m5re)

pr.homog.mat.chain <- logistic(post5$a_mu + post5$b1 * 0 + post5$b2 * 0 +
  post5$b3 * 0 + post5$b9)
pr.mixed.mat.chain <- logistic(post5$a_mu + post5$b1 * 1 + post5$b2 * 0 +
  post5$b3 * 0 + post5$b9)
pr.homog.pat.chain <- logistic(post5$a_mu + post5$b1 * 0 + post5$b2 * 1 +
  post5$b3 * 0 + post5$b9)
pr.mixed.pat.chain <- logistic(post5$a_mu + post5$b1 * 1 + post5$b2 * 1 +
  post5$b3 * 1 + post5$b9)
  
pr.homog.mat.mean <- mean(pr.homog.mat.chain)
pr.mixed.mat.mean <- mean(pr.mixed.mat.chain)
pr.homog.pat.mean <- mean(pr.homog.pat.chain)
pr.mixed.pat.mean <- mean(pr.mixed.pat.chain)

pr.homog.mat.lb <- HPDI(pr.homog.mat.chain)[1]
pr.mixed.mat.lb <- HPDI(pr.mixed.mat.chain)[1]
pr.homog.pat.lb <- HPDI(pr.homog.pat.chain)[1]
pr.mixed.pat.lb <- HPDI(pr.mixed.pat.chain)[1]
  
pr.homog.mat.ub <- HPDI(pr.homog.mat.chain)[2]
pr.mixed.mat.ub <- HPDI(pr.mixed.mat.chain)[2]
pr.homog.pat.ub <- HPDI(pr.homog.pat.chain)[2]
pr.mixed.pat.ub <- HPDI(pr.mixed.pat.chain)[2]
  
  
# parity 2 variations

pdf("./temp/parity2_vars.pdf", height=8, width=20)
  
par(mfrow=c(1,2))
  
plot(1:6, ylim=c(0,.05), ylab="Annual Probability of Birth", 
  xlab="Parity 2 Sibset", xaxt="n", col=NULL, xlim=c(0, 7), frame.plot=FALSE, las=1)

abline(h=0:10/20, col=gray(0.8))

polygon(c(0.7, 0.7, 1.3, 1.3), c(0, pr.lineal.mat.mean, pr.lineal.mat.mean, 0), 
  col="steelblue", border=NA)
polygon(c(1.7, 1.7, 2.3, 2.3), c(0, pr.nolineal.mat.mean, pr.nolineal.mat.mean, 0), 
  col="steelblue", border=NA)

polygon(c(4.7, 4.7, 5.3, 5.3), c(0, pr.lineal.pat.mean, pr.lineal.pat.mean, 0), 
  col="sandybrown", border=NA)
polygon(c(5.7, 5.7, 6.3, 6.3), c(0, pr.nolineal.pat.mean, pr.nolineal.pat.mean, 0), 
  col="sandybrown", border=NA)

points(c(1, 2, 5, 6), c(pr.lineal.mat.mean, pr.nolineal.mat.mean, pr.lineal.pat.mean, 
  pr.nolineal.pat.mean), pch=20)
lines(c(1, 1), c(pr.lineal.mat.lb, pr.lineal.mat.ub))
lines(c(2, 2), c(pr.nolineal.mat.lb, pr.nolineal.mat.ub))
lines(c(5, 5), c(pr.lineal.pat.lb, pr.lineal.pat.ub))
lines(c(6, 6), c(pr.nolineal.pat.lb, pr.nolineal.pat.ub))

axis(1, at=c(1, 2), labels=c("At Least One\nInheriting", "None\nInherting"))

axis(1, at=c(5, 6), labels=c("At Least One\nInheriting", "None\nInherting"))

text(1.5, 0.25, "Matrilineal\nMosuo", col="steelblue", cex=1.3)

text(5.5, 0.25, "Patrilineal\nMosuo", col="sandybrown", cex=1.3)
  
# parity 2 bin: both one sex / different sexes
  
plot(1:6, ylim=c(0,0.05), ylab="Annual Probability of Birth", xlab="Parity 2 Sibset", 
  xaxt="n", col=NULL, xlim=c(0, 7), frame.plot=FALSE, las=1)

abline(h=0:10/20, col=gray(0.8))

polygon(c(0.7, 0.7, 1.3, 1.3), c(0, pr.homog.mat.mean, pr.homog.mat.mean, 0), 
  col="steelblue", border=NA)
polygon(c(1.7, 1.7, 2.3, 2.3), c(0, pr.mixed.mat.mean, pr.mixed.mat.mean, 0), 
  col="steelblue", border=NA)

polygon(c(4.7, 4.7, 5.3, 5.3), c(0, pr.homog.pat.mean, pr.homog.pat.mean, 0), 
  col="sandybrown", border=NA)
polygon(c(5.7, 5.7, 6.3, 6.3), c(0, pr.mixed.pat.mean, pr.mixed.pat.mean, 0), 
  col="sandybrown", border=NA)

points(c(1, 2, 5, 6), c(pr.homog.mat.mean, pr.mixed.mat.mean, pr.homog.pat.mean, 
  pr.mixed.pat.mean), pch=20)
lines(c(1, 1), c(pr.homog.mat.lb, pr.homog.mat.ub))
lines(c(2, 2), c(pr.mixed.mat.lb, pr.mixed.mat.ub))
lines(c(5, 5), c(pr.homog.pat.lb, pr.homog.pat.ub))
lines(c(6, 6), c(pr.mixed.pat.lb, pr.mixed.pat.ub))

axis(1, at=c(1, 2), labels=c("All Girls", "Boy, Girl"))

axis(1, at=c(5, 6), labels=c("All Boys", "Boy, Girl"))

text(1.5, 0.2, "Matrilineal\nMosuo", col="steelblue", cex=1.3)

text(5.5, 0.2, "Patrilineal\nMosuo", col="sandybrown", cex=1.3)
  
dev.off()

# move all temp to output
dir_init("./output")
files <- list.files("./temp", full.names=TRUE)
file.copy(files, "./output")

# housekeeping - delete temp
if(!save_temp) unlink("./temp", recursive=TRUE)
