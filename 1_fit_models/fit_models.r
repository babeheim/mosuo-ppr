
if (scaffold) {
  rm(list = ls())
  source("../project_support.r")
}

dir_init("./temp")

d <- read.csv("./inputs/final_regression_data.csv", as.is = TRUE)
d$pcom <- gsub("P", "", d$pcom)

tic("fit parity1 random effects model")
d2 <- d[which(d$pcom %in% c("1", "0")),]
d2$son <- as.numeric(d2$pcode)
d2$son_patrilineal <- as.numeric(d2$pcode == 1 & d2$patrilineal == 1)
uid.list <- sort(unique(d2$uid))
d2$id <- match(d2$uid, uid.list)
 model2re <- alist(
  conc ~ dbinom(1, p),
  logit(p) <- a[id] +
    b1 * son + b2 * patrilineal + b3 * son_patrilineal +
    b4 * age15 +
    b6 * yearbirth1930 + b7 * yearbirth1940 + b8 * yearbirth1950 +
    b9 * yearbirth1970 + b10 * yearbirth1920 + b11 * yearbirth1980 +
    b12 * mi_job + b13 * highestgrade +
    b14 * bly +
    b15 * age25 + b16 * age30 + b17 * age35 + b18 * age40,
  a[id] ~ dnorm(a_mu, a_sigma),
  c(a_mu, b1, b2, b3, b4, b6, b7, b8, b9, b10, b11, b12, b13, b14,
    b15, b16, b17, b18) ~ dnorm(0, 100),
  a_sigma ~ dcauchy(0,1)
)
m2re <- map2stan(model2re, data = d2, sample = enable_sampling, iter = n_iter)
save(m2re, file = "./temp/parity1_re.robj")
toc(log = TRUE)

# baselines:
# cohort 1960-69
# age 25-29

tic("fit parity2 random effects model")
d3 <- d[which(d$pcom %in% c("11", "10", "00")),]
d3 <- d3[which(d3$yearbirth1980 != "1"),]
d3$pat_10 <- as.numeric(d3$pcode == "10" & d3$patrilineal == 1)
d3$pat_11 <- as.numeric(d3$pcode == "11" & d3$patrilineal == 1)
d3$pcode10 <- as.numeric(d3$pcode == "10")
d3$pcode11 <- as.numeric(d3$pcode == "11")
uid.list <- sort(unique(d3$uid))
d3$id <- match(d3$uid, uid.list)
model3 <- alist(
  conc ~ dbinom(1, p),
  logit(p) <- a[id] +
    b1 * pcode10 + b2 * pcode11 +
    b3 * patrilineal +
    b4 * pat_10 + b5 * pat_11 +
    b6 * age15 + b7 * age20 +
    b8 * yearbirth1930 + b9 * yearbirth1940 + b10 * yearbirth1950 +
    b11 * yearbirth1970 + b12 * yearbirth1920 +
    b14 * mi_job + b15 * highestgrade + b16 * bly +
    b17 * age30 + b18 * age35 + b19 * age40,
  a[id] ~ dnorm(a_mu, a_sigma),
  c(a_mu, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b14,
    b15, b16, b17, b18, b19) ~ dnorm(0, 100),
  a_sigma ~ dcauchy(0,1)
)
m3re <- map2stan(model3, data = d3, iter = n_iter, sample = enable_sampling)
save(m3re, file = "./temp/parity2_re.robj")
toc(log = TRUE)

# recode parity 2 for lineal vs nonlineal

tic("fit parity2 'lineal' random effects model")
d4 <- d3
d4$type <- NA
d4$type[d4$pcode == "11" & d4$patrilineal == "0"] <- "no_lineal"
d4$type[d4$pcode == "00" & d4$patrilineal == "1"] <- "no_lineal"
d4$type[d4$pcode != "11" & d4$patrilineal == "0"] <- "at_least_one_lineal"
d4$type[d4$pcode != "00" & d4$patrilineal == "1"] <- "at_least_one_lineal"
d4$no_lineal_pat <- as.numeric(d4$type == "no_lineal" & d4$patrilineal == "1")
d4$type_nolineal <- as.numeric(d4$type == "no_lineal")
model4 <- alist(
  conc ~ dbinom(1, p),
  logit(p) <- a[id] +
    b1 * type_nolineal + b2 * patrilineal + b3 * no_lineal_pat +
    b4 * age15 + b5 * age20 +
    b6 * yearbirth1930 + b7 * yearbirth1940 +
    b8 * yearbirth1950 + b9 * yearbirth1970 + b10 * yearbirth1920 +
    b12 * mi_job + b13 * highestgrade + b14 * bly +
    b15 * age30 + b16 * age35 + b17 * age40,
  a[id] ~ dnorm(a_mu, a_sigma),
  c(a_mu, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b12, b13,
    b14, b15, b16, b17) ~ dnorm(0, 100),
  a_sigma ~ dcauchy(0,1)
)
m4re <- map2stan(model4, data = d4, sample = enable_sampling, iter = n_iter)
save(m4re, file = "./temp/parity2_pooled1_re.robj")
toc(log = TRUE)

#  recode for homogeneous and heterogeneous gender compositions for parity2

tic("fit parity2 'balanced' random effects model")
d5 <- d4
d5$type <- NA
d5$type[d5$pcode == "11" | d5$pcode == "00"] <- "H"
d5$type[d5$pcode == "01" | d5$pcode == "10"] <- "M"
d5$typeM <- as.numeric(d5$type == "M")
d5$Mpat <- as.numeric(d5$type == "M" & d5$patrilineal == "1")
d5$type_mixed <- as.numeric(d5$type == "M")
model5 <- alist(
  conc ~ dbinom(1, p),
  logit(p) <- a[id] +
    b1 * type_mixed + b2 * patrilineal + b3 * Mpat +
    b4 * age15 + b5 * age20 +
    b6 * yearbirth1930 + b7 * yearbirth1940 +
    b8 * yearbirth1950 + b9 * yearbirth1970 + b10 * yearbirth1920 +
    b12 * mi_job + b13 * highestgrade + b14 * bly +
    b15 * age30 + b16 * age35 + b17 * age40,
  a[id] ~ dnorm(a_mu, a_sigma),
  c(a_mu, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b12, b13,
    b14, b15, b16, b17) ~ dnorm(0, 100),
  a_sigma ~ dcauchy(0,1)
)
m5re <- map2stan(model5, data = d5, sample = enable_sampling, iter = n_iter)
save(m5re, file = "./temp/parity2_pooled2_re.robj")
toc(log = TRUE)

# parity 0

# baselines:
# 1960-69 cohort
# age 20-24

tic("fit parity0 random effects model")
d6 <- d[which(d$pcom %in% c("")),]
uid.list <- sort(unique(d6$uid))
d6$id <- match(d6$uid, uid.list)
model6re <- alist(
  conc ~ dbinom(1, p),
  logit(p) <- a[id] +
    b2 * patrilineal + b4 * age15 +
    b6 * yearbirth1930 + b7 * yearbirth1940 + b8 * yearbirth1950 +
    b9 * yearbirth1970 + b10 * yearbirth1920 + b11 * yearbirth1980 +
    b12 * mi_job + b13 * highestgrade +
    b15 * age25 + b16 * age30 + b17 * age35 + b18 * age40,
  a[id] ~ dnorm(a_mu, a_sigma),
  c(a_mu, b2, b4, b6, b7, b8, b9, b10, b11, b12, b13,
    b15, b16, b17, b18) ~ dnorm(0, 100),
  a_sigma ~ dcauchy(0,1)
)
m6re <- map2stan(model6re, data = d6, sample = enable_sampling, iter = n_iter)
save(m6re, file = "./temp/parity0_re.robj")
toc(log = TRUE)

# analysis of parity 3

# baselines:
# 1960-69 cohort
# age 25-29

tic("fit parity3 random effects model")
d7 <- d[which(nchar(d$pcom) == "3"),]
d7 <- d7[which(d7$yearbirth1980 != "1"),] # check this number of ppl
d7 <- d7[which(d7$yearbirth1970 != "1"),] # check this number of ppl
d7 <- d7[which(d7$ageb != "15"),] # check this number of ppl
d7 <- d7[which(d7$mi_job != "1"),] # check this number of ppl
d7$pat_100 <- as.numeric(d7$pcom == "100" & d7$patrilineal == 1)
d7$pat_110 <- as.numeric(d7$pcom == "110" & d7$patrilineal == 1)
d7$pat_111 <- as.numeric(d7$pcom == "111" & d7$patrilineal == 1)
d7$pcode100 <- as.numeric(d7$pcom == "100")
d7$pcode110 <- as.numeric(d7$pcom == "110")
d7$pcode111 <- as.numeric(d7$pcom == "111")
uid.list <- sort(unique(d7$uid))
d7$id <- match(d7$uid, uid.list)
model7 <- alist(
  conc ~ dbinom(1, p),
  logit(p) <- a[id] +
    b1 * pcode100 + b2 * pcode110 + b3 * pcode111 +
    b4 * patrilineal +
    b5 * pat_100 + b6 * pat_110 + b7 * pat_111 +
    b9 * age20 +
    b10 * yearbirth1930 + b11 * yearbirth1940 +
    b12 * yearbirth1950 + b14 * yearbirth1920 +
    b16 * highestgrade +
    b17 * bly +
    b18 * age30 + b19 * age35 + b20 * age40,
  a[id] ~ dnorm(a_mu, a_sigma),
  c(a_mu, b1, b2, b3, b4, b5, b6, b7, b9, b10, b11, b12, b14, b16, b17,
  	b18, b19, b20) ~ dnorm(0, 100),
  a_sigma ~ dcauchy(0,1)
)
m7re <- map2stan(model7, data = d7, iter = n_iter, sample = enable_sampling)
save(m7re, file = "./temp/parity3_re.robj")
toc(log = TRUE)

# analysis of parity 4

# baselines:
# 1940-49 cohort
# age 40+

tic("fit parity4 random effects model")
d8 <- d[which(nchar(d$pcom) == "4"),]
d8 <- d8[which(d8$yearbirth1980 != "1"),] # check this number of ppl
d8 <- d8[which(d8$yearbirth1970 != "1"),] # check this number of ppl
d8 <- d8[which(d8$yearbirth1960 != "1"),] # check this number of ppl
d8 <- d8[which(d8$ageb != "15"),] # check this number of ppl
d8 <- d8[which(d8$ageb != "20"),] # check this number of ppl
d8$pat_1000 <- as.numeric(d8$pcom == "1000" & d8$patrilineal == 1)
d8$pat_1100 <- as.numeric(d8$pcom == "1100" & d8$patrilineal == 1)
d8$pat_1110 <- as.numeric(d8$pcom == "1110" & d8$patrilineal == 1)
d8$pat_1111 <- as.numeric(d8$pcom == "1111" & d8$patrilineal == 1)
d8$pcode1000 <- as.numeric(d8$pcom == "1000")
d8$pcode1100 <- as.numeric(d8$pcom == "1100")
d8$pcode1110 <- as.numeric(d8$pcom == "1110")
d8$pcode1111 <- as.numeric(d8$pcom == "1111")
uid.list <- sort(unique(d8$uid))
d8$id <- match(d8$uid, uid.list)
model8 <- alist(
  conc ~ dbinom(1, p),
  logit(p) <- a[id] +
    b1 * pcode1000 + b2 * pcode1100 +
    b3 * pcode1110 + b4 * pcode1111 +
    b5 * patrilineal +
    b6 * pat_1000 + b7 * pat_1100 +
    b8 * pat_1110 + b9 * pat_1111 +
    b10 * yearbirth1920 + b11 * yearbirth1930 + b12 * yearbirth1950 +
    b13 * highestgrade +
    b14 * bly +
    b15 * age25 + b16 * age30 + b17 * age35,
  a[id] ~ dnorm(a_mu, a_sigma),
  c(a_mu, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14,
    b15, b16, b17) ~ dnorm(0, 100),
  a_sigma ~ dcauchy(0,1)
)
m8re <- map2stan(model8, data = d8, iter = n_iter, sample = enable_sampling)
save(m8re, file = "./temp/parity4_re.robj")
toc(log = TRUE)

# move all temp to output
dir_init("./output")
files <- list.files("./temp", full.names = TRUE)
file.copy(files, "./output")

# housekeeping - delete temp
if(!save_temp) unlink("./temp", recursive = TRUE)
