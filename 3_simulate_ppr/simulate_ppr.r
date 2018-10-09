
if (scaffold) {
  rm(list = ls())
  source("../project_support.r")
}

start.time <- Sys.time()

dir_init("./temp")

reg <- read.csv("./inputs/Mosuo_pop_reg.csv", as.is=TRUE)

reg$parity.code <- NA
reg$n.kids <- 0
reg$age.today <- NA
reg$age.cats <- NA

has.kids <- which(reg$pid %in% reg$m.pid)
for(i in 1:length(has.kids)){

  my.kids.rows <- which(reg$m.pid == reg$pid[has.kids[i]])
  reg$parity.code[has.kids[i]] <- paste(sort(reg$male[my.kids.rows], 
    decreasing=TRUE), collapse="")
  reg$n.kids[has.kids[i]] <- length(my.kids.rows)

}

reg$son <- as.numeric(reg$parity.code=="1")
reg$son_patrilineal <- as.numeric(reg$son==1 & reg$patrilineal==1)

reg$pcode10 <- as.numeric(reg$parity.code=="10")
reg$pat_10 <- as.numeric(reg$pcode10==1 & reg$patrilineal==1)
reg$pcode11 <- as.numeric(reg$parity.code=="11")
reg$pat_11 <- as.numeric(reg$pcode11==1 & reg$patrilineal==1)

reg$pcode100 <- as.numeric(reg$parity.code=="100")
reg$pat_100 <- as.numeric(reg$pcode100==1 & reg$patrilineal==1)
reg$pcode110 <- as.numeric(reg$parity.code=="110")
reg$pat_110 <- as.numeric(reg$pcode110==1 & reg$patrilineal==1)
reg$pcode111 <- as.numeric(reg$parity.code=="111")
reg$pat_111 <- as.numeric(reg$pcode111==1 & reg$patrilineal==1)

reg$pcode1000 <- as.numeric(reg$parity.code=="1000")
reg$pat_1000 <- as.numeric(reg$pcode1000==1 & reg$patrilineal==1)
reg$pcode1100 <- as.numeric(reg$parity.code=="1100")
reg$pat_1100 <- as.numeric(reg$pcode1100==1 & reg$patrilineal==1)
reg$pcode1110 <- as.numeric(reg$parity.code=="1110")
reg$pat_1110 <- as.numeric(reg$pcode1110==1 & reg$patrilineal==1)
reg$pcode1111 <- as.numeric(reg$parity.code=="1111")
reg$pat_1111 <- as.numeric(reg$pcode1111==1 & reg$patrilineal==1)

current.year <- 2008
reg$age.today <- current.year - reg$yob
reg$age.cats <- reg$age.today - (reg$age.today %% 5)
if(any(reg$age.cats > 90)) reg$age.cats[reg$age.cats > 90] <- 90

reg$age15 <- as.numeric(reg$age.cats==15)
reg$age20 <- as.numeric(reg$age.cats==20)
reg$age25 <- as.numeric(reg$age.cats==25)
reg$age30 <- as.numeric(reg$age.cats==30)
reg$age35 <- as.numeric(reg$age.cats==35)
reg$age40 <- as.numeric(reg$age.cats==40)

mosuo.reg <- reg

library(rethinking)

load("./inputs/parity0_re.robj") # m6re
load("./inputs/parity1_re.robj") # m2re
load("./inputs/parity2_re.robj") # m3re
load("./inputs/parity3_re.robj") # m7re
load("./inputs/parity4_re.robj") # m8re

parity0.post <- extract.samples(m6re)
parity1.post <- extract.samples(m2re)
parity2.post <- extract.samples(m3re)
parity3.post <- extract.samples(m7re)
parity4.post <- extract.samples(m8re)

age.specific.mortality.males <- c(12.928, 1.234, 1.104, 1.768, 2.584, 3.004, 
  3.624, 4.978, 7.384, 10.82, 16.906, 24.06, 36.934, 48.222, 70.428, 90.41, 112.454, 200, 800)
names(age.specific.mortality.males) <- seq(0, 90, by=5)
age.specific.mortality.females <- c(16.628, 1.416, 1.102, 2.222, 3.12, 3.956, 
  4.66, 5.82, 7.372, 10.034, 14.114, 20.1, 28.396, 39.996, 55.57, 75.234, 101.328, 200, 800)
names(age.specific.mortality.females) <- seq(0, 90, by=5)

n.sims <- 10
  
### simulator
  
for(j in 1:n.sims){

  reg <- mosuo.reg

  new.baby.template <- reg[1,]
  new.baby.template[!is.na(new.baby.template)] <- NA

  current.year <- 2009
  stop.year <- 2109

  while(current.year <= stop.year){

    # step 1: update state variables
    
    reg$age.today <- current.year - reg$yob
    reg$age.cats <- reg$age.today - (reg$age.today %% 5)
    if(any(reg$age.cats > 90)) reg$age.cats[reg$age.cats > 90] <- 90
    reg$age15 <- as.numeric(reg$age.cats==15)
    reg$age20 <- as.numeric(reg$age.cats==20)
    reg$age25 <- as.numeric(reg$age.cats==25)
    reg$age30 <- as.numeric(reg$age.cats==30)
    reg$age35 <- as.numeric(reg$age.cats==35)
    reg$age40 <- as.numeric(reg$age.cats==40)
      
    has.kids <- which(reg$pid %in% reg$m.pid)
      
    for(i in 1:length(has.kids)){

      my.kids.rows <- which(reg$m.pid == reg$pid[has.kids[i]])
      reg$parity.code[has.kids[i]] <- paste(sort(reg$male[my.kids.rows], 
        decreasing=TRUE), collapse="")
      reg$n.kids[has.kids[i]] <- length(my.kids.rows)

    }
    
    reg$son <- as.numeric(reg$parity.code=="1")
    reg$son_patrilineal <- as.numeric(reg$son==1 & reg$patrilineal==1)

    reg$pcode10 <- as.numeric(reg$parity.code=="10")
    reg$pat_10 <- as.numeric(reg$pcode10==1 & reg$patrilineal==1)
    reg$pcode11 <- as.numeric(reg$parity.code=="11")
    reg$pat_11 <- as.numeric(reg$pcode11==1 & reg$patrilineal==1)

    reg$pcode100 <- as.numeric(reg$parity.code=="100")
    reg$pat_100 <- as.numeric(reg$pcode100==1 & reg$patrilineal==1)
    reg$pcode110 <- as.numeric(reg$parity.code=="110")
    reg$pat_110 <- as.numeric(reg$pcode110==1 & reg$patrilineal==1)
    reg$pcode111 <- as.numeric(reg$parity.code=="111")
    reg$pat_111 <- as.numeric(reg$pcode111==1 & reg$patrilineal==1)

    reg$pcode1000 <- as.numeric(reg$parity.code=="1000")
    reg$pat_1000 <- as.numeric(reg$pcode1000==1 & reg$patrilineal==1)
    reg$pcode1100 <- as.numeric(reg$parity.code=="1100")
    reg$pat_1100 <- as.numeric(reg$pcode1100==1 & reg$patrilineal==1)
    reg$pcode1110 <- as.numeric(reg$parity.code=="1110")
    reg$pat_1110 <- as.numeric(reg$pcode1110==1 & reg$patrilineal==1)
    reg$pcode1111 <- as.numeric(reg$parity.code=="1111")
    reg$pat_1111 <- as.numeric(reg$pcode1111==1 & reg$patrilineal==1)


    # step 2: mortality

    dead.pids <- grim_reaper(reg)
    if(length(dead.pids)>0) reg$yod[reg$pid %in% dead.pids] <- current.year

    # step 3: fertility

    new.moms <- baby_maker(reg)

    if(length(new.moms)>0){

      for(i in 1:length(new.moms)){

        new.baby <- new.baby.template
        new.baby$pid <- pid_maker(1, reserved=reg$pid)
        new.baby$yob <- current.year
        new.baby$male <- rbinom(1, 1, 0.500)
        new.baby$m.pid <- new.moms[i]
        new.baby$patrilineal <- reg$patrilineal[which(reg$pid==new.moms[i])]
        new.baby$n.kids <- 0
        reg <- rbind(reg, new.baby)

      }


    }
      
    # step 4: end loop

    current.year <- current.year + 1

    # if(current.year %% 10 == 0) print(current.year)

  }

  if(any(duplicated(reg$pid))) stop("pid duplication error; check pid_maker seed")

  print(paste("simulation ", j, " complete", sep=""))
  filename <- paste("./temp/simulation_", j, ".csv", sep="")
  write.csv(reg, filename, row.names=FALSE)
    
}    

stop.time<- Sys.time()
cat(task_timer("simulate off fitted models"), file="./temp/simulateppr_log.txt")

# move all temp to output
dir_init("./output")
files <- list.files("./temp", full.names=TRUE)
file.copy(files, "./output")

# housekeeping - delete temp
if(!save_temp) unlink("./temp", recursive=TRUE)
