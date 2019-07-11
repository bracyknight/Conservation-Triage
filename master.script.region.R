#MASTER CODE TO RUN ALL MODELS

library(phytools)
library(ape)
library(stringr)
library(foreach)
library(doMC)
library(picante)
library(ape)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2); theme_set(theme_bw())
library(purrr); library(forcats)
library(matrixStats)

registerDoMC(7)  #change the 2 to your number of CPU cores  
options(cores=7)

#How many iterations do you want to do?
n =  1000 #number of iterations
y = 16 #number of decades (16 for 2160, 11 for 2110)

sp.reg <- with(db, data.frame(binomial, supregion))
rownames(sp.reg) <- sp.reg$binomial
sp.reg$binomial <- NULL

##### #Model 2: Random Funding (but divided by country evenly) #################
source('01_R/model.prep.R')
source('01_R/models_region/m2.f2.reg.R')
risk.m2.f2.reg<- foreach(i = 1:n) %dopar% {
    
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m2.f2.reg(sp.db)
}

out.m2.f2.reg <- ldply(risk.m2.f2.reg)
out.m2.f2.reg$binomial <- rep(rownames(risk.m2.f2.reg[[1]]), length(risk.m2.f2.reg))
out.m2.f2.reg$sim <- rep(1:length(risk.m2.f2.reg), each=nrow(risk.m2.f2.reg[[1]]))
out <- out.m2.f2.reg #prep for analysis/graphics scripts
risk <- risk.m2.f2.reg #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
model <- 'm2.f2.reg'
source('01_R/metrics.R')
source('01_R/fam.gen.R')
source('01_R/reg.div.stats.R')
source('01_R/reg.ext.stats.R')


##### #Model 3: Rarest #################
source('01_R/model.prep.R')
source('01_R/models_region/m3.f2.reg.R')
risk.m3.f2.reg<- foreach(i = 1:n) %dopar% {
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m3.f2.reg(sp.db)
}

out.m3.f2.reg <- ldply(risk.m3.f2.reg)
out.m3.f2.reg$binomial <- rep(rownames(risk.m3.f2.reg[[1]]), length(risk.m3.f2.reg))
out.m3.f2.reg$sim <- rep(1:length(risk.m3.f2.reg), each=nrow(risk.m3.f2.reg[[1]]))
out <- out.m3.f2.reg #prep for analysis/graphics scripts
risk <- risk.m3.f2.reg #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
model <- 'm3.f2.reg'
source('01_R/metrics.R')
source('01_R/fam.gen.R')
source('01_R/reg.div.stats.R')
source('01_R/reg.ext.stats.R')

 

##### Model 4: Evolutionarily Distinct
source('01_R/model.prep.R')
source('01_R/models_region/m4.f2.reg.R')
risk.m4.f2.reg <- foreach(i = 1:n) %dopar% {
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m4.f2.reg(sp.db)
  
  #if(i %in% thousand){
  #  send_text(paste("finishing model m4.f2", i, "of",n, Sys.time(), sep = " "))
  #} 
}

out.m4.f2.reg <- ldply(risk.m4.f2.reg)
out.m4.f2.reg$binomial <- rep(rownames(risk.m4.f2.reg[[1]]), length(risk.m4.f2.reg))
out.m4.f2.reg$sim <- rep(1:length(risk.m4.f2.reg), each=nrow(risk.m4.f2.reg[[1]]))
out <- out.m4.f2.reg #prep for analysis/graphics scripts
risk <- risk.m4.f2.reg #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
model <- 'm4.f2.reg'
source('01_R/metrics.R')
source('01_R/fam.gen.R')
source('01_R/reg.div.stats.R')
source('01_R/reg.ext.stats.R')



##### Model 5: EDGE
source('01_R/model.prep.R')
source('01_R/models_region/m5.f2.reg.R')
risk.m5.f2.reg <- foreach(i = 1:n) %dopar% {
  
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m5.f2.reg(sp.db)
}

out.m5.f2.reg <- ldply(risk.m5.f2.reg)
out.m5.f2.reg$binomial <- rep(rownames(risk.m5.f2.reg[[1]]), length(risk.m5.f2.reg))
out.m5.f2.reg$sim <- rep(1:length(risk.m5.f2.reg), each=nrow(risk.m5.f2.reg[[1]]))

out <- out.m5.f2.reg #prep for analysis/graphics scripts
risk <- risk.m5.f2.reg #prep for analysis/graphics scripts
model <- 'm5.f2.reg'
source('01_R/metrics.R')
source('01_R/fam.gen.R')
source('01_R/reg.div.stats.R')
source('01_R/reg.ext.stats.R')


##### Model 6: Commonest Spp
source('01_R/model.prep.R')
source('01_R/models_region/m6.f2.reg.R')
risk.m6.f2.reg <- foreach(i = 1:n) %dopar% {
  #if(i %in% thousand){
  #  send_text(paste("beginning model m6.f2", i, "of",n, Sys.time(), sep = " "))
  #}
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m6.f2.reg(sp.db)
}

out.m6.f2.reg <- ldply(risk.m6.f2.reg)
out.m6.f2.reg$binomial <- rep(rownames(risk.m6.f2.reg[[1]]), length(risk.m6.f2.reg))
out.m6.f2.reg$sim <- rep(1:length(risk.m6.f2.reg), each=nrow(risk.m6.f2.reg[[1]]))

out <- out.m6.f2.reg #prep for analysis/graphics scripts
risk <- risk.m6.f2.reg #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
model <- 'm6.f2.reg'
source('01_R/metrics.R')
source('01_R/fam.gen.R')
source('01_R/reg.div.stats.R')
source('01_R/reg.ext.stats.R')

###### Model 7: Faith's Index
source('01_R/model.prep.R')
source('01_R/models_region/m7.f2.reg.R')
risk.m7.f2.reg <- foreach(i = 1:n) %dopar% {
  if(i %in% thousand){
    send_text(paste("beginning model m7.f2.reg", i, "of 100", Sys.time(), sep = " "))
  }
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  #dimnames(sp.db)
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m7.f2.reg(sp.db)
   
}

out.m7.f2.reg <- ldply(risk.m7.f2.reg)
out.m7.f2.reg$binomial <- rep(rownames(risk.m7.f2.reg[[1]]), length(risk.m7.f2.reg))
out.m7.f2.reg$sim <- rep(1:length(risk.m7.f2.reg), each=nrow(risk.m7.f2.reg[[1]]))

out <- out.m7.f2.reg #prep for analysis/graphics scripts
risk <- risk.m7.f2.reg #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
model <- 'm7.f2.reg'
source('01_R/metrics.R')
source('01_R/fam.gen.R')
source('01_R/reg.div.stats.R')
source('01_R/reg.ext.stats.R')


######## Model 8: Faith's Edge
source('01_R/model.prep.R')
source('01_R/models_region/m8.f2.reg.R')
risk.m8.f2.reg <- foreach(i = 1:n) %dopar% {
  #if(i %in% thousand){
  #  send_text(paste("beginning model m8.f2s", i, "of 100", Sys.time(), sep = " "))
  #}
  
 sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m8.f2.reg(sp.db)
}

out.m8.f2.reg <- ldply(risk.m8.f2.reg)
out.m8.f2.reg$binomial <- rep(rownames(risk.m8.f2.reg[[1]]), length(risk.m8.f2.reg))
out.m8.f2.reg$sim <- rep(1:length(risk.m8.f2.reg), each=nrow(risk.m8.f2.reg[[1]]))

model <- 'm8.f2.reg'
out <- out.m8.f2.reg #prep for analysis/graphics scripts
risk <- risk.m8.f2.reg #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
source('01_R/metrics.R')
source('01_R/fam.gen.R')
source('01_R/reg.div.stats.R')
source('01_R/reg.ext.stats.R')

##### Model 5: EDGE BUT WITH REGIONAL TREES USED TO GENERATE UNIQUENESS
source('01_R/model.prep.R')
source('01_R/models_region/m5.f2.reg.regd.R')
risk.m5.f2.reg.regd <- foreach(i = 1:n) %dopar% {
  
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m5.f2.reg.regd(sp.db)
}

risk.m5.f2.reg.reg.d <- ldply(risk.m5.f2.reg.regd)
risk.m5.f2.reg.reg.d$binomial <- rep(rownames(risk.m5.f2.reg.regd[[1]]), length(risk.m5.f2.reg.regd))
risk.m5.f2.reg.reg.d$sim <- rep(1:length(risk.m5.f2.reg.regd), each=nrow(risk.m5.f2.reg.regd[[1]]))

out <- risk.m5.f2.reg.reg.d #prep for analysis/graphics scripts
risk <- risk.m5.f2.reg.reg.d #prep for analysis/graphics scripts
model <- 'm5.f2.reg.reg.d'
source('01_R/metrics.R')
source('01_R/fam.gen.R')
source('01_R/reg.div.stats.R')
source('01_R/reg.ext.stats.R')


##### Model 5: EDGE BUT WITH REGIONAL TREES USED TO GENERATE UNIQUENESS & CONSTANT FUNDING
source('01_R/model.prep.R')
source('01_R/models_region/m5.CF.reg.phy.R')
risk.m5.CF.reg.phy <- foreach(i = 1:n) %dopar% {
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m5.CF.reg.phy(sp.db)
}

out.m5.CF.reg.phy <- ldply(risk.m5.CF.reg.phy)
out.m5.CF.reg.phy$binomial <- rep(rownames(risk.m5.CF.reg.phy[[1]]), length(risk.m5.CF.reg.phy))
out.m5.CF.reg.phy$sim <- rep(1:length(risk.m5.CF.reg.phy), each=nrow(risk.m5.CF.reg.phy[[1]]))
out <- out.m5.CF.reg.phy #prep for analysis/graphics scripts
risk <- risk.m5.CF.reg.phy #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
model <- 'm5.CF.reg.phy'
source('01_R/metrics.R')
source('01_R/fam.gen.R')
source('01_R/reg.div.stats.R')
source('01_R/reg.ext.stats.R')
