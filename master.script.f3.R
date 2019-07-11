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

registerDoMC(7)  #change the 2 to your number of CPU cores  
options(cores=7)

#How many iterations do you want to do?
n =  1000 #number of iterations
y = 16 #number of decades (16 for 2160, 11 for 2110)

############# MODEL 2: RANDOM FUNDING ###############
source('01_R/model.prep.R')
source('01_R/models_funds/m2.f3.R')
risk.m2.f3 <- foreach(i = 1:n) %dopar% {
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m2.f3(sp.db)
}

out.m2.f3 <- ldply(risk.m2.f3)
out.m2.f3$binomial <- rep(rownames(risk.m2.f3[[1]]), length(risk.m2.f3))
out.m2.f3$sim <- rep(1:length(risk.m2.f3), each=nrow(risk.m2.f3[[1]]))

#calculate output stats
out <- out.m2.f3 #prep for analysis/graphics scripts
risk <- risk.m2.f3 #prep for analysis/graphics scripts
model <- "m2.f3"
source('01_R/metrics.f3.R')



##### #Model 3: Rarest #################
source('01_R/model.prep.R')
source('01_R/models_funds/m3.f3.R')
risk.m3.f3 <- foreach(i = 1:n) %dopar% {
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m3.f3(sp.db)
}

out.m3.f3 <- ldply(risk.m3.f3)
out.m3.f3$binomial <- rep(rownames(risk.m3.f3[[1]]), length(risk.m3.f3))
out.m3.f3$sim <- rep(1:length(risk.m3.f3), each=nrow(risk.m3.f3[[1]]))

#calculate output stats
out <- out.m3.f3 #prep for analysis/graphics scripts
risk <- risk.m3.f3 #prep for analysis/graphics scripts
model <- "m3.f3"
source('01_R/metrics.f3.R')


 

##### Model 4: Evolutionarily Distinct
source('01_R/model.prep.R')
source('01_R/models_funds/m4.f3.R')
risk.m4.f3 <- foreach(i = 1:n) %dopar% {
  #if(i %in% thousand){
  #  send_text(paste("beginning model m4.f3s", i, "of 100", Sys.time(), sep = " "))
  #}
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m4.f3(sp.db)
}

out.m4.f3 <- ldply(risk.m4.f3)
out.m4.f3$binomial <- rep(rownames(risk.m4.f3[[1]]), length(risk.m4.f3))
out.m4.f3$sim <- rep(1:length(risk.m4.f3), each=nrow(risk.m4.f3[[1]]))

out <- out.m4.f3 #prep for analysis/graphics scripts
risk <- risk.m4.f3 #prep for analysis/graphics scripts

model <- "m4.f3"
source('01_R/annual funding.R')

source('01_R/metrics.f3.R')


##### Model 5: EDGE
source('01_R/model.prep.R')
source('01_R/models_funds/m5.f3.R')
risk.m5.f3 <- foreach(i = 1:n) %dopar% {
  #if(i %in% thousand){
  #  send_text(paste("beginning model m5.f3s", i, "of 100", Sys.time(), sep = " "))
  #}
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m5.f3(sp.db)
}

out.m5.f3 <- ldply(risk.m5.f3)
out.m5.f3$binomial <- rep(rownames(risk.m5.f3[[1]]), length(risk.m5.f3))
out.m5.f3$sim <- rep(1:length(risk.m5.f3), each=nrow(risk.m5.f3[[1]]))

out <- out.m5.f3 #prep for analysis/graphics scripts
risk <- risk.m5.f3 #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
model <- "m5.f3"
source('01_R/metrics.f3.R')



##### Model 6: Commonest Spp
source('01_R/model.prep.R')
source('01_R/models_funds/m6.f3.R')
risk.m6.f3 <- foreach(i = 1:n) %dopar% {
  #if(i %in% thousand){
  #  send_text(paste("beginning model m6.f3s", i, "of 100", Sys.time(), sep = " "))
  #}
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m6.f3(sp.db)
}

out.m6.f3 <- ldply(risk.m6.f3)
out.m6.f3$binomial <- rep(rownames(risk.m6.f3[[1]]), length(risk.m6.f3))
out.m6.f3$sim <- rep(1:length(risk.m6.f3), each=nrow(risk.m6.f3[[1]]))

out <- out.m6.f3 #prep for analysis/graphics scripts
risk <- risk.m6.f3 #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
model <- "m6.f3"
source('01_R/metrics.f3.R')



###### Model 7: Faith's Index
source('01_R/model.prep.R')
source('01_R/models_funds/m7.f3.R')
risk.m7.f3 <- foreach(i = 1:n) %dopar% {
  #if(i %in% thousand){
  #  send_text(paste("beginning model m7.f3s", i, "of 100", Sys.time(), sep = " "))
  #}
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m7.f3(sp.db)
}

out.m7.f3 <- ldply(risk.m7.f3)
out.m7.f3$binomial <- rep(rownames(risk.m7.f3[[1]]), length(risk.m7.f3))
out.m7.f3$sim <- rep(1:length(risk.m7.f3), each=nrow(risk.m7.f3[[1]]))

out <- out.m7.f3 #prep for analysis/graphics scripts
risk <- risk.m7.f3 #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
model <- "m7.f3"
source('01_R/metrics.f3.R')


######## Model 8: Faith's Edge
source('01_R/model.prep.R')
source('01_R/models_funds/m8.f3.R')
risk.m8.f3 <- foreach(i = 1:n) %dopar% {
  #if(i %in% thousand){
  #  send_text(paste("beginning model m8.f3s", i, "of 100", Sys.time(), sep = " "))
  #}
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m8.f3(sp.db)
}

out.m8.f3 <- ldply(risk.m8.f3)
out.m8.f3$binomial <- rep(rownames(risk.m8.f3[[1]]), length(risk.m8.f3))
out.m8.f3$sim <- rep(1:length(risk.m8.f3), each=nrow(risk.m8.f3[[1]]))

out <- out.m8.f3 #prep for analysis/graphics scripts
risk <- risk.m8.f3 #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
model <- "m8.f3"
source('01_R/metrics.f3.R')









