#MASTER CODE TO RUN ALL MODELS

library(phytools)
library(ape)
library(stringr)
library(foreach)
library(doMC)
library(picante)
library(ape)
library(tidyverse)
library(matrixStats)
library(ggplot2); theme_set(theme_bw())

registerDoMC(7)  #change the 2 to your number of CPU cores  
options(cores=7)

#How many iterations do you want to do?
n =  1000 #number of iterations
y = 16 #number of decades (16 for 2160, 11 for 2110)

############# MODEL 2: RANDOM FUNDING ###############
source('01_R/model.prep.R')
source('01_R/models_funds/m2.f2.R')
risk.m2.f2 <- foreach(i = 1:n) %dopar% {
  if(i %in% thousand){
    send_text(paste("starting model m2.f2", i, "of",n, Sys.time(), sep = " "))
  }
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m2.f2(sp.db)

}

out.m2.f2 <- ldply(risk.m2.f2)
out.m2.f2$binomial <- rep(rownames(risk.m2.f2[[1]]), length(risk.m2.f2))
out.m2.f2$sim <- rep(1:length(risk.m2.f2), each=nrow(risk.m2.f2[[1]]))

out <- out.m2.f2 #prep for analysis/graphics scripts
risk <- risk.m2.f2 #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
source('01_R/metrics.R')

##### #Model 3: Rarest #################
source('01_R/model.prep.R')
source('01_R/models_funds/m3.f2.R')
risk.m3.f2 <- foreach(i = 1:n) %dopar% {
  if(i %in% thousand){
  send_text(paste("starting model m3.f2", i, "of",n, Sys.time(), sep = " "))
  } 
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m3.f2(sp.db)
}

out.m3.f2 <- ldply(risk.m3.f2)
out.m3.f2$binomial <- rep(rownames(risk.m3.f2[[1]]), length(risk.m3.f2))
out.m3.f2$sim <- rep(1:length(risk.m3.f2), each=nrow(risk.m3.f2[[1]]))

out <- out.m3.f2 #prep for analysis/graphics scripts
risk <- risk.m3.f2 #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
source('01_R/metrics.R')
m3.f2.survived <- data.frame(survived)
m3.f2.diversity <- data.frame(diversity)
m3.f2.p.risk.ch <- data.frame(out.change)
m3.f2.habs <- data.frame(habs)
m3.f2.risk <- data.frame(out.rare)
m3.f2.improved <- data.frame(sp.imp)

##### Model 4: Evolutionarily Distinct
source('01_R/model.prep.R')
source('01_R/models_funds/m4.f2.R')
risk.m4.f2 <- foreach(i = 1:n) %dopar% {
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m4.f2(sp.db)
  
  #if(i %in% thousand){
  #  send_text(paste("finishing model m4.f2", i, "of",n, Sys.time(), sep = " "))
  #} 
}

out.m4.f2 <- ldply(risk.m4.f2)
out.m4.f2$binomial <- rep(rownames(risk.m4.f2[[1]]), length(risk.m4.f2))
out.m4.f2$sim <- rep(1:length(risk.m4.f2), each=nrow(risk.m4.f2[[1]]))

out <- out.m4.f2 #prep for analysis/graphics scripts
risk <- risk.m4.f2 #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
source('01_R/metrics.R')
m4.f2.survived <- data.frame(survived)
m4.f2.diversity <- data.frame(diversity)
m4.f2.p.risk.ch <- data.frame(out.change)
m4.f2.habs <- data.frame(habs)
m4.f2.risk <- data.frame(out.rare)
m4.f2.improved <- data.frame(sp.imp)

##### Model 5: EDGE
source('01_R/model.prep.R')
source('01_R/models_funds/m5.f2.R')
risk.m5.f2 <- foreach(i = 1:n) %dopar% {
  if(i %in% thousand){
    send_text(paste("beginning model m5.f2", i, "of",n, Sys.time(), sep = " "))
  }
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m5.f2(sp.db)
  
  
}

out.m5.f2 <- ldply(risk.m5.f2)
out.m5.f2$binomial <- rep(rownames(risk.m5.f2[[1]]), length(risk.m5.f2))
out.m5.f2$sim <- rep(1:length(risk.m5.f2), each=nrow(risk.m5.f2[[1]]))

out <- out.m5.f2 #prep for analysis/graphics scripts
risk <- risk.m5.f2 #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
source('01_R/metrics.R')
m5.f2.survived <- data.frame(survived)
m5.f2.diversity <- data.frame(diversity)
m5.f2.p.risk.ch <- data.frame(out.change)
m5.f2.habs <- data.frame(habs)
m5.f2.risk <- data.frame(out.rare)
m5.f2.improved <- data.frame(sp.imp)


##### Model 6: Commonest Spp
source('01_R/model.prep.R')
source('01_R/models_funds/m6.f2.R')
risk.m6.f2 <- foreach(i = 1:n) %dopar% {
  #if(i %in% thousand){
  #  send_text(paste("beginning model m6.f2", i, "of",n, Sys.time(), sep = " "))
  #}
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m6.f2(sp.db)
}

out.m6.f2 <- ldply(risk.m6.f2)
out.m6.f2$binomial <- rep(rownames(risk.m6.f2[[1]]), length(risk.m6.f2))
out.m6.f2$sim <- rep(1:length(risk.m6.f2), each=nrow(risk.m6.f2[[1]]))

out <- out.m6.f2 #prep for analysis/graphics scripts
risk <- risk.m6.f2 #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
source('01_R/metrics.R')
m6.f2.survived <- data.frame(survived)
m6.f2.diversity <- data.frame(diversity)
m6.f2.p.risk.ch <- data.frame(out.change)
m6.f2.habs <- data.frame(habs)
m6.f2.risk <- data.frame(out.rare)
m6.f2.improved <- data.frame(sp.imp)


###### Model 7: Faith's Index
source('01_R/model.prep.R')
source('01_R/models_funds/m7.f2.R')
risk.m7.f2 <- foreach(i = 1:n) %dopar% {
  if(i %in% thousand){
    send_text(paste("beginning model m7.f2", i, "of 100", Sys.time(), sep = " "))
  }
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  #dimnames(sp.db)
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m7.f2(sp.db)
   
}

out.m7.f2 <- ldply(risk.m7.f2)
out.m7.f2$binomial <- rep(rownames(risk.m7.f2[[1]]), length(risk.m7.f2))
out.m7.f2$sim <- rep(1:length(risk.m7.f2), each=nrow(risk.m7.f2[[1]]))

out <- out.m7.f2 #prep for analysis/graphics scripts
risk <- risk.m7.f2 #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
source('01_R/metrics.R')
m7.f2.survived <- data.frame(survived)
m7.f2.diversity <- data.frame(diversity)
m7.f2.p.risk.ch <- data.frame(out.change)
m7.f2.habs <- data.frame(habs)
m7.f2.risk <- data.frame(out.rare)
m7.f2.improved <- data.frame(sp.imp)


######## Model 8: Faith's Edge
source('01_R/model.prep.R')
source('01_R/models_funds/m8.f2.R')
risk.m8.f2 <- foreach(i = 1:n) %dopar% {
  #if(i %in% thousand){
  #  send_text(paste("beginning model m8.f2s", i, "of 100", Sys.time(), sep = " "))
  #}
  
 sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m8.f2(sp.db)
}

out.m8.f2 <- ldply(risk.m8.f2)
out.m8.f2$binomial <- rep(rownames(risk.m8.f2[[1]]), length(risk.m8.f2))
out.m8.f2$sim <- rep(1:length(risk.m8.f2), each=nrow(risk.m8.f2[[1]]))
h2 <- habitat
h2$binomial <- tolower(h2$binomial)
out.m8.f2.k <- merge(out.m8.f2, h2, all.x = T, by = 'binomial')

out <- out.m8.f2.k #prep for analysis/graphics scripts
risk <- risk.m8.f2 #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
source('01_R/metrics.R')
m8.f2.survived <- data.frame(survived)
m8.f2.diversity <- data.frame(diversity)
m8.f2.p.risk.ch <- data.frame(out.change)
m8.f2.habs <- data.frame(habs)
m8.f2.risk <- data.frame(out.rare)
m8.f2.improved <- data.frame(sp.imp)



##### Model 5.f1: EDGE + funding remains 10% extant spp at risk
source('01_R/model.prep.R')
source('01_R/models_funds/m5.f2.f1.R')
risk.m5.f2.f1 <- foreach(i = 1:n) %dopar% {
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m5.f2.f1(sp.db)
  
  if(i %in% thousand){
    send_text(paste("finishing model m5.f2", i, "of",n, Sys.time(), sep = " "))
  } 
}

out.m5.f2.f1 <- ldply(risk.m5.f2.f1)
out.m5.f2.f1$binomial <- rep(rownames(risk.m5.f2.f1[[1]]), length(risk.m5.f2.f1))
out.m5.f2.f1$sim <- rep(1:length(risk.m5.f2.f1), each=nrow(risk.m5.f2.f1[[1]]))

out <- out.m5.f2.f1 #prep for analysis/graphics scripts
risk <- risk.m5.f2.f1 #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
source('01_R/metrics.R')
m5.f2.f1.survived <- data.frame(survived)
m5.f2.f1.diversity <- data.frame(diversity)
m5.f2.f1.p.risk.ch <- data.frame(out.change)
m5.f2.f1.habs <- data.frame(habs)
m5.f2.f1.risk <- data.frame(out.rare)
m5.f2.f1.improved <- data.frame(sp.imp)
#####

######## Repeat above models, but allow for funding to reflect endangered and extinct

####### Model 3 f2: Rarest
source('01_R/model.prep.R')
source('01_R/models_funds/m3.f2.R')
risk.m3.f2 <- foreach(i = 1:n) %dopar% {
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m3.f2(sp.db)
  
  if(i %in% thousand){
    send_text(paste("finishing model m3.f2", i, "of 100", Sys.time(), sep = " "))
  } 
  
}

out.m3.f2 <- ldply(risk.m3.f2)
out.m3.f2$binomial <- rep(rownames(risk.m3.f2[[1]]), length(risk.m3.f2))
out.m3.f2$sim <- rep(1:length(risk.m3.f2), each=nrow(risk.m3.f2[[1]]))

out <- out.m3.f2 #prep for analysis/graphics scripts
risk <- risk.m3.f2 #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
source('01_R/metrics.R')
m3.f2.survived <- data.frame(survived)
m3.f2.diversity <- data.frame(diversity)
m3.f2.p.risk.ch <- data.frame(out.change)
m3.f2.habs <- data.frame(habs)
m3.f2.risk <- data.frame(out.rare)
m3.f2.improved <- data.frame(sp.imp)

#Model 3 f3: Rarest with funding at 1/5 and varying by how many spp are endangered/extinct
source('01_R/model.prep.R')
source('01_R/models_funds/m3.f3.R')
risk.m3.f3 <- foreach(i = 1:n) %dopar% {
  if(i %in% thousand){
    send_text(paste("starting model m3.f3", i, "of 100", Sys.time(), sep = " "))
  } 
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m3.f3(sp.db)
}

out.m3.f3 <- ldply(risk.m3.f3)
out.m3.f3$binomial <- rep(rownames(risk.m3.f3[[1]]), length(risk.m3.f3))
out.m3.f3$sim <- rep(1:length(risk.m3.f3), each=nrow(risk.m3.f3[[1]]))

out <- out.m3.f3 #prep for analysis/graphics scripts
risk <- risk.m3.f3 #prep for analysis/graphics scripts


#calculate the percentage of spp that went extinct & remaining diversity
source('01_R/metrics.R')
m3.f3.survived <- data.frame(survived)
m3.f3.diversity <- data.frame(diversity)
m3.f3.p.risk.ch <- data.frame(out.change)
m3.f3.habs <- data.frame(habs)
m3.f3.risk <- data.frame(out.rare)
m3.f3.improved <- data.frame(sp.imp)

###### Model 8 f2: Faith's Edge
source('01_R/model.prep.R')
source('01_R/models_funds/m8.f2.f2.R')
risk.m8.f2.f2 <- foreach(i = 1:n) %dopar% {
  if(i %in% thousand){
    send_text(paste("starting model m8.f2.f2", i, "of 100", Sys.time(), sep = " "))
  } 
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m8.f2.f2(sp.db)
}

out.m8.f2.f2 <- ldply(risk.m8.f2.f2)
out.m8.f2.f2$binomial <- rep(rownames(risk.m8.f2.f2[[1]]), length(risk.m8.f2.f2))
out.m8.f2.f2$sim <- rep(1:length(risk.m8.f2.f2), each=nrow(risk.m8.f2.f2[[1]]))

out <- out.m8.f2.f2 #prep for analysis/graphics scripts
risk <- risk.m8.f2.f2 #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
source('01_R/metrics.R')
m8.f2.survived <- data.frame(survived)
m8.f2.diversity <- data.frame(diversity)
m8.f2.p.risk.ch <- data.frame(out.change)
m8.f2.habs <- data.frame(habs)
m8.f2.risk <- data.frame(out.rare)
m8.f2.improved <- data.frame(sp.imp)


