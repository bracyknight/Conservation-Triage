#MASTER CODE TO RUN ALL MODELS

library(phytools)
library(ape)
library(stringr)
library(foreach)
library(doMC)
library(picante)
library(ape)
library(tidyverse)
library(plyr)
library(ggplot2); theme_set(theme_bw())

registerDoMC(7)  #change the 2 to your number of CPU cores  
options(cores=7)

#How many iterations do you want to do?
n =  1000 #number of iterations
y = 16 #number of decades (16 for 2160, 11 for 2110)
init.risk <- data.frame(db$binomial,db$iucn.no)
  
##### Model 1: Control: No Funding at all

source('01_R/model.prep.cf.R')
source('01_R/models/m1.R')
risk.m1 <- foreach(i = 1:n) %dopar% {
  #if(i %in% thousand){
  #  send_text(paste("starting model m1", i, "of",n, Sys.time(), sep = " "))
  #} 
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m1(sp.db)
}

out.m1 <- ldply(risk.m1)
out.m1$binomial <- rep(rownames(risk.m1[[1]]), length(risk.m1))
out.m1$sim <- rep(1:length(risk.m1), each=nrow(risk.m1[[1]]))

out <- out.m1 #prep for analysis/graphics scripts
risk <- risk.m1 #prep for analysis/graphics scripts


#calculate the percentage of spp that went extinct & remaining diversity
model <- "m1.cf"
source('01_R/metrics.R')
source('01_R/fam.gen.R')
source('01_R/reg.div.stats.R')


############# MODEL 2: RANDOM FUNDING ###############
source('01_R/model.prep.cf.R')
source('01_R/models/m2.R')
risk.m2 <- foreach(i = 1:n) %dopar% {
  if(i %in% thousand){
    send_text(paste("starting model m2", i, "of",n, Sys.time(), sep = " "))
  }
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m2(sp.db)

}

out.m2 <- ldply(risk.m2)
out.m2$binomial <- rep(rownames(risk.m2[[1]]), length(risk.m2))
out.m2$sim <- rep(1:length(risk.m2), each=nrow(risk.m2[[1]]))

out <- out.m2 #prep for analysis/graphics scripts
risk <- risk.m2 #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
model <- "m2.cf"
source('01_R/metrics.R')



##### #Model 3: Rarest #################
source('01_R/model.prep.cf.R')
source('01_R/models/m3.R')
risk.m3.cf <- foreach(i = 1:n) %dopar% {
  if(i %in% thousand){
  send_text(paste("starting model m3", i, "of",n, Sys.time(), sep = " "))
  } 
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m3(sp.db)
}

out.m3.cf <- ldply(risk.m3.cf)
out.m3.cf$binomial <- rep(rownames(risk.m3.cf[[1]]), length(risk.m3.cf))
out.m3.cf$sim <- rep(1:length(risk.m3.cf), each=nrow(risk.m3.cf[[1]]))

out <- out.m3.cf #prep for analysis/graphics scripts
risk <- risk.m3.cf #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
model <- "m3.cf"
source('01_R/metrics.cf.R')


##### Model 4: Evolutionarily Distinct
source('01_R/model.prep.cf.R')
source('01_R/models/m4.R')
risk.m4 <- foreach(i = 1:n) %dopar% {
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m4(sp.db)
  
  #if(i %in% thousand){
  #  send_text(paste("finishing model m4", i, "of",n, Sys.time(), sep = " "))
  #} 
}

out.m4 <- ldply(risk.m4)
out.m4$binomial <- rep(rownames(risk.m4[[1]]), length(risk.m4))
out.m4$sim <- rep(1:length(risk.m4), each=nrow(risk.m4[[1]]))

out <- out.m4 #prep for analysis/graphics scripts
risk <- risk.m4 #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
model <- "m4.cf"
source('01_R/metrics.R')


##### Model 5: EDGE
source('01_R/model.prep.cf.R')
source('01_R/models/m5.R')
risk.m5 <- foreach(i = 1:n) %dopar% {
  if(i %in% thousand){
    send_text(paste("beginning model m5", i, "of",n, Sys.time(), sep = " "))
  }
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m5(sp.db)
  
  
}

out.m5 <- ldply(risk.m5)
out.m5$binomial <- rep(rownames(risk.m5[[1]]), length(risk.m5))
out.m5$sim <- rep(1:length(risk.m5), each=nrow(risk.m5[[1]]))

out <- out.m5 #prep for analysis/graphics scripts
risk <- risk.m5 #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
model <- "m5.cf"
source('01_R/metrics.R')



##### Model 6: Commonest Spp
source('01_R/model.prep.cf.R')
source('01_R/models/m6.R')
risk.m6 <- foreach(i = 1:n) %dopar% {
  if(i %in% thousand){
    send_text(paste("beginning model m6", i, "of",n, Sys.time(), sep = " "))
  }
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m6(sp.db)
}

out.m6 <- ldply(risk.m6)
out.m6$binomial <- rep(rownames(risk.m6[[1]]), length(risk.m6))
out.m6$sim <- rep(1:length(risk.m6), each=nrow(risk.m6[[1]]))

out <- out.m6 #prep for analysis/graphics scripts
risk <- risk.m6 #prep for analysis/graphics scripts

#calculate the percentage of spp that went extinct & remaining diversity
model <- "m6.cf"
source('01_R/metrics.R')


###### Model 7: Faith's Index
source('01_R/model.prep.cf.R')
source('01_R/models/m7.R')
risk.m7 <- foreach(i = 1:n) %dopar% {
  if(i %in% thousand){
    send_text(paste("beginning model m7", i, "of 100", Sys.time(), sep = " "))
  }
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  #dimnames(sp.db)
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m7(sp.db)
   
}

out.m7 <- ldply(risk.m7)
out.m7$binomial <- rep(rownames(risk.m7[[1]]), length(risk.m7))
out.m7$sim <- rep(1:length(risk.m7), each=nrow(risk.m7[[1]]))

out <- out.m7 #prep for analysis/graphics scripts
risk <- risk.m7 #prep for analysis/graphics scripts


#calculate the percentage of spp that went extinct & remaining diversity
model <- "m7.cf"
source('01_R/metrics.R')



######## Model 8: Faith's Edge
source('01_R/model.prep.cf.R')
source('01_R/models/m8.R')
risk.m8 <- foreach(i = 1:n) %dopar% {
  #if(i %in% thousand){
  #  send_text(paste("beginning model m8s", i, "of 100", Sys.time(), sep = " "))
  #}
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m8(sp.db)
}

out.m8 <- ldply(risk.m8)
out.m8$binomial <- rep(rownames(risk.m8[[1]]), length(risk.m8))
out.m8$sim <- rep(1:length(risk.m8), each=nrow(risk.m8[[1]]))

out <- out.m8 #prep for analysis/graphics scripts
risk <- risk.m8 #prep for analysis/graphics scripts


#calculate the percentage of spp that went extinct & remaining diversity
model <- "m8.cf"
source('01_R/metrics.R')




######## Model 9: Prioritizing Conservation Based c('4','3','2','5')
source('01_R/model.prep.cf.R')
source('01_R/models/m9.R')
risk.m9 <- foreach(i = 1:n) %dopar% {
  #if(i %in% thousand){
  #  send_text(paste("beginning model m8s", i, "of 100", Sys.time(), sep = " "))
  #}
  
 sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m9(sp.db)
}

out.m9 <- ldply(risk.m9)
out.m9$binomial <- rep(rownames(risk.m9[[1]]), length(risk.m9))
out.m9$sim <- rep(1:length(risk.m9), each=nrow(risk.m9[[1]]))

out <- out.m9 #prep for analysis/graphics scripts
risk <- risk.m9 #prep for analysis/graphics scripts


#calculate the percentage of spp that went extinct & remaining diversity
model <- "m9.cf"
source('01_R/metrics.R')

  

######## Model 11: Prioritizing based on smallest HPD change over 100 years
source('01_R/model.prep.cf.R')
source('01_R/models/m11.R')
risk.m11 <- foreach(i = 1:n) %dopar% {
  #if(i %in% thousand){
  #  send_text(paste("beginning model m8s", i, "of 100", Sys.time(), sep = " "))
  #}
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m11(sp.db)
}

out.m11 <- ldply(risk.m11)
out.m11$binomial <- rep(rownames(risk.m11[[1]]), length(risk.m11))
out.m11$sim <- rep(1:length(risk.m11), each=nrow(risk.m11[[1]]))

out <- out.m11 #prep for analysis/graphics scripts
risk <- risk.m11 #prep for analysis/graphics scripts


#calculate the percentage of spp that went extinct & remaining diversity
source('01_R/metrics.R')
m11.survived <- data.frame(survived)
m11.diversity <- data.frame(diversity)
m11.p.risk.ch <- data.frame(out.change)
m11.habs <- data.frame(habs)
m11.risk <- data.frame(out.rare)
m11.improved <- data.frame(sp.imp)

######## Model 12: Prioritizing Habitat Specialists
source('01_R/model.prep.cf.R')
source('01_R/models/m12.R')
risk.m12 <- foreach(i = 1:n) %dopar% {
  #if(i %in% thousand){
  #  send_text(paste("beginning model m8s", i, "of 100", Sys.time(), sep = " "))
  #}
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m12(sp.db)
}

out.m12 <- ldply(risk.m12)
out.m12$binomial <- rep(rownames(risk.m12[[1]]), length(risk.m12))
out.m12$sim <- rep(1:length(risk.m12), each=nrow(risk.m12[[1]]))

out <- out.m12 #prep for analysis/graphics scripts
risk <- risk.m12 #prep for analysis/graphics scripts


#calculate the percentage of spp that went extinct & remaining diversity
source('01_R/metrics.R')
m12.survived <- data.frame(survived)
m12.diversity <- data.frame(diversity)
m12.p.risk.ch <- data.frame(out.change)
m12.habs <- data.frame(habs)
m12.risk <- data.frame(out.rare)
m12.improved <- data.frame(sp.imp)


######## Model 13: Prioritizing Habitat Generalists
source('01_R/model.prep.cf.R')
source('01_R/models/m13.R')
risk.m13 <- foreach(i = 1:n) %dopar% {
  #if(i %in% thousand){
  #  send_text(paste("beginning model m8s", i, "of 100", Sys.time(), sep = " "))
  #}
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m13(sp.db)
}

out.m13 <- ldply(risk.m13)
out.m13$binomial <- rep(rownames(risk.m13[[1]]), length(risk.m13))
out.m13$sim <- rep(1:length(risk.m13), each=nrow(risk.m13[[1]]))

out <- out.m13 #prep for analysis/graphics scripts
risk <- risk.m13 #prep for analysis/graphics scripts


#calculate the percentage of spp that went extinct & remaining diversity
source('01_R/metrics.R')
m13.survived <- data.frame(survived)
m13.diversity <- data.frame(diversity)
m13.p.risk.ch <- data.frame(out.change)
m13.habs <- data.frame(habs)
m13.risk <- data.frame(out.rare)
m13.improved <- data.frame(sp.imp)


######## Model 14: Highest Current HPD
source('01_R/model.prep.cf.R')
source('01_R/models/m14.R')
risk.m14 <- foreach(i = 1:n) %dopar% {
  #if(i %in% thousand){
  #  send_text(paste("beginning model m8s", i, "of 100", Sys.time(), sep = " "))
  #}
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m14(sp.db)
}

out.m14 <- ldply(risk.m14)
out.m14$binomial <- rep(rownames(risk.m14[[1]]), length(risk.m14))
out.m14$sim <- rep(1:length(risk.m14), each=nrow(risk.m14[[1]]))

out <- out.m14 #prep for analysis/graphics scripts
risk <- risk.m14 #prep for analysis/graphics scripts


#calculate the percentage of spp that went extinct & remaining diversity
source('01_R/metrics.R')
m14.survived <- data.frame(survived)
m14.diversity <- data.frame(diversity)
m14.p.risk.ch <- data.frame(out.change)
m14.habs <- data.frame(habs)
m14.risk <- data.frame(out.rare)
m14.improved <- data.frame(sp.imp)








##### Model 5.f1: EDGE + funding remains 10% extant spp at risk
source('01_R/model.prep.cf.R')
source('01_R/models/m5.f1.R')
risk.m5.f1 <- foreach(i = 1:n) %dopar% {
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m5.f1(sp.db)
  
  if(i %in% thousand){
    send_text(paste("finishing model m5", i, "of",n, Sys.time(), sep = " "))
  } 
}

out.m5.f1 <- ldply(risk.m5.f1)
out.m5.f1$binomial <- rep(rownames(risk.m5.f1[[1]]), length(risk.m5.f1))
out.m5.f1$sim <- rep(1:length(risk.m5.f1), each=nrow(risk.m5.f1[[1]]))

out <- out.m5.f1 #prep for analysis/graphics scripts
risk <- risk.m5.f1 #prep for analysis/graphics scripts


#calculate the percentage of spp that went extinct & remaining diversity
source('01_R/graphics.prep.R')
m5.f1.survived <- data.frame(survived)
m5.f1.diversity <- data.frame(diversity)

#####

######## Repeat above models, but allow for funding to reflect endangered and extinct

####### Model 3 f2: Rarest
source('01_R/model.prep.cf.R')
source('01_R/models/m3.R')
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
source('01_R/graphics.prep.R')
m3.f2.survived <- data.frame(survived)
m3.f2.diversity <- data.frame(diversity)


#Model 3 f3: Rarest with funding at 1/5 and varying by how many spp are endangered/extinct
source('01_R/model.prep.cf.R')
source('01_R/models/m3.f3.R')
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
source('01_R/graphics.prep.R')
m3.f3.survived <- data.frame(survived)
m3.f3.diversity <- data.frame(diversity)



###### Model 8 f2: Faith's Edge
source('01_R/model.prep.cf.R')
source('01_R/models/m8.f2.R')
risk.m8.f2 <- foreach(i = 1:n) %dopar% {
  if(i %in% thousand){
    send_text(paste("starting model m8.f2", i, "of 100", Sys.time(), sep = " "))
  } 
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m8.f2(sp.db)
}

out.m8.f2 <- ldply(risk.m8.f2)
out.m8.f2$binomial <- rep(rownames(risk.m8.f2[[1]]), length(risk.m8.f2))
out.m8.f2$sim <- rep(1:length(risk.m8.f2), each=nrow(risk.m8.f2[[1]]))

out <- out.m8.f2 #prep for analysis/graphics scripts
risk <- risk.m8.f2 #prep for analysis/graphics scripts


#calculate the percentage of spp that went extinct & remaining diversity
source('01_R/graphics.prep.R')
m8.f2.survived <- data.frame(survived)
m8.f2.diversity <- data.frame(diversity)





