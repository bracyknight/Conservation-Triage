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

#rm(list=ls(pattern="m5.d10"))

registerDoMC(7)  #change the 2 to your number of CPU cores  
options(cores=7)

##### #Model 2: Random Funding #################
#How many iterations do you want to do?
n =  1000 #number of iterations
y = 17 #number of decades (16 for 2170, 11 for 2120)

source('01_R/model.prep.d10.R')
source('01_R/models_delay/m2.d10.R')

risk.m2.d10 <- foreach(i = 1:n) %dopar% {
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db[,2] <- db$iucn.no #Change here
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m2.d10(sp.db) #Change here
}

out.m2.d10 <- ldply(risk.m2.d10)
out.m2.d10$binomial <- rep(rownames(risk.m2.d10[[1]]), length(risk.m2.d10))
out.m2.d10$sim <- rep(1:length(risk.m2.d10), each=nrow(risk.m2.d10[[1]]))
out <- out.m2.d10 #prep for analysis/graphics scripts
risk <- risk.m2.d10 #prep for analysis/graphics scripts
sp.db <- sp.db[,2:17] #unique for delayed funding models
out <- out[,2:19] #unique for delayed funding models
y <- 16 #unique for delayed funding models
model <- "m2.d10"
source('01_R/metrics.d10.R')
source('01_R/fam.gen.R')



##### #Model 3: Rarest #################
#How many iterations do you want to do?
n =  1000 #number of iterations
y = 17 #number of decades (16 for 2170, 11 for 2120)

source('01_R/model.prep.d10.R')
source('01_R/models_delay/m3.d10.R')

risk.m3.d10 <- foreach(i = 1:n) %dopar% {
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db[,2] <- db$iucn.no #Change here
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m3.d10(sp.db) #Change here
}

out.m3.d10 <- ldply(risk.m3.d10)
out.m3.d10$binomial <- rep(rownames(risk.m3.d10[[1]]), length(risk.m3.d10))
out.m3.d10$sim <- rep(1:length(risk.m3.d10), each=nrow(risk.m3.d10[[1]]))

out <- out.m3.d10 #prep for analysis/graphics scripts
risk <- risk.m3.d10 #prep for analysis/graphics scripts
sp.db <- sp.db[,2:17] #unique for delayed funding models
out <- out[,2:19] #unique for delayed funding models
y <- 16 #unique for delayed funding models
model <- "m3.d10"
source('01_R/metrics.d10.R')
source('01_R/fam.gen.R')

 
##### #Model 4: Evolutionary Distinct #################
#How many iterations do you want to do?
n =  1000 #number of iterations
y = 17 #number of decades (16 for 2170, 11 for 2120)

source('01_R/model.prep.d10.R')
source('01_R/models_delay/m4.d10.R')

risk.m4.d10 <- foreach(i = 1:n) %dopar% {
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1] <- db$iucn.no
  sp.db[,2] <- db$iucn.no #Change here
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m4.d10(sp.db) #Change here
}

out.m4.d10 <- ldply(risk.m4.d10)
out.m4.d10$binomial <- rep(rownames(risk.m4.d10[[1]]), length(risk.m4.d10))
out.m4.d10$sim <- rep(1:length(risk.m4.d10), each=nrow(risk.m4.d10[[1]]))

out <- out.m4.d10 #prep for analysis/graphics scripts
risk <- risk.m4.d10 #prep for analysis/graphics scripts
sp.db <- sp.db[,2:17] #unique for delayed funding models
out <- out[,2:19] #unique for delayed funding models
y <- 16 #unique for delayed funding models
model <- "m4.d10"
source('01_R/metrics.d10.R')
source('01_R/fam.gen.R')



##### Model 5: EDGE

#rm(list=ls(pattern="m5.d10"))

n =  1000 #number of iterations
y = 17 #number of decades (16 for 2170, 11 for 2120)
source('01_R/model.prep.d10.R')
source('01_R/models_delay/m5.d10.R')
model <- "m5.d10"

risk.m5.d10 <- foreach(i = 1:n) %dopar% {
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1:2] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m5.d10(sp.db)
}

out.m5.d10 <- ldply(risk.m5.d10)
out.m5.d10$binomial <- rep(rownames(risk.m5.d10[[1]]), length(risk.m5.d10))
out.m5.d10$sim <- rep(1:length(risk.m5.d10), each=nrow(risk.m5.d10[[1]]))

out <- out.m5.d10 #prep for analysis/graphics scripts
risk <- risk.m5.d10 #prep for analysis/graphics scripts
sp.db <- sp.db[,2:17] #unique for delayed funding models
out <- out[,2:19] #unique for delayed funding models
y <- 16 #unique for delayed funding models
model <- "m5.d10"
source('01_R/metrics.d10.R')
source('01_R/fam.gen.R')




#m7 Faith's Index

#rm(list=ls(pattern="m7.d10"))

n =  1000 #number of iterations
y = 17 #number of decades (16 for 2170, 11 for 2120)
source('01_R/model.prep.d10.R')
source('01_R/models_delay/m7.d10.R')
model <- "m7.d10"

risk.m7.d10 <- foreach(i = 1:n) %dopar% {
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1:2] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m7.d10(sp.db)
}

out.m7.d10 <- ldply(risk.m7.d10)
out.m7.d10$binomial <- rep(rownames(risk.m7.d10[[1]]), length(risk.m7.d10))
out.m7.d10$sim <- rep(1:length(risk.m7.d10), each=nrow(risk.m7.d10[[1]]))

out <- out.m7.d10 #prep for analysis/graphics scripts
risk <- risk.m7.d10 #prep for analysis/graphics scripts
sp.db <- sp.db[,2:17] #unique for delayed funding models
out <- out[,2:19] #unique for delayed funding models
y <- 16 #unique for delayed funding models
model <- "m7.d10"
source('01_R/metrics.d10.R')
source('01_R/fam.gen.R')


##### Model 8: Faith's EDGE

#rm(list=ls(pattern="m7.d10"))

n =  1000 #number of iterations
y = 17 #number of decades (16 for 2170, 11 for 2120)
source('01_R/model.prep.d10.R')
source('01_R/models_delay/m8.d10.R')
model <- "m8.d10"

risk.m8.d10 <- foreach(i = 1:n) %dopar% {
  sp.db <- matrix(nrow=length(db$binomial),ncol=length(years), dimnames = list(db$binomial,years))
  sp.db[,1:2] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  m8.d10(sp.db)
}

out.m8.d10 <- ldply(risk.m8.d10)
out.m8.d10$binomial <- rep(rownames(risk.m8.d10[[1]]), length(risk.m8.d10))
out.m8.d10$sim <- rep(1:length(risk.m8.d10), each=nrow(risk.m8.d10[[1]]))

out <- out.m8.d10 #prep for analysis/graphics scripts
risk <- risk.m8.d10 #prep for analysis/graphics scripts
sp.db <- sp.db[,2:17] #unique for delayed funding models
out <- out[,2:19] #unique for delayed funding models
y <- 16 #unique for delayed funding models
model <- "m8.d10"
source('01_R/metrics.d10.R')
source('01_R/fam.gen.R')






