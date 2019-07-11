##m6: Prioritizing Conservation Based COMMONEST Spp Only
library(ggplot2)
library(foreach)
library(doMC)
library(picante)
library(ape)
library(plyr)
library(dplyr)
registerDoMC(7)  #change the 2 to your number of CPU cores  
options(cores=7)

send_text = function(message){
  system(paste('osascript -e \'tell application "Messages"\' -e \'send "',
               message, '" to buddy "303-999-6726" of (service 1 whose service type is iMessage)\' -e \'end tell\''))
}


#Bring in data
db <- read.csv('data/ch.4.data.csv',stringsAsFactors = F)
db[1] <- NULL
db <- db[order(db$binomial),]
#sp.db <- data.frame(sp.db[order(row.names(sp.db)),])
sp.db <- data.frame(db)
sp.list <- data.frame(sp.db$binomial)
names(sp.list) <- 'binomial'
tree.m <- tree

years <- seq(2010,2160, by = 10)
years <- paste("t",years, sep = "")
nm <- list('binomial','years',seq(1:100))
sp.db <- matrix(nrow=length(db$binomial),ncol=16, dimnames = list(db$binomial,years))
dimnames(sp.db)
sp.db[,1] <- db$iucn.no
#sp.db <- sp.db[ order(row.names(sp.db)), ]
na_count <-sapply(sp.db, function(y) sum(is.na(y)))
thousand <- seq(0,5000, by = 500)
thousand <- c(1, thousand)
i <- 1

#calculate initial dfunding
at.risk <- sp.db[which(sp.db[,1]>1&sp.db[,1]<6),1] #subset spp to those at risk of extinction this turn
funds <- round(length(at.risk)/10,0) #funding to raise each species by one IUCN class based on McCarthy et al 2012


m6 <- function(db){
  #Calculate Available Funding 
  
  for(i in 1:(length(years)-1)){
    #Remaining Species Possible for Funding
    extinct <- rownames(sp.db)[which(sp.db[,i]>5)]
    at.risk <- data.frame(sp.db[which(sp.db[,i]>1&sp.db[,i]<6),i]) #subset spp to those at risk of extinction this turn
    names(at.risk) <- 'iucn.no'
    num.risk <- length(at.risk$iucn.no)-funds
    
    funding <- (rep(c(1,0),c(funds,num.risk)))
    
    #Prioritization section
    at.risk$binomial <- row.names(at.risk) #assigns names
    at.risk <- at.risk[sample(nrow(at.risk)),] #randomizes order (so that prioritization gets randomized by risk classes)
    at.risk.2 <- at.risk[order(at.risk$iucn.no),] #sorts by risk class (but doesn't override previous random order by classes)
    #at.risk$binomial <- NULL
    
    
    #5. merge with available funding
    risk.1 <- cbind(at.risk, funding)
    
    #6. create new risk for this round
    risk.1$new.risk <- risk.1$iucn.no - risk.1$funding
    risk.1 <- data.frame(risk.1[,c(2,4)]) #changed
    risk.1$binomial<- NULL
    #7. bring back in all IUCN 1 spp
    risk.2 <- merge(data.frame(sp.db[,i]), risk.1, by = 'row.names', all=T)
    #risk.2[,3] <- NULL
    risk.2[,4] <- ifelse(is.na(risk.2[,3]),risk.2[,2],risk.2[,3])
    risk.2 <- risk.2[,c(1,4)]
    names(risk.2) <- c('binomial','new.risk')
    
    ####### NEW STUFF #######
    k1 <- t.mat[risk.2[,2],]
    k2 <- rep(NA,length(k1[,1]))
    for(j in 1:length(k1[,1])){
      k2[j] <- which(rmultinom(1,1,k1[j,])==1)  
    }
    
    risk.2 <- cbind(risk.2,k2)
    sp.db[,i+1] <- risk.2$k2 #assign newly calculated species risk to next column (time + 1)
    
  }
  return(sp.db)
}

