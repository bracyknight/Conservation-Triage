library(foreach)
library(doMC)
registerDoMC(7)  #change the 2 to your number of CPU cores  
options(cores=7)

nm <- list('binomial','years',seq(1:100))
sp.db <- matrix(nrow=409,ncol=16, dimnames = list(db$binomial,years))
dimnames(sp.db)
sp.db[,1] <- db$iucn.no
sp.db <- sp.db[ order(row.names(sp.db)), ]
na_count <-sapply(sp.db, function(y) sum(is.na(y)))
thousand <- seq(1000,10000, by = 1000)
thousand <- c(1, thousand)

send_text = function(message){
  system(paste('osascript -e \'tell application "Messages"\' -e \'send "',
               message, '" to buddy "303-999-6726" of (service 1 whose service type is iMessage)\' -e \'end tell\''))
}

#risk.db needs to be an multi-level array with a level for each iteration

risky <- function(db){
  for(i in 1:15){
    #Remaining Species Possible for Funding
    at.risk <- sp.db[which(sp.db[,i]>1&sp.db[,i]<6),i] #subset spp to those at risk of extinction this turn
    
    #Calculate Available Funding 
    funds <- round(length(at.risk)/10,0) #funding to raise each species by one IUCN class based on McCarthy et al 2012
    funds <- (rep(c(1,0),c(funds,length(at.risk)-funds)))
    
    #Prioritization section
    funds <- sample(funds)
    at.risk <- at.risk - funds
    risk.i <- merge(sp.db[,i], at.risk, by= 'row.names', all= T)
    risk.i[is.na(risk.i$y),3] <- risk.i[is.na(risk.i$y),2]
    risk.i[,2] <- NULL
    
    #calculate change in status
    rando <- sample(1:6, length(risk.i[,2]), replace = T)# calculate the random effect
    new.risk <- ifelse(rando>=risk.i[,2] ,risk.i[,2]+1,risk.i[,2])
    sp.db[,i+1] <- new.risk #assign newly calculated species risk to next column (time + 1)
  }
return(sp.db)
}


riskloop <- foreach(i = 1:10000) %dopar% {
  
  sp.db <- matrix(nrow=409,ncol=16, dimnames = list(db$binomial,years))
  dimnames(sp.db)
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]

  if(i %in% hundred){
    send_text(paste("finishing model", i, "of 10000", Sys.time(), sep = " "))
  }   
  
  risky(sp.db)
}

typeof(riskloop)
unlist(riskloop)



riskloop[1]

###Unused right now. convert list to array
