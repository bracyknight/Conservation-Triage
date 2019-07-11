##m3: Prioritizing Conservation Based Rarest Spp Only

m3.f3 <- function(db){
  #Calculate Available Funding 
  
  for(i in 1:(length(years)-1)){
    #Remaining Species Possible for Funding & ED Tree
    extinct <- rownames(sp.db)[which(sp.db[,i]>5)]
    at.risk <- data.frame(sp.db[which(sp.db[,i]>1&sp.db[,i]<6),i]) #subset spp to those at risk of extinction this turn
    names(at.risk) <- 'iucn.no'
    
    funds <-round(sum(sp.db[,i]>1)/5,0) #### THIS IS WHAT IS NEW/UNIQUE ABOUT THIS MODEL
    
    funding <- (rep(c(1,0),c(funds,(length(at.risk$iucn.no)-funds))))
    
    #Prioritization section
    at.risk$binomial <- row.names(at.risk) #assigns names
    at.risk <- at.risk[sample(nrow(at.risk)),] #randomizes order (so that prioritization gets randomized by risk classes)
    at.risk <- at.risk[order(-at.risk$iucn.no),] #sorts by risk class (but doesn't override previous random order by classes)
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
