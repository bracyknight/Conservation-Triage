##m7: Prioritizing Conservation Faith's Index

m7 <- function(db){
  
  for(i in 1:(length(years)-1)){
    #1 DETERMINE WHICH SPECIES ARE ELIGIBLE FOR FUNDING
    #Remaining Species Possible for Funding & ED Tree
    extinct <- rownames(sp.db)[which(sp.db[,i]>5)]
    at.risk <- data.frame(sp.db[which(sp.db[,i]>1&sp.db[,i]<6),i]) #subset spp to those at risk of extinction this turn
    names(at.risk) <- 'iucn.no'
    funding <- (rep(c(1,0),c(funds,(length(at.risk$iucn.no)-funds))))
    
    #2 PRIORITIZE CONSERVATION (THIS IS THE MAIN PART THAT VARIES BY-MODEL)
    ## FOR MODEL m7, THIS IS A FAITH'S INDEX PHYLOGENETIC PRIORITIZATION 
    
    #Create subset tree
    tree.m<-drop.tip(tree,tree$tip.label[match(extinct, tree$tip.label)])
    #1. Calculate Faith's Index for all trees that result from 1 species going extinct
    comm3 <- matrix(nrow=length(tree.m$tip.label),ncol = length(tree.m$tip.label))
    colnames(comm3) <-tree.m$tip.label
    
    for(m in 1:length(tree.m$tip.label)){
      comm3[m,] <- rep(1, length(tree.m$tip.label))
      comm3[m,m] <- 0
    }
    
    #comm3 <- data.frame(comm3)
    faiths <- pd(comm3, tree.m, include.root=TRUE)
    faiths$binomial <- tree.m$tip.label
    faiths$SR <- NULL
    rownames(faiths) <- faiths$binomial
    
    #2. merge back with binomial and iucn
    test.sp <- merge(faiths,at.risk, by='row.names')
    #View(test.sp)
    test.sp[1] <- NULL
    #3. remove non-at risk spp
    test.sp <- test.sp[which(test.sp$iucn.no>1 & test.sp$iucn.no<6),]
    #4. sort test.sp in order of faiths
    test.sp <- test.sp[order(test.sp[,1]),]
    
    #5. merge with available funding
      ## This part is keeps the db in the order sorted by prioritization and then adds funding from top to bottom until it runs out
    risk.1 <- cbind(test.sp, funding)
    #6. create new risk for this round
    risk.1$new.risk <- risk.1$iucn.no - risk.1$funding
    risk.1 <- risk.1[,c(2,5)]
    rownames(risk.1) <- risk.1$binomial
    #7. bring back in all IUCN 1 spp
    risk.2 <- merge(data.frame(sp.db[,i]), risk.1, by = 'row.names', all=T)
    risk.2[,3] <- NULL
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


