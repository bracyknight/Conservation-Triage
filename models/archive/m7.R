##m7: Prioritizing Conservation Faith's Index


library(ggplot2)
library(foreach)
library(doMC)
library(picante)
library(ape)
library(plyr)
library(dplyr)
registerDoMC(7)  #change the 2 to your number of CPU cores  
options(cores=7)

# pr(ext) for each 10-year timestep
# this assumes uniform pr(extinction) through all timesteps
prExt.100 <- c("LC"=0.0000, "NT"=0.00, "V"=0.1, "En"=0.667, "CE"=0.999, "EX" = 0)
prExt.10 <- 1 - (1-prExt.100)^(1/10)


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


#calculate initial dfunding
at.risk <- sp.db[which(sp.db[,1]>1&sp.db[,1]<6),1] #subset spp to those at risk of extinction this turn
funds <- round(length(at.risk)/10,0) #funding to raise each species by one IUCN class based on McCarthy et al 2012

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
    
    for(j in 1:length(tree.m$tip.label)){
      comm3[j,] <- rep(1, length(tree.m$tip.label))
      comm3[j,j] <- 0
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
    
    
    #3 CALCULATE SPECIES EXCTINCTION FOR THIS ROUND 
    # rbinom using category-based extinction probabilities
    ext.step <- rbinom(length(risk.2[,2]), size=1, prob=prExt.10[risk.2[,2]])
    risk.2$out[ext.step==1] <- 6   # these species went extinct :(
    
    
    #4 CALCULATE SPECIES THAT INCREASE IN RISK
    #1 Sum up the steps in risk class already taken due to extinction in 3 above
    #this is the number of extinctions * the number of classes each species took to become extinct
    #e.g. if a sp was risk class 3 and went extinct, it is weighted as 3: 1 for transition to 4, 1 for trans to 5, 1 for extinction
    w.c <- risk.2[which(risk.2$out ==6),2] #grabs the species that went extinct
    w.c <- 6 - w.c #tells how many "steps up" in endangerment species that went extinct made
    w.c <- sum(w.c) #sums those steps
    
    #2. calculate the number of spp that SHOULD go up in risk based on RLI (Red List Index)
    up.risk <- round(sum(risk.2$new.risk<6)*0.35,0)#total number of remaining extant spp in study * 0.35 
    #gives number of steps more endangerd to meet RLI
    
    #3. subtract the upsteps left after extinctions already in prior step
    rli <- up.risk - w.c #this is the number of "steps up" the remaining spp have to go
    
    #4 Assign all species that didn't go extinct a spot in the risk.2$out column (move status over)
    risk.2$out <- ifelse(is.na(risk.2[,3]), risk.2[,2], risk.2[,3] )
    
    #5 Determine the fate of species that DID NOT go extinct this turn (they may become more endangered)
    sp.might.incr <- filter(risk.2, out<5) #get list of spp that may go up in risk after extinction phase
    up.risk <- rbinom(length(sp.might.incr$out), 1, rli/length(risk.2$new.risk))
    
    #6. assign increased extinction risk randomly to all spp in new.risk NOT risk class 5 (or above)
    sp.might.incr$out <- sp.might.incr$out + up.risk # assigns increased risk values
    
    #7. bring the species that may have gotten worse back in with those that went extinct 
    risk.2 <- risk.2[which(risk.2$out>4),]
    risk.3 <- rbind(risk.2, sp.might.incr)
    
    #8. join these data back into the master database
    risk.3 <- risk.3[order(risk.3$binomial),c(1,3)] #put into alphabetical order to align up with original db
    sp.db[,i+1] <- risk.3$out #assign newly calculated species risk to next column (time + 1)
    
  }
  return(sp.db)
}



#m7(sp.db)

#View(sp.db)

risk.m7 <- foreach(i = 1:1000) %dopar% {
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=16, dimnames = list(db$binomial,years))
  #dimnames(sp.db)
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  
  m7(sp.db)
  
  if(i %in% thousand){
    send_text(paste("finishing model", i, "of 5000", Sys.time(), sep = " "))
  } 
}

####  NEW  #####################################################################
library(ggplot2); theme_set(theme_bw()); library(plyr); library(tidyr)
out.m7 <- ldply(risk.m7)
out.m7$binomial <- rep(rownames(risk.m7[[1]]), length(risk.m7))
out.m7$sim <- rep(1:length(risk.m7), each=nrow(risk.m7[[1]]))

ggplot(out.m7, aes(x=t2010, y=..prop..)) + geom_bar() 
ggplot(out.m7, aes(x=t2160, y=..prop..)) + geom_bar() 
ggplot(out.m7, aes(x=t2160-t2010, y=..prop..)) + geom_bar()

ggplot(filter(out.m7, t2160 != 6), aes(x=t2160-t2010, y=..prop..)) + geom_bar() + 
  labs(x="Change in status", y="Proportion of species-simulations", 
       title="Extant species only")
ggplot(filter(out.m7, t2160 == 6), aes(x=t2160-t2010, y=..prop..)) + geom_bar() +
  labs(x="Change in status", y="Proportion of species-simulations", 
       title="Extinct species only")

ggplot(out.m7, aes(x=t2160, y=..prop..)) + geom_bar() + facet_wrap(~t2010) + 
  labs(x="2160 status", y="Proportion of species-simulations", 
       title="All species (panels = 2010 status)")
ggplot(filter(out.m7, t2160 != 6), aes(x=t2160, y=..prop..)) + geom_bar() + facet_wrap(~t2010) + 
  labs(x="2160 status", y="Proportion of species-simulations", 
       title="Extant species (panels = 2010 status)")
ggplot(out.m7, aes(x=factor(t2010), fill=(t2160==6))) + geom_bar(position="fill") +
  labs(x="2010 status", y="Proportion of 2010 category extinct in 2160")
ggplot(out.m7, aes(x=(t2160==6))) + geom_bar() + facet_wrap(~t2010) + 
  labs(x="Extinct", y="Proportion of species-simulations", 
       title="All species (panels = 2010 status)")


timeComp.m7 <- gather(out.m7, timeStep, extRisk, 1:16)
timeCompSp.m7 <- ddply(timeComp.m7, .(binomial, timeStep), summarise,
                       mnRisk=mean(extRisk))
timeCompSp.m7$initRisk <- out.m7$t2010[match(timeCompSp.m7$binomial,
                                             out.m7$binomial)]
ggplot(timeCompSp.m7, aes(x=timeStep, y=mnRisk, group=binomial)) + 
  geom_line(alpha=0.2, colour="red3")

ggplot(timeCompSp.m7, aes(x=timeStep, y=mnRisk, 
                          group=binomial, colour=factor(initRisk))) + 
  geom_line(alpha=0.3, size=0.75) + scale_colour_brewer(type="qual", palette=3)

################################################################################

#calculate the percentage of spp that survived to the end
final.survive.m7 <- rep(NA, 1000)
for(i in 1:1000){
  final.survive.m7[i] <- length((out.m7$binomial)[which(out.m7$t2160<6 & out.m7$sim ==i)])/
    length(db$binomial)
}
hist(final.survive.m7)
#calculate percent phylogenetic diversity lost
diversity.m7 <- rep(NA,1000)
for(i in 1:1000){
  extinct <- (out.m7$binomial)[which(out.m7$t2160>5 & out.m7$sim ==i)]
  tree.f<-drop.tip(tree,tree$tip.label[match(extinct, tree$tip.label)])
  diversity.m7[i] <- sum(tree.f$edge.length)/sum(tree$edge.length) #total phylo diversity (branch lengnth of new tree) Called "Faith's Index"
  #total pgylo diversity (branch lenght of original tree)
  
}
hist(diversity.m7)
