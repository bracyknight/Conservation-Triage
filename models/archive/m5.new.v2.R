##M5: Prioritizing Conservation Based on "EDGE"
  #EDGE = ln(1 + ED) + GE  * ln(2) 


library(ggplot2)
library(foreach)
library(doMC)
library(picante)
library(ape)
library(plyr)
library(dplyr)
registerDoMC(7)  #change the 2 to your number of CPU cores  
options(cores=7)

####  NEW  #####################################################################
# pr(ext) for each 10-year timestep
# this assumes uniform pr(extinction) through all timesteps
prExt.100 <- c("LC"=0.0000, "NT"=0.00, "V"=0.1, "En"=0.667, "CE"=0.999, "EX" = 0)
prExt.10 <- 1 - (1-prExt.100)^(1/10)
################################################################################
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

m5 <- function(db){
  #Calculate Available Funding 
  
  for(i in 1:(length(years)-1)){
    #Remaining Species Possible for Funding & ED Tree
    extinct <- rownames(sp.db)[which(sp.db[,i]>5)]
    at.risk <- data.frame(sp.db[which(sp.db[,i]>1&sp.db[,i]<6),i]) #subset spp to those at risk of extinction this turn
    names(at.risk) <- 'iucn.no'
    
    funding <- (rep(c(1,0),c(funds,(length(at.risk$iucn.no)-funds))))
    
    #Prioritization section
    #Create subset tree
    tree.m<-drop.tip(tree,tree$tip.label[match(extinct, tree$tip.label)])
      #1. calculate the phylogenetic breadth
    distinctness<-evol.distinct(tree.m,type="fair.proportion")
    names(distinctness) <- c('binomial','ed')
    rownames(distinctness) <- distinctness$binomial
      #2. merge back with binomial and iucn
    test.sp <- merge(distinctness,at.risk, by='row.names')
    #View(test.sp)
    test.sp[1] <- NULL
      #3. remove non-at risk spp
    test.sp <- test.sp[which(test.sp$iucn.no>1 & test.sp$iucn.no<6),]
      #4. sort test.sp in order of ed.
    test.sp <- test.sp[order(-test.sp[,2]),]
    test.sp$ge <- test.sp$iucn.no - 1
    test.sp$edge <- log(1 + test.sp$ed) + test.sp$ge*log(2)
    test.sp <- test.sp[order(-test.sp[,5]),]


      #5. merge with available funding
    risk.1 <- cbind(test.sp, funding)
      #6. create new risk for this round
    risk.1$new.risk <- risk.1$iucn.no - risk.1$funding
    risk.1 <- risk.1[,c(1,7)]
    rownames(risk.1) <- risk.1$binomial
      #7. bring back in all IUCN 1 spp
    risk.2 <- merge(data.frame(sp.db[,i]), risk.1, by = 'row.names', all=T)
    risk.2[,3] <- NULL
    risk.2[,4] <- ifelse(is.na(risk.2[,3]),risk.2[,2],risk.2[,3])
    risk.2 <- risk.2[,c(1,4)]
    names(risk.2) <- c('binomial','new.risk')
    
    
    
####  NEW  #####################################################################
    
    #calculate spp that go extinct
    # rbinom using category-based extinction probabilities
    ext.step <- rbinom(length(risk.2[,2]), size=1, prob=prExt.10[risk.2[,2]])
    risk.2$out[ext.step==1] <- 6   # these species went extinct :(
    
    #do this first because the risk-increaser function below should not supercede the extinction function
    
    #calculate species that become more endangered
    #1 count the weighted number of extinctions already this phase
        #this is the number of extinctions * the number of classes each species took to become extinct
        #e.g. if a sp was risk class 3 and went extinct, it is weighted as 3: 1 for transition to 4, 1 for trans to 5, 1 for extinction
    w.c <- risk.2[which(risk.2$out ==6),2] #grabs the species that went extinct
    w.c <- 6 - w.c #tells how many "steps up" in endangerment species that went extinct made
    w.c <- sum(w.c) #sums those steps
    
    
    #2. calculate the number of spp that SHOULD go up in risk back on RLI
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
    
    
    #incr.poss <- rep(1:length(sp.might.incr$out), times=(6-sp.might.incr$new.risk))
    #incr.act <- table(sample(incr.poss, rli, replace=FALSE))
    #incr.act.ind <- as.numeric(names(incr.act))
   # sp.might.incr$out[incr.act.ind] <- sp.might.incr$out[incr.act.ind] + incr.act    
 
    #5 Determine the fate of species that DID NOT go extinct this turn (they may become more endangered)
    #sp.might.incr <- risk.2[which(risk.2$out<5),] #get list of spp that may go up in risk after extinction phase
    #up.risk <- rbinom(length(sp.might.incr$out),1,0.35) #generate random list of species getting 1 more endangered
    #up.risk <- rbinom(length(sp.might.incr$out), 1, rli/length(risk.2$new.risk))
      
    #6. assign increased extinction risk randomly to all spp in new.risk NOT risk class 5 (or above)
    #sp.might.incr$out <- sp.might.incr$out + up.risk # assigns increased risk values
    
    #7. bring the species that may have gotten worse back in with those that went extinct 
    risk.2 <- risk.2[which(risk.2$out>4),]
    risk.3 <- rbind(risk.2, sp.might.incr)
    
    #8. join these data back into the master database
    risk.3 <- risk.3[order(risk.3$binomial),c(1,3)] #put into alphabetical order to align up with original db
    sp.db[,i+1] <- risk.3$out #assign newly calculated species risk to next column (time + 1)
    
  }
  return(sp.db)
}



#m5(sp.db)

#View(sp.db)

risk.m5 <- foreach(i = 1:100) %dopar% {
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=16, dimnames = list(db$binomial,years))
  #dimnames(sp.db)
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  
  m5(sp.db)
  
  #if(i %in% thousand){
  #  send_text(paste("finishing model", i, "of 5000", Sys.time(), sep = " "))
  #} 
}

####  NEW  #####################################################################
library(ggplot2); theme_set(theme_bw()); library(plyr); library(tidyr)
out.m5 <- ldply(risk.m5)
out.m5$binomial <- rep(rownames(risk.m5[[1]]), length(risk.m5))
out.m5$sim <- rep(1:length(risk.m5), each=nrow(risk.m5[[1]]))

ggplot(out.m5, aes(x=t2010, y=..prop..)) + geom_bar() 
ggplot(out.m5, aes(x=t2160, y=..prop..)) + geom_bar() 
ggplot(out.m5, aes(x=t2160-t2010, y=..prop..)) + geom_bar()

ggplot(filter(out.m5, t2160 != 6), aes(x=t2160-t2010, y=..prop..)) + geom_bar() + 
  labs(x="Change in status", y="Proportion of species-simulations", 
       title="Extant species only")
ggplot(filter(out.m5, t2160 == 6), aes(x=t2160-t2010, y=..prop..)) + geom_bar() +
  labs(x="Change in status", y="Proportion of species-simulations", 
       title="Extinct species only")

ggplot(out.m5, aes(x=t2160, y=..prop..)) + geom_bar() + facet_wrap(~t2010) + 
  labs(x="2160 status", y="Proportion of species-simulations", 
       title="All species (panels = 2010 status)")
ggplot(filter(out.m5, t2160 != 6), aes(x=t2160, y=..prop..)) + geom_bar() + facet_wrap(~t2010) + 
  labs(x="2160 status", y="Proportion of species-simulations", 
       title="Extant species (panels = 2010 status)")
ggplot(out.m5, aes(x=factor(t2010), fill=(t2160==6))) + geom_bar(position="fill") +
  labs(x="2010 status", y="Proportion of 2010 category extinct in 2160")
ggplot(out.m5, aes(x=(t2160==6))) + geom_bar() + facet_wrap(~t2010) + 
  labs(x="Extinct", y="Proportion of species-simulations", 
       title="All species (panels = 2010 status)")


timeComp.m5 <- gather(out.m5, timeStep, extRisk, 1:16)
timeCompSp.m5 <- ddply(timeComp.m5, .(binomial, timeStep), summarise,
                       mnRisk=mean(extRisk))
timeCompSp.m5$initRisk <- out.m5$t2010[match(timeCompSp.m5$binomial,
                                             out.m5$binomial)]
ggplot(timeCompSp.m5, aes(x=timeStep, y=mnRisk, group=binomial)) + 
  geom_line(alpha=0.2, colour="red3")

ggplot(timeCompSp.m5, aes(x=timeStep, y=mnRisk, 
                          group=binomial, colour=factor(initRisk))) + 
  geom_line(alpha=0.3, size=0.75) + scale_colour_brewer(type="qual", palette=3)

################################################################################

#calculate the percentage of spp that went extinct
final.extinct <- rep(NA, 100)
for(i in 1:100){
  final.extinct[i] <- length((out.m5$binomial)[which(out.m5$t2160>5 & out.m5$sim ==i)])/
    length(db$binomial)
}
hist(final.extinct)

#calculate percent phylogenetic diversity lost
prop.div.lost <- rep(NA,100)
for(i in 1:100){
  extinct.sp <- (out.m5$binomial)[which(out.m5$t2160>5 & out.m5$sim ==i)]
  tree.f<-drop.tip(tree,tree$tip.label[match(extinct.sp, tree$tip.label)])
  prop.div.lost[i] <- sum(tree.f$edge.length)/sum(tree$edge.length) #total phylo diversity (branch lengnth of new tree) Called "Faith's Index"
   #total pgylo diversity (branch lenght of original tree)
 
}
hist(prop.div.lost)


