##m2: Prioritizing Conservation Based on Evolutionary Distinctiveness only

#calculate initial dfunding
at.risk <- sp.db[which(sp.db[,1]>1&sp.db[,1]<6),1] #subset spp to those at risk of extinction this turn
funds <- round(length(at.risk)/10,0) #funding to raise each species by one IUCN class based on McCarthy et al 2012

m2 <- function(db){
  #Calculate Available Funding 
  
  for(i in 1:(length(years)-1)){
    #Remaining Species Possible for Funding & ED Tree
    extinct <- rownames(sp.db)[which(sp.db[,i]>5)]
    at.risk <- data.frame(sp.db[which(sp.db[,i]>1&sp.db[,i]<6),i]) #subset spp to those at risk of extinction this turn
    names(at.risk) <- 'iucn.no'
    
    funding <- (rep(c(1,0),c(funds,(length(at.risk$iucn.no)-funds))))
    
    #Prioritization section
    #randomize the funding
    funding <- sample(funding)
    test.sp <- at.risk
   
    
    #5. merge with available funding
    risk.1 <- cbind(test.sp, funding)
    #6. create new risk for this round
    risk.1$new.risk <- risk.1$iucn.no - risk.1$funding
    #risk.1 <- risk.1$new.risk #changed
    #rownames(risk.1) <- risk.1$binomial
    #7. bring back in all IUCN 1 spp
    risk.2 <- merge(data.frame(sp.db[,i]), risk.1, by = 'row.names', all=T)
    risk.2 <- risk.2[,c(1,2,5)]
    risk.2[,4] <- ifelse(is.na(risk.2[,3]),risk.2[,2],risk.2[,3])
    risk.2 <- risk.2[,c(1,4)]
    names(risk.2) <- c('binomial','new.risk')
    
    #Calculate extinction and increased risk for spp
    #Using a Transition State Matrix
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



#m2(sp.db)

#View(sp.db)

risk.m2 <- foreach(i = 1:1000) %dopar% {
  
  sp.db <- matrix(nrow=length(db$binomial),ncol=16, dimnames = list(db$binomial,years))
  #dimnames(sp.db)
  sp.db[,1] <- db$iucn.no
  sp.db <- sp.db[ order(row.names(sp.db)), ]
  
  m2(sp.db)
  
  #if(i %in% thousand){
  #  send_text(paste("finishing model", i, "of 5000", Sys.time(), sep = " "))
  #} 
}

####  NEW  #####################################################################
library(ggplot2); theme_set(theme_bw()); library(plyr); library(tidyr)
out.m2 <- ldply(risk.m2)
out.m2$binomial <- rep(rownames(risk.m2[[1]]), length(risk.m2))
out.m2$sim <- rep(1:length(risk.m2), each=nrow(risk.m2[[1]]))

ggplot(out.m2, aes(x=t2010, y=..prop..)) + geom_bar() 
ggplot(out.m2, aes(x=t2160, y=..prop..)) + geom_bar() 
ggplot(out.m2, aes(x=t2160-t2010, y=..prop..)) + geom_bar()

ggplot(filter(out.m2, t2160 != 6), aes(x=t2160-t2010, y=..prop..)) + geom_bar() + 
  labs(x="Change in status", y="Proportion of species-simulations", 
       title="Extant species only")
ggplot(filter(out.m2, t2160 == 6), aes(x=t2160-t2010, y=..prop..)) + geom_bar() +
  labs(x="Change in status", y="Proportion of species-simulations", 
       title="Extinct species only")

ggplot(out.m2, aes(x=t2160, y=..prop..)) + geom_bar() + facet_wrap(~t2010) + 
  labs(x="2160 status", y="Proportion of species-simulations", 
       title="All species (panels = 2010 status)")
ggplot(filter(out.m2, t2160 != 6), aes(x=t2160, y=..prop..)) + geom_bar() + facet_wrap(~t2010) + 
  labs(x="2160 status", y="Proportion of species-simulations", 
       title="Extant species (panels = 2010 status)")
ggplot(out.m2, aes(x=factor(t2010), fill=(t2160==6))) + geom_bar(position="fill") +
  labs(x="2010 status", y="Proportion of 2010 category extinct in 2160")
ggplot(out.m2, aes(x=(t2160==6))) + geom_bar() + facet_wrap(~t2010) + 
  labs(x="Extinct", y="Proportion of species-simulations", 
       title="All species (panels = 2010 status)")


timeComp.m2 <- gather(out.m2, timeStep, extRisk, 1:16)
timeCompSp.m2 <- ddply(timeComp.m2, .(binomial, timeStep), summarise,
                       mnRisk=mean(extRisk))
timeCompSp.m2$initRisk <- out.m2$t2010[match(timeCompSp.m2$binomial,
                                             out.m2$binomial)]
ggplot(timeCompSp.m2, aes(x=timeStep, y=mnRisk, group=binomial)) + 
  geom_line(alpha=0.2, colour="red3")

ggplot(timeCompSp.m2, aes(x=timeStep, y=mnRisk, 
                          group=binomial, colour=factor(initRisk))) + 
  geom_line(alpha=0.3, size=0.75) + scale_colour_brewer(type="qual", palette=3)

################################################################################

#calculate the percentage of spp that went extinct
final.extinct.m2 <- rep(NA, 100)
for(i in 1:100){
  final.extinct.m2[i] <- length((out.m2$binomial)[which(out.m2$t2160>5 & out.m2$sim ==i)])/
    length(db$binomial)
}
hist(final.extinct.m2)

#calculate percent phylogenetic diversity lost
prop.div.lost.m2 <- rep(NA,100)
for(i in 1:100){
  extinct.sp.m2 <- (out.m2$binomial)[which(out.m2$t2160>5 & out.m2$sim ==i)]
  tree.f<-drop.tip(tree,tree$tip.label[match(extinct.sp.m2, tree$tip.label)])
  prop.div.lost.m2[i] <- sum(tree.f$edge.length)/sum(tree$edge.length) #total phylo diversity (branch lengnth of new tree) Called "Faith's Index"
  #total pgylo diversity (branch lenght of original tree)
  
}
hist(prop.div.lost.m2)


hist(final.extinct.m2, col = 'grey', xlim = c(0.4,0.6), breaks = 20, xlab = "Percent Spp Extinct")
hist(final.extinct.m5, col = 'red', breaks = 20,add=T)
f.extinct <- c(final.extinct.m4,final.extinct.m5)
f.extinct <- data.frame(f.extinct)
f.extinct$model <- rep(c('m4','m5'),c(100,100))
m1 <- lm(f.extinct$f.extinct ~ f.extinct$model)
anova(m1)
summary(m1)
hist(prop.div.lost.m2, col = 'grey', breaks = 20, xlim = c(0.5,0.8),xlab = "Percent Original Diversity",
     main = "Percentage of Original Diveristy Retained")
hist(prop.div.lost.m4, col = 'red',alpha = 0.5, breaks = 20,add=T)




