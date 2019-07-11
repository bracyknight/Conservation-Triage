######## Metrics

#calculate percentage of species that survive at time i
survived <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 
for(i in 1:length(risk)){
  for(m in 1:y){
    survived[i,m] <- length((out$binomial)[which(out[,m]<6 & out$sim ==i)])/
      length(db$binomial)
  }
}
survived <- data.frame(survived)
survstats <- data.frame(map_df(list(mean, var), ~ map_df(survived, .x)))
rownames(survstats) <- c('mean','var')

#calculate percent phylogenetic diversity lost at time i
diversity <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 
for(i in 1:length(risk)){
  for(m in 1:y){
    extinct <- (out$binomial)[which(out[,m]>5 & out$sim ==i)]
    tree.f<-drop.tip(tree,tree$tip.label[match(extinct, tree$tip.label)])
    diversity[i,m] <- sum(tree.f$edge.length)/sum(tree$edge.length) #total phylo diversity (branch lengnth of new tree) Called "Faith's Index"
  }
}
diversity <- data.frame(diversity)
divstats <- data.frame(map_df(list(mean, var), ~ map_df(diversity, .x)))
rownames(divstats) <- c('mean','var')

#Number of species going extinct
extinct <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db)))
for(i in 1:length(risk)){
  for(m in 1:y){
    extinct[i,m] <- length(which(out[,m]>5 & out$sim ==i))
    #tree.f<-drop.tip(tree,tree$tip.label[match(extinct, tree$tip.label)])
    # diversity[i,m] <- sum(tree.f$edge.length)/sum(tree$edge.length) #total phylo diversity (branch lengnth of new tree) Called "Faith's Index"
  }
}
extinct <- data.frame(extinct)
exstats <- data.frame(map_df(list(mean, var), ~ map_df(extinct, .x)))
rownames(exstats) <- c('mean','var')

### Funding

#calculate percentage of species that survive at time i
ann.funding <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 
for(i in 1:length(risk)){
  for(m in 1:y){
    ann.funding[i,m] <- round(length((out$binomial)[which(out[,m]>1 & out$sim ==i)])/10,0)
  }
}

ann.funding <- data.frame(ann.funding)
fundstats <- data.frame(map_df(list(mean, var), ~ map_df(ann.funding, .x)))
rownames(fundstats) <- c('mean','var')


#### Traits and Habitats
traits <- db
out.trait <- merge(out, traits, all.x = T, by = 'binomial')
names(out.trait)
t <- names(out.trait[,c(2:17,22:71,21,20,1,18)])
trait.db <- out.trait[,c(2:17,22:71,21,20,1,18)]

names(trait.db)

#How many habitats are still occupied? original number is 27
habitats <- trait.db[,c(1:16,32:62,69,70)]
habs <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 

for(i in 1:length(risk)){
  for(m in 1:y){
    h.s <- (habitats)[which(habitats[,m]<6 & habitats$sim == i),]
    habs[i,m] <- sum(colSums(h.s[17:47],na.rm=T)>0)
  }
}

habitats <-data.frame(habs)
habstats <- data.frame(map_df(list(mean, var), ~ map_df(habitats, .x)))
rownames(habstats) <- c('mean','var')

#regional habitats
#afro habs
af.habitats <- trait.db[which(trait.db$supregion == "afrotropical"),]
af.habitats <-  af.habitats[,c(1:16,32:62,69,70)]

af.habs <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 

for(i in 1:length(risk)){
  for(m in 1:y){
    h.s <- (af.habitats)[which(af.habitats[,m]<6 & af.habitats$sim == i),]
    af.habs[i,m] <- sum(colSums(h.s[17:47],na.rm=T)>0)
  }
}

af.habitats <-data.frame(af.habs)
af.habstats <- data.frame(map_df(list(mean, var), ~ map_df(af.habitats, .x)))
rownames(af.habstats) <- c('mean','var')

#mada habs
ma.habitats <- trait.db[which(trait.db$supregion == "madagascar"),]
ma.habitats <-  ma.habitats[,c(1:16,32:62,69,70)]

ma.habs <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 

for(i in 1:length(risk)){
  for(m in 1:y){
    h.s <- (ma.habitats)[which(ma.habitats[,m]<6 & ma.habitats$sim == i),]
    ma.habs[i,m] <- sum(colSums(h.s[17:47],na.rm=T)>0)
  }
}

ma.habitats <-data.frame(ma.habs)
ma.habstats <- data.frame(map_df(list(mean, var), ~ map_df(ma.habitats, .x)))
rownames(ma.habstats) <- c('mean','var')

#neot habs
ne.habitats <- trait.db[which(trait.db$supregion == "neotropical"),]
ne.habitats <-  ne.habitats[,c(1:16,32:62,69,70)]

ne.habs <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 

for(i in 1:length(risk)){
  for(m in 1:y){
    h.s <- (ne.habitats)[which(ne.habitats[,m]<6 & ne.habitats$sim == i),]
    ne.habs[i,m] <- sum(colSums(h.s[17:47],na.rm=T)>0)
  }
}

ne.habitats <-data.frame(ne.habs)
ne.habstats <- data.frame(map_df(list(mean, var), ~ map_df(ne.habitats, .x)))
rownames(ne.habstats) <- c('mean','var')

#asia habs
as.habitats <- trait.db[which(trait.db$supregion == "asia"),]
as.habitats <-  as.habitats[,c(1:16,32:62,69,70)]

as.habs <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 

for(i in 1:length(risk)){
  for(m in 1:y){
    h.s <- (as.habitats)[which(as.habitats[,m]<6 & as.habitats$sim == i),]
    as.habs[i,m] <- sum(colSums(h.s[17:47],na.rm=T)>0)
  }
}

as.habitats <-data.frame(as.habs)
as.habstats <- data.frame(map_df(list(mean, var), ~ map_df(ne.habitats, .x)))
rownames(as.habstats) <- c('mean','var')


#by-species stats on species change in risk
sp.results <- out %>%
  split(.$binomial) %>%
  map(summary) 
#(sp.results$`rhinopithecus avunculus`)

##Mean and variance of rarity at time i
rarestats <- data.frame(map_df(list(mean, var), ~ map_df(out[1:16], .x)))
rownames(rarestats) <- c('mean','var')

#Change in risk (mean and variance) from initial
k1 <- out[,1:16] - out$t2020
k1$binomial <- out$binomial
k1$sim <- out$sim
changestats <- data.frame(map_df(list(mean, var), ~ map_df(k1[1:16], .x)))
rownames(changestats) <- c('mean','var')

#assign
assign(paste(model, "survived", sep="."), data.frame(survived)) # mX.XXX.survived
assign(paste(model, "survstats", sep="."), data.frame(survstats))
assign(paste(model, "diversity", sep="."), data.frame(diversity))
assign(paste(model, "divstats", sep="."), data.frame(divstats))
assign(paste(model, "extinct", sep="."), data.frame(extinct))
assign(paste(model, "exstats", sep="."), data.frame(exstats))
assign(paste(model, "annfunding", sep="."), data.frame(ann.funding))
assign(paste(model, "fundstats", sep="."), data.frame(fundstats))
assign(paste(model, "habitats", sep="."), data.frame(habitats))
assign(paste(model, "habstats", sep="."), data.frame(habstats))
assign(paste(model, "af.habitats", sep="."), data.frame(af.habitats))
assign(paste(model, "af.habstats", sep="."), data.frame(af.habstats))
assign(paste(model, "as.habstats", sep="."), data.frame(as.habstats))
assign(paste(model, "as.habitats", sep="."), data.frame(as.habitats))
assign(paste(model, "ma.habitats", sep="."), data.frame(ma.habitats))
assign(paste(model, "ma.habstats", sep="."), data.frame(ma.habstats))
assign(paste(model, "ne.habitats", sep="."), data.frame(ne.habitats))
assign(paste(model, "ne.habstats", sep="."), data.frame(ne.habstats))
assign(paste(model, "sp.results", sep="."), data.frame(sp.results))
assign(paste(model, "rarestats", sep="."), data.frame(rarestats))
assign(paste(model, "changestats", sep="."), data.frame(changestats))



