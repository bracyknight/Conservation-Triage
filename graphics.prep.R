#Graphics Prep

#calculate percentage of species that survive at time i
survived <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 
for(i in 1:length(risk)){
  for(m in 1:y){
    survived[i,m] <- length((out$binomial)[which(out[,m]<6 & out$sim ==i)])/
      length(db$binomial)
  }
}

#calculate percent phylogenetic diversity lost at time i
diversity <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 
for(i in 1:length(risk)){
  for(m in 1:y){
    extinct <- (out$binomial)[which(out[,m]>5 & out$sim ==i)]
    tree.f<-drop.tip(tree,tree$tip.label[match(extinct, tree$tip.label)])
    diversity[i,m] <- sum(tree.f$edge.length)/sum(tree$edge.length) #total phylo diversity (branch lengnth of new tree) Called "Faith's Index"
  }
}

#### Habitat utilization at time i
habs <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 

h2 <- habitat
h2$binomial <- tolower(h2$binomial)
out.hab <- merge(out, h2, all.x = T, by = 'binomial')
h <- names(out.hab[,c(2:18,55:85,1,18)])
habitats <- out.hab[h]
habitats <- na.omit(habitats)
#habitats <- habitats[,c(3:32,1,2)]

out.h3$bin <- out.h3$binomial
out.h3$binomial <- NULL
names(out.h3)[which(names(out.h3)=='bin')] <- 'binomial'

for(i in 1:length(risk)){
 for(m in 1:y){
   h.s <- (habitats)[which(habitats[,m]<6 & habitats$sim ==i),]
   habs[i,m] <- sum(colSums(h.s[17:47])>0)
 }
}

##Mean and variance of rarity at time i
out.rare <- data.frame(map_df(list(mean, var), ~ map_df(out[1:16], .x)))
rownames(out.rare) <- c('mean','var')


out.rare <- data.frame(map_df(list(mean, sd), ~ map_df(out[1:16], .x)))





##numer of species improving at time i
improve <- matrix(nrow=n, ncol = 16)
colnames(improve) <- colnames(out[1:16])
 
out.r <- out[,-17]
out.r[,1:16] <- out.r[,c(1:16)] - out.r[,1]
out.r[,2:16] <- ifelse(out.r[,2:16]<0,1,0)
out.r <- data.frame(out.r)

for(i in 1:n){
  improve[i,] <- colSums(out.r[which(out.r$sim==i),1:16])
}

sp.imp <- rbind(colMeans(improve),colSdDiffs(improve))
rownames(sp.imp) <- c('mean','sd')
#exports from this step are: improve, sp.imp