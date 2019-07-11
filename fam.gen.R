#### Traits and Habitats
traits <- db
out.trait <- merge(out, traits, all.x = T, by = 'binomial')
names(out.trait)
t <- names(out.trait[,c(2:17,22:71,21,20,1,18)])
trait.db <- out.trait[,c(2:17,22:71,21,20,1,18)]

names(trait.db)

#How many habitats are still occupied? original number is 27
genera <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 
families <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 
af.genera <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 
af.families <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 
as.genera <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 
as.families <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 
ne.genera <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 
ne.families <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 
ma.genera <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 
ma.families <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 




for(i in 1:length(risk)){
  for(m in 1:y){
    h.s <- (trait.db)[which(trait.db[,m]<6 & trait.db$sim == i),]
    genera[i,m] <- length(unique(h.s$genus))
    families[i,m] <- length(unique(h.s$family))
    
    h.ss <- h.s[which(h.s$supregion == 'afrotropical'),]
    af.genera[i,m] <-  length(unique(h.ss$genus))
    af.families[i,m] <-  length(unique(h.ss$family))
    
    h.ss <- h.s[which(h.s$supregion == 'asia'),]
    as.genera[i,m] <-  length(unique(h.ss$genus))
    as.families[i,m] <-  length(unique(h.ss$family))
    
    h.ss <- h.s[which(h.s$supregion == 'neotropical'),]
    ne.genera[i,m] <-  length(unique(h.ss$genus))
    ne.families[i,m] <-  length(unique(h.ss$family))
    
    h.ss <- h.s[which(h.s$supregion == 'madagascar'),]
    ma.genera[i,m] <-  length(unique(h.ss$genus))
    ma.families[i,m] <-  length(unique(h.ss$family))
  }
}



af.genera <-data.frame(af.genera)
af.genstats <- data.frame(map_df(list(mean, var), ~ map_df(af.genera, .x)))
rownames(af.genstats) <- c('mean','var')
af.families <-data.frame(af.families)
af.famstats <- data.frame(map_df(list(mean, var), ~ map_df(af.families, .x)))
rownames(af.famstats) <- c('mean','var')

as.genera <-data.frame(as.genera)
as.genstats <- data.frame(map_df(list(mean, var), ~ map_df(as.genera, .x)))
rownames(as.genstats) <- c('mean','var')
as.families <-data.frame(as.families)
as.famstats <- data.frame(map_df(list(mean, var), ~ map_df(as.families, .x)))
rownames(as.famstats) <- c('mean','var')

ne.genera <-data.frame(ne.genera)
ne.genstats <- data.frame(map_df(list(mean, var), ~ map_df(ne.genera, .x)))
rownames(ne.genstats) <- c('mean','var')
ne.families <-data.frame(ne.families)
ne.famstats <- data.frame(map_df(list(mean, var), ~ map_df(ne.families, .x)))
rownames(ne.famstats) <- c('mean','var')

ma.genera <-data.frame(ma.genera)
ma.genstats <- data.frame(map_df(list(mean, var), ~ map_df(ma.genera, .x)))
rownames(ma.genstats) <- c('mean','var')
ma.families <-data.frame(ma.families)
ma.famstats <- data.frame(map_df(list(mean, var), ~ map_df(ma.families, .x)))
rownames(ma.famstats) <- c('mean','var')


assign(paste(model, "genera", sep="."), data.frame(genera))
assign(paste(model, "families", sep="."), data.frame(families))
assign(paste(model, "genera", sep="."), data.frame(genera))
assign(paste(model, "af.genera", sep="."), data.frame(af.genera))
assign(paste(model, "af.genstats", sep="."), data.frame(af.genstats))
assign(paste(model, "af.families", sep="."), data.frame(af.families))
assign(paste(model, "af.famstats", sep="."), data.frame(af.famstats))

assign(paste(model, "as.genera", sep="."), data.frame(as.genera))
assign(paste(model, "as.genstats", sep="."), data.frame(as.genstats))
assign(paste(model, "as.families", sep="."), data.frame(as.families))
assign(paste(model, "as.famstats", sep="."), data.frame(as.famstats))

assign(paste(model, "ne.genera", sep="."), data.frame(ne.genera))
assign(paste(model, "ne.genstats", sep="."), data.frame(ne.genstats))
assign(paste(model, "ne.families", sep="."), data.frame(ne.families))
assign(paste(model, "ne.famstats", sep="."), data.frame(ne.famstats))

assign(paste(model, "ma.genera", sep="."), data.frame(ma.genera))
assign(paste(model, "ma.genstats", sep="."), data.frame(ma.genstats))
assign(paste(model, "ma.families", sep="."), data.frame(ma.families))
assign(paste(model, "ma.famstats", sep="."), data.frame(ma.famstats))






