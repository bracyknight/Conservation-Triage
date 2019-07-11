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

assign(paste(model, "annfunding", sep="."), data.frame(ann.funding))
assign(paste(model, "fundstats", sep="."), data.frame(fundstats))