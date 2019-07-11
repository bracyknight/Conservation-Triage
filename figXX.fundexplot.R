library(cowplot)


out <- out.m3.f2 #prep for analysis/graphics scripts
risk <- risk.m3.f2 #prep for analysis/graphics scripts
model <- "m3.f2"

#calculate percentage of species that survive at time i
lcleft <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 
for(i in 1:length(risk)){
  for(m in 1:y){
    lcleft[i,m] <- length((out$binomial)[which(out[,m] < 2 & out$sim ==i)])  ### I CHANGED THIS 26 OCT 2017 FROM == 2 TO < 2
  }
}
m3.f2.lcleft <- data.frame(lcleft)
m3.f2.lcstats <- data.frame(map_df(list(mean, var), ~ map_df(m3.f2.lcleft, .x)))
rownames(m3.f2.lcstats) <- c('mean','var')

riskleft <- matrix(nrow=n,ncol=y,dimnames = list(1:n,colnames(sp.db))) 
for(i in 1:length(risk)){
  for(m in 1:y){
    riskleft[i,m] <- length((out$binomial)[which(out[,m]>1 & out[,m]<6 &out$sim ==i)])
  }
}
m3.f2.riskleft <- data.frame(riskleft)
m3.f2.riskleft <- data.frame(map_df(list(mean, var), ~ map_df(m3.f2.riskleft, .x)))
rownames(m3.f2.riskleft) <- c('mean','var')


sp.db[,1]
sum(sp.db[,1]>1)/10

k.1 <- as.numeric(m3.f2.fundstats[1,])
k.2 <- (seq(2020,2170, by = 10))
k.4 <- as.numeric(m3.f2.exstats[1,])
k.5 <- as.numeric(m3.f2.lcstats[1,])
k.6 <- as.numeric(m3.f2.riskleft[1,])


k.3 <- cbind(k.1,k.4,k.5,k.6,k.2)
colnames(k.3) <- c('Funding','Extinct','LC','AtRisk',"Decade")
k.3 <- data.frame(k.3)

##Overlap plot test
#munge the data to long form
k.ex <- k.3[,2:5]
exdata <- k.ex %>%
    #select(starts_with('x')) %>%
    gather(key = Decade)
colnames(exdata) <- c('Decade','Class','Value')
exdata$Class <- factor(exdata$Class, levels = c('Extinct', 'AtRisk','LC'))
levels(exdata$Class)

explot <- ggplot(data=exdata, aes(x=Decade, y=Value, group = factor(Class))) +
  geom_line(aes(color = Class), linetype = 'dashed')+
  geom_point(aes(color = Class)) +
  scale_color_manual(name = NULL,
                     values=c("#C0392B","#F39C12",  "#196F3D"),
                     labels=c("Extinct", "At Risk","Least Concern"))
explot <-explot + theme(
  legend.position = c(1, .75),
  legend.justification = c("right", "top")
) +
  scale_y_continuous(name="Number of Species", limits=c(0,200))


explot 

fplot <- ggplot(data=k.3, aes(x=Decade, y=Funding)) +
  geom_line(aes(), linetype = 'dashed')+
  geom_point() 
  


fplot <- ggplot(data=k.3, aes(x=Decade, y=Funding)) +
  geom_line(linetype = 'dashed', color = 'black')+
  geom_point(color = 'black') +
  theme_classic() + scale_x_continuous(breaks=seq(2020,2170,50))+
  scale_y_continuous(name="Number of Funded Species", limits=c(0,20))


fundex.plot <- plot_grid(fplot, explot, labels = c("A", "B"))

fundex.plot
