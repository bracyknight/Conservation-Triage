##Model Prep Code
library(ggplot2)
library(foreach)
library(doMC)
library(picante)
library(ape)
library(plyr)
library(dplyr)
registerDoMC(7)  #change the 2 to your number of CPU cores  
options(cores=7)

#create the Transition State Matrix
LC <- c(0.678,	0.244,	0.060,	0.015,	0.004,	0)
NT <- c(0,	0.682,	0.244,	0.060,	0.015,	0)
VU <- c(0,	0,	0.686,	0.244,	0.060,	0.010)
EN <- c(0,	0,	0,	0.652,	0.244,	0.104)
CR <- c(0,	0,	0,	0,	0.501,	0.499)
EX <- c(0,0,0,0,0,1)
t.mat <- rbind(LC,NT,VU,EN,CR,EX)
colnames(t.mat) <- 1:6
rownames(t.mat) <- 1:6
rm(LC,NT,VU,EN,CR,EX)

#Bring in data
tree <- read.tree('data/untrametric.genbank.tre')
db <- read.csv('data/ch.4.data.csv',stringsAsFactors = F)
db[1] <- NULL
db <- db[order(db$binomial),]
#sp.db <- data.frame(sp.db[order(row.names(sp.db)),])
db <- data.frame(db)
sp.list <- data.frame(db$binomial)
names(sp.list) <- 'binomial'

#Clean the tree
#Convert names to new designation
tips <- data.frame(tree$tip.label)
names(tips) <- "og"
tips$genus <- str_split_fixed(tips$og, "_", n = Inf)[,1]
tips$species <- str_split_fixed(tips$og, "_", n = Inf)[,2]
tips$binomial <- paste(tips$genus, tips$species, sep = " ")
tips$binomial <- tolower(tips$binomial)
tips$og <- NULL
tips$in.gb <- "Y"
tree$tip.label <- tips$binomial
tree.m <- tree

years <- seq(2020,length.out = y, by = 10)
years <- paste("t",years, sep = "")
nm <- list('binomial','years',seq(1:100))
sp.db <- matrix(nrow=length(db$binomial),ncol=y, dimnames = list(db$binomial,years))
dimnames(sp.db)
sp.db[,1] <- db$iucn.no

#sp.db <- sp.db[ order(row.names(sp.db)), ]
na_count <-sapply(sp.db, function(y) sum(is.na(y)))
thousand <- seq(0,1000, by = 200)
thousand <- c(1, thousand)

i <- 1

#calculate initial dfunding
at.risk <- sp.db[which(sp.db[,1]>1&sp.db[,1]<6),1] #subset spp to those at risk of extinction this turn
funds <- round(length(at.risk)/10,0) #funding to raise each species by one IUCN class based on McCarthy et al 2012


send_text = function(message){
  system(paste('osascript -e \'tell application "Messages"\' -e \'send "',
               message, '" to buddy "303-999-6726" of (service 1 whose service type is iMessage)\' -e \'end tell\''))
}

