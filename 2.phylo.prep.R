
library(phytools)
library(ape)
library(stringr)

setwd("~/Dropbox/01_Ch4")
tree <- read.nexus('data/10k.genbank.phylo.nex')
tips <- data.frame(tree$tip.label)
tree <- drop.tip(tree, tip = c(179, 32)) #removing extinct species not needed

plotTree(tree)
is.ultrametric(tree)
#Get the nodes of common ancestors of homo-pan, homo-pongo, Papio- Theropithecus, Cebus- Saimiri, Loris- Galago
node <- findMRCA(tree, c('Homo_sapiens', 'Pan_troglodytes_verus'))
#homo-pan = 425
node <- c(node, findMRCA(tree, c('Homo_sapiens', 'Pongo_pygmaeus')))
#homo-pongo = 421
node <- c(node, findMRCA(tree, c('Papio_anubis', 'Theropithecus_gelada')))
#Papio- Theropithecus = 341
node <- c(node, findMRCA(tree, c('Cebus_albifrons', 'Saimiri_ustus')))
#Cebus- Saimiri = 479
node <- c(node, findMRCA(tree, c('Loris_tardigradus', 'Galago_alleni')))
# Loris- Galago = 572
node <- c(node, findMRCA(tree, c('Cebus_albifrons', 'Papio_anubis')))
# Extant Catarrhini = 302

age.min <- c(5, 12.5, 3.5, 12.5, 38, 21)
age.max <- c(8, 18, 6.5, 15, 42, 30)
soft.bounds <- c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
mycalibration <- data.frame(node, age.min, age.max, soft.bounds) 

tree.c <- chronos(tree, lambda = 0.1, model = 'correlated', calibration = mycalibration)
is.ultrametric(tree.c)
plot(tree.c)
tree <- tree.c 

#Convert names to new designation
tree$tip.label[which(tree$tip.label == 'Cebus_apella')] <- 'Sapajus_apella'
tree$tip.label[which(tree$tip.label == 'Cebus_xanthosternos')] <- 'Sapajus_xanthosternos'
tree$tip.label <- tolower(tree$tip.label)
tips <- data.frame(tree$tip.label)



#Build the merge data.frame
names(tips) <- "og"

tips$genus <- str_split_fixed(tips$og, "_", n = Inf)[,1]
tips$species <- str_split_fixed(tips$og, "_", n = Inf)[,2]
tips$binomial <- paste(tips$genus, tips$species, sep = " ")
tips$binomial <- tolower(tips$binomial)
tips$og <- NULL
tips$in.gb <- "Y"

tree$tip.label <- tips$binomial

dup.sp <- which(duplicated(tree$tip.label))
tree <- drop.tip(tree, tip = dup.sp)
tip_order <- factor(tree$tip.label)
write.tree(tree, 'data/untrametric.genbank.tre')


#tree$tip.label


## 3 DO THE PHYLO-DATA MERGE - MAKE SURE TIPS LINE UP

test <- merge(db, tips, by = 'binomial', all = T)
#View(test)

#How many species are in the tree that do not have analogs in the IUCN db?
sum(is.na(test$species))
#How many species are in the IUCN DB that do not have analogs in the tree?
sum(!is.na(test$iucn.no))
#View(test[is.na(test$iucn.no),])

#Create a DB that has missing from each DB
test2 <- test[is.na(test$species),]
test3 <- test[is.na(test$iucn.no),]
test4 <- data.frame(rbind(test3,test2))
#View(test4)

#Rename spp in tree to match those in IUCN DB. Look up in test4 above
tree$tip.label[tree$tip.label=='cercopithecus lhoesti'] <- 'allochrocebus lhoesti'
tree$tip.label[tree$tip.label=='cercopithecus preussi'] <- 'allochrocebus preussi'
tree$tip.label[tree$tip.label=='cercopithecus solatus'] <- 'allochrocebus solatus'
tree$tip.label[tree$tip.label=='microcebus lokobensis'] <- 'microcebus mamiratra'
tree$tip.label[tree$tip.label=='callithrix mauesi'] <- 'mico mauesi'
tree$tip.label[tree$tip.label=='pithecia irrorata'] <- 'pithecia vanzolinii'
tree$tip.label[tree$tip.label=='galago alleni'] <- 'sciurocheirus alleni'
tree$tip.label[tree$tip.label=='cercopithecus pogonias'] <- 'cercopithecus denti'
tree$tip.label[tree$tip.label=='galago granti'] <- 'galagoides granti'
tree$tip.label[tree$tip.label=='cercopithecus solatus'] <- 'allochrocebus solatus'
tree$tip.label[tree$tip.label=='callithrix argentata'] <- 'mico argentatus'
tree$tip.label[tree$tip.label=='callithrix emiliae'] <- 'mico emiliae'
tree$tip.label[tree$tip.label=='bunopithecus hoolock'] <- 'hoolock hoolock'
tree$tip.label[tree$tip.label=='lepilemur manasamody'] <- 'lepilemur grewcockorum'
tree$tip.label[tree$tip.label=='macaca brunnescens'] <- 'macaca ochreata'
tree$tip.label[tree$tip.label=='callithrix kuhli'] <- 'callithrix kuhlii'
tree$tip.label[tree$tip.label=='cercopithecus preussi'] <- 'allochrocebus preussi'
tree$tip.label[tree$tip.label=='hapalemur simus'] <- 'prolemur simus'
tree$tip.label[tree$tip.label=='callithrix pygmaea'] <- 'cebuella pygmaea'
tree$tip.label[tree$tip.label=='alouatta seniculus'] <- 'alouatta macconnelli'
tree$tip.label[tree$tip.label=='cacajao melanocephalus'] <- 'cacajao hosomi'
tree$tip.label[tree$tip.label=='aotus azarai'] <- 'aotus azarae'
tree$tip.label[tree$tip.label=='cebus olivaceus'] <- 'cebus kaapori'
tree$tip.label[tree$tip.label=='callithrix humeralifera'] <- 'mico humeralifer'
tree$tip.label[tree$tip.label=='lepilemur randrianasoli'] <- 'lepilemur randrianasoloi'
tree$tip.label[tree$tip.label=='piliocolobus foai'] <- 'piliocolobus oustaleti'

dup.sp <- which(duplicated(tree$tip.label))
tree <- drop.tip(tree, tip = dup.sp)
tip_order <- factor(tree$tip.label)
write.tree(tree, 'data/untrametric.genbank.tre')



length(db$binomial)
db$binomial <- as.character(db$binomial)
## FIX cebus capucinus needs IUCN rating of 1 (LC)
db[410,] <- c('cebus capucinus',1)
db[411,] <- c('piliocolobus pennantii',4)

db$binomial <- factor(db$binomial)

rm(test,test2,test3,test4,tips)
tips <- data.frame(tree$tip.label)
names(tips)<-'binomial'
tips$in.phylo <- 'yes'

test <- merge(tips,db, by = 'binomial', all = T )
test <- test[!is.na(test$in.phylo),]
test <- na.omit(test) #there are 258 spp in 10k trees and IUCN
#View(test)

d <- test





