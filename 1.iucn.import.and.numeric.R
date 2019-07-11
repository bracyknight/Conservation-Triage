
library(rgdal)
library(raster)
library(sp)
library(parallel)
library(plyr)
library(rgeos)

setwd("~/Dropbox/01_Ch4/data")

primate.map <- readOGR(".", "primate.map.2017")
#View(primate.map)


length(unique(primate.map$binomial))
primate.map <- primate.map[which(primate.map$code != "DD"),]

primate.map$iucn.no <- 0
primate.map$iucn.no[which(primate.map$code == "LC")] <- 1
primate.map$iucn.no[which(primate.map$code == "NT")] <- 2
primate.map$iucn.no[which(primate.map$code == "VU")] <- 3
primate.map$iucn.no[which(primate.map$code == "EN")] <- 4
primate.map$iucn.no[which(primate.map$code == "CR")] <- 5

names(primate.map)
db <- primate.map@data
myvars <- c('binomial','family_nam','genus_name','iucn.no')
db <- db[myvars]
#writeOGR(primate.map, 'primate.map.2017', 'primate.map.2017', driver="ESRI Shapefile")
#rm(primate.map,spp.map)

#View(db)
db1 <- db[match(unique(db$binomial), db$binomial),]
#View(db1)
db <- db1

db$binomial <- tolower(db$binomial)
db$binomial <- factor(db$binomial)
db<- db[,c(1,4)]

length(unique(db$binomial))

