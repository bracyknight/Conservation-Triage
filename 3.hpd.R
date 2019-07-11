# Other primate data import and managment

#dataset is now: d

#View(d)
#View(tips)

hpd <- read.csv('data/primate.hpd.csv', stringsAsFactors = F)
hpd[1] <- NULL
hpd$binomial <- tolower(hpd$binomial)
#View(hpd)
names(hpd)
hpd <- hpd[,c(3,32:52)]

test <- merge(d, hpd, by = 'binomial', all = T)
#View(test)
#View(tips)

#Check to identify which spp are missing from each db
test2 <- test[is.na(test$in.phylo),]
test3 <- test[is.na(test$hpd1800),]
test4 <- data.frame(rbind(test3,test2))
#View(test4)

#Okay, now create a final db with only the complete spp from hpd and the phylo
db <- na.omit(test)
#View(db)

#Now, do that whole change over time for last 100 years
db$hpd100 <- (db$hpd2000)-(db$hpd1900)


