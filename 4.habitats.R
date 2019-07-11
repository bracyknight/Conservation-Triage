
#Habitat Associations from Ch.1
habitat <- read.csv('data/ch1.data.csv', stringsAsFactors = F)
#View(habitat)
#keep <- c('binomial','supregion','breadth','nonahabbreadth','iucnanthro')
#test <- habitat[keep]
#View(test)
#habitat <- test
habitat$binomial <- tolower(habitat$binomial)

#Taxonomic fixes
habitat$binomial[which(habitat$binomial=='cercopithecus solatus')] <- 'allochrocebus solatus'
habitat$binomial[which(habitat$binomial=='cercopithecus lhoesti')] <- 'allochrocebus lhoesti'
habitat$binomial[which(habitat$binomial=='cercopithecus preussi')] <- 'allochrocebus preussi'
habitat$binomial[which(habitat$binomial=='procolobus badius')] <- 'piliocolobus badius'
habitat$binomial[which(habitat$binomial=='procolobus gordonorum')] <- 'piliocolobus gordonorum'
habitat$binomial[which(habitat$binomial=='procolobus kirkii')] <- 'piliocolobus kirkii'
habitat$binomial[which(habitat$binomial=='procolobus pennantii')] <- 'piliocolobus pennantii'
habitat$binomial[which(habitat$binomial=='procolobus preussi')] <- 'piliocolobus preussi'
habitat$binomial[which(habitat$binomial=='procolobus rufomitratus')] <- 'piliocolobus rufomitratus'


test <- merge(d,habitat, by= 'binomial', all.x = T)

#View(test)
missing.hab <- test[is.na(test$breadth),]
#View(missing.hab)

db <- test
names(db)
keep <- c(1,3,4,10,7,12:25,40:70,81:84)
names(db)[keep]
#View(db)
test <- db[keep]
#View(test)
db <- test
