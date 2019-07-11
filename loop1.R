

#This script needs to have 4 sections
  #1. funding
  #2. action
  #3. roll the dice
  #4. calc new risk (risk[i+1])

##Intial Setup of each DB
## RISK
years <- seq(2010,2410, by = 10)
years <- paste("t",years, sep = "")
risk <- matrix(nrow=409,ncol=1, dimnames = list(db$binomial,'init'))
risk[,1] <- db$iucn.no
#View(risk)

## FUNDING
funding <- matrix(nrow=409,ncol=0, dimnames = list(db$binomial))
#View(funding)

## ACTION (this is the db that subtracts Risk[i] - Funding[i])
action <- matrix(nrow=409,ncol=41, dimnames = list(db$binomial,years))


##Pseudo code
for(i in 1:5){
  #part 1: look at extant spp and calculate funding and regime for supplying it
  extant <- risk[,i]
  extant<- extant[which(extant < 6)]
  #code to assign randomly (aka the Null Model)
  potential.sp <- risk[which(1<risk[,i]&risk[,i]<6),]
  potential.sp <- potential.sp #potential.sp[,i]
  funds <- ceiling(length(potential.sp)/10)
  funds <- rep(c(1,0),c(funds, (length(potential.sp)-funds))) 
  funds <- sample(funds) #random w/o replacement ordering
  names(funds) <- names(potential.sp)
  funding <- transform(merge(funding,funds,by= 'row.names',all=TRUE), row.names=Row.names, Row.names=NULL)
  colnames(funding)[i] <- colnames(risk)[i]
  funding[is.na(funding)] <- 0 #set all non-potential species funding to zero

  
  #part 2: record action (i.e. risk[i]-funding[i])
  #action[,i] <- potential.sp - funds
  #action[which(action[,i] == 0)] <- 1 #setting all risk of 0 back to 1 (can't have no risk)
  funds <- potential.sp - funds
  funds[which(funds < 1)] <- 1
  
  #part 3: roll the dice
  # calculate the random effect
  rando <- sample(1:6, length(funds), replace = T) #problem is that rando doesn't fill in blanks
  
  #part 4: calc new risk
  newrisk <- data.frame(ifelse(rando>funds,funds+1,funds))
  colnames(newrisk) <- years[i+1]
  

 risk <- transform(merge(risk,newrisk,by= 'row.names',all=TRUE), row.names=Row.names, Row.names=NULL)
 risk[,i+1]<-ifelse(is.na(risk[,i+1]),risk[,i],risk[,i+1])

}



View(risk)
View(funding)
View(action)

