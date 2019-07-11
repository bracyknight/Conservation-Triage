#Blob Stats Generator

ave.surv <- aggregate(ms$per.survive, by = list(ms$year), FUN = mean)
names(ave.surv) <- c('year','ave.surv')
min.surv <- aggregate(ms$per.survive, by = list(ms$year), FUN = min)
names(min.surv) <- c('year','min.surv')
max.surv <- aggregate(ms$per.survive, by = list(ms$year), FUN = max)
names(max.surv) <- c('year','max.surv')
surv.over.time <- cbind(ave.surv,min.surv,max.surv)
surv.over.time <- surv.over.time[,-c(3,5)]


ave.div <- aggregate(md$div, by = list(md$year), FUN = mean)
names(ave.div) <- c('year','ave.div')
min.div <- aggregate(md$div, by = list(md$year), FUN = min)
names(min.div) <- c('year','min.div')
max.div <- aggregate(md$div, by = list(md$year), FUN = max)
names(max.div) <- c('year','max.div')
div.over.time <- cbind(ave.div,min.div,max.div)
div.over.time <- div.over.time[,-c(3,5)]
