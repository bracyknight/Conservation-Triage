## Fig XXIa-d

#NEOTROPICAL HABITATS

# munge the data into long form
m2.ne.habs <- m2.f2.ne.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m2.ne.habs$model <- "M2"

# munge the data into long form
m3.ne.habs <- m3.f2.ne.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m3.ne.habs$model <- "M3"

# munge the data into long form
m4.ne.habs <- m4.f2.ne.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m4.ne.habs$model <- "M4"

# munge the data into long form
m5.ne.habs <- m5.f2.ne.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m5.ne.habs$model <- "M5"

# munge the data into long form
m6.ne.habs <- m6.f2.ne.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m6.ne.habs$model <- "M6"


habs.ne <- rbind(m2.ne.habs, m3.ne.habs, m4.ne.habs, m5.ne.habs, m6.ne.habs)
habs.ne <-data.frame(habs.ne)
names(habs.ne) <- c('year','num.hab','mod')
habs.ne$year <- sub('.', '', habs.ne$year)
habs.ne <- data.frame(habs.ne)

p <- ggplot(habs.ne, aes(x=year, y=num.hab, group =  mod, colour = mod)) +
  stat_summary(aes(y = num.hab,group=mod), fun.y=mean, geom="line")+
  stat_summary(fun.data = 'mean_sdl', fun.args = list(mult=0.5),geom = 'smooth', alpha = 0.2) 

p1 <- p +  labs(y = "Number of Habitats", x = "Decade") +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(axis.text.y = element_text(size=10,angle=0)) +
  scale_colour_manual(values=plotcols) +
  scale_x_discrete(breaks=c(2020, 2070, 2120, 2170), name = '') +
  scale_y_continuous(name = '') +
  theme(axis.text.x=element_blank(),plot.margin = unit(c(0,0,-1,-1),"lines")) 
  

ggsave('figs/figXXIb.ne.png', width = 2.75, height = 1.87, unit = 'in', dpi = 600, pointsize = 10)

#ASIA HABITATS

#NEOTROPICAL HABITATS

# munge the data into long form
m2.as.habs <- m2.f2.as.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m2.as.habs$model <- "M2"

# munge the data into long form
m3.as.habs <- m3.f2.as.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m3.as.habs$model <- "M3"

# munge the data into long form
m4.as.habs <- m4.f2.as.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m4.as.habs$model <- "M4"

# munge the data into long form
m5.as.habs <- m5.f2.as.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m5.as.habs$model <- "M5"

# munge the data into long form
m6.as.habs <- m6.f2.as.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m6.as.habs$model <- "M6"


habs.ne <- rbind(m2.as.habs, m3.as.habs, m4.as.habs, m5.as.habs, m6.as.habs)
habs.ne <-data.frame(habs.ne)
names(habs.ne) <- c('year','num.hab','mod')
habs.ne$year <- sub('.', '', habs.ne$year)
habs.ne <- data.frame(habs.ne)

p <- ggplot(habs.ne, aes(x=year, y=num.hab, group =  mod, colour = mod)) +
  stat_summary(aes(y = num.hab,group=mod), fun.y=mean, geom="line")+
  stat_summary(fun.data = 'mean_sdl', fun.args = list(mult=0.5),geom = 'smooth', alpha = 0.2) 

p1 <- p +  labs(y = "Number of Habitats", x = "Decade") +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(axis.text.y = element_text(size=10,angle=0)) +
  scale_colour_manual(values=plotcols) +
  scale_x_discrete(breaks=c(2020, 2070, 2120, 2170), name = '') +
  scale_y_continuous(name = '') +
  theme(axis.text.x=element_blank(),plot.margin = unit(c(0,0,-1,-1),"lines")) 


ggsave('figs/figXXIa.as.png', width = 2.75, height = 1.87, unit = 'in', dpi = 600, pointsize = 10)



#AFRICA HABITATS

# munge the data into long form
m3.af.habs <- m3.f2.af.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m3.af.habs$model <- "M3"

# munge the data into long form
m5.af.habs <- m5.f2.af.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m5.af.habs$model <- "M5"

# munge the data into long form
m4.af.habs <- m4.f2.af.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m4.af.habs$model <- "M4"

# munge the data into long form
m2.af.habs <- m2.f2.af.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m2.af.habs$model <- "M2"

# munge the data into long form
m6.af.habs <- m6.f2.af.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m6.af.habs$model <- "M6"

habs.af <- rbind(m2.af.habs, m3.af.habs, m4.af.habs, m5.af.habs,m6.af.habs)
habs.af <-data.frame(habs.af)
names(habs.af) <- c('year','num.hab','mod')
habs.af$year <- sub('.', '', habs.af$year)
habs.af <- data.frame(habs.af)

p <- ggplot(habs.af, aes(x=year, y=num.hab, group =  mod, colour = mod)) +
  stat_summary(aes(y = num.hab,group=mod), fun.y=mean, geom="line")+
  stat_summary(fun.data = 'mean_sdl', fun.args = list(mult=0.5),geom = 'smooth', alpha = 0.2) 

p1 <- p +  labs(y = "Number of Habitats", x = "Decade") +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(axis.text.y = element_text(size=10,angle=0)) +
  scale_colour_manual(values=plotcols) +
  scale_y_continuous(breaks=c(12, 15, 18, 21, 24), name = '') +
  scale_x_discrete(breaks=c(2020, 2070, 2120, 2170), name = '') +
  #scale_y_continuous(name = '') +
  theme(axis.text.x=element_blank(),plot.margin = unit(c(0,0,-1,-1),"lines")) 


ggsave('figs/figXXIc.af.png', width = 2.75, height = 1.87, unit = 'in', dpi = 600, pointsize = 10)

#MADAGASCAR HABITATS

# munge the data into long form
m3.ma.habs <- m3.f2.ma.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m3.ma.habs$model <- "M3"

# munge the data into long form
m5.ma.habs <- m5.f2.ma.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m5.ma.habs$model <- "M5"

# munge the data into long form
m4.ma.habs <- m4.f2.ma.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m4.ma.habs$model <- "M4"

# munge the data into long form
m2.ma.habs <- m2.f2.ma.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m2.ma.habs$model <- "M2"

# munge the data into long form
m6.ma.habs <- m6.f2.ma.habitats %>%
  select(starts_with('t')) %>%
  gather(key = row)
m6.ma.habs$model <- "M6"

habs.ma <- rbind(m2.ma.habs, m3.ma.habs,m4.ma.habs, m5.ma.habs, m6.ma.habs)
habs.ma <-data.frame(habs.ma)
names(habs.ma) <- c('year','num.hab','mod')
habs.ma$year <- sub('.', '', habs.ma$year)
habs.ma <- data.frame(habs.ma)

p <- ggplot(habs.ma, aes(x=year, y=num.hab, group =  mod, colour = mod)) +
  stat_summary(aes(y = num.hab,group=mod), fun.y=mean, geom="line")+
  stat_summary(fun.data = 'mean_sdl', fun.args = list(mult=0.5),geom = 'smooth', alpha = 0.2) 

p1 <- p +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(axis.text.y = element_text(size=10,angle=0)) +
  scale_x_discrete(breaks=c(2020, 2070, 2120, 2170), name = '') +
  scale_y_continuous(name = '') +
  theme(axis.text.x=element_blank(),plot.margin = unit(c(0,0,-1,-1),"lines")) +
  scale_colour_manual(name = "Strategy", values=plotcols, labels = c("RAND", "RARE","EVOL","EDGE", 'COMM'))
p1 <- p1+ theme(legend.position=c(0,0),legend.justification=c(0,0),
              legend.direction="vertical",
              legend.box="vertical",
              legend.box.just = c("right"), 
              legend.text=element_text(size=8),
              legend.title = element_text(size = 8),
              legend.background =  element_rect(fill=alpha('grey', 0.4)),
              legend.key.height=unit(0.75,"line")) +
  


ggsave('figs/figXXId.ma.png', width = 2.75, height = 1.87, unit = 'in', dpi = 600, pointsize = 10)

