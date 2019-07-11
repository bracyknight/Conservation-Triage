## Figure 3

## FIG fig3 Rarity for RARE, COMM, and RAND


# munge the data into long form
m3.f2.surv <- m3.f2.survived %>%
  select(starts_with('t')) %>%
  gather(key = row)
m3.f2.surv$model <- "m3"

# munge the data into long form
m2.f2.surv <- m2.f2.survived %>%
  select(starts_with('t')) %>%
  gather(key = row)
m2.f2.surv$model <- "m2"

# munge the data into long form
m5.f2.surv <- m5.f2.survived %>%
  select(starts_with('t')) %>%
  gather(key = row)
m5.f2.surv$model <- "m5"

# munge the data into long form
m4.f2.surv <- m4.f2.survived %>%
  select(starts_with('t')) %>%
  gather(key = row)
m4.f2.surv$model <- "m4"

survmodel <- rbind(m2.f2.surv, m3.f2.surv, m4.f2.surv, m5.f2.surv )
survmodel <-data.frame(survmodel)
names(survmodel) <- c('year','prop.surv','mod')
survmodel$year <- sub('.', '', survmodel$year)
survmodel$year <- as.numeric(survmodel$year)
survmodel <- data.frame(survmodel)

plotcols <- c('yellow','red','orange')
plotcols <- c('red','pink','orange','purple')


# THIS FUNCTION DEFINES THE 0.975 AND 0.025 INTERVALS FOR MY SPECIAL BOXPLOTS
f <- function(x) {
  r <- quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

fig3 <- ggplot(survmodel, aes( x = year, y = prop.surv)) + 
  stat_summary(fun.data = f, geom = 'boxplot',alpha = 0.7, aes(fill = mod, group = mod), size = 0.2) +
  theme_classic()  +
  labs(y = "Percent Species Surviving", x = "Decade") 


fig3 <- fig3 + theme(legend.position=c(1,1),legend.justification=c(1,1),
                   legend.direction="vertical",
                   legend.box="vertical",
                   legend.box.just = c("top"), 
                   legend.text=element_text(size=8),
                   legend.background =  element_rect(fill=alpha('grey', 0.4)),
                   legend.key.height=unit(0.75,"line")) +
  scale_fill_manual(name = "Conservation Strategy",
                    values=plotcols,
                    labels=c('RAND', 'RARE','EVOL', 'EDGE'))
fig3 <- fig3 + scale_x_continuous(name="Decade", breaks=c(2020,2070,2120,2170))

fig3

png("figs/fig3.png", width = 6.5, height = 4, units = 'in', pointsize = 10, res = 600, type = "quartz")
fig3
dev.off()
