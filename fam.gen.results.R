#Analysis of family and genera

m5.f2.genera

m5.f2.genstats
mean(m5.f2.genera$t2030)
overlap(m5.f2.genera$t2070, m3.f2.genera$t2070)
boxplot(m5.f2.genera)

#genera w/ constant funding
boxplot(m2.f2.genera, col = 'blue', outline = F)
boxplot(m3.f2.genera, col = 'red', add = T, outline = F)
boxplot(m4.f2.genera, col = 'green', add = T, outline = F)
boxplot(m5.f2.genera, col = 'orange', add = T, outline = F)
boxplot(m8.f2.genera, col = 'yellow', add = T, outline = F)
boxplot(m6.f2.genera, col = 'purple', add = T, outline = F)

plot(colMeans(m5.f2.genera), type = "l")
lines(colMeans(m8.f2.genera), col = "orange")
lines(colMeans(m3.f2.genera), col = "red")

colMeans(m2.f2.genera)
colMeans(m3.f2.genera)
colMeans(m4.f2.genera)
colMeans(m5.f2.genera)
colMeans(m6.f2.genera)
colMeans(m7.f2.genera)
colMeans(m8.f2.genera)


#families w/ constant funding
boxplot(m2.f2.families, col = 'blue', outline = F)
boxplot(m3.f2.families, col = 'red', add = T, outline = F)
boxplot(m4.f2.families, col = 'green', add = T, outline = F)
boxplot(m5.f2.families, col = 'orange', add = T, outline = F)
boxplot(m8.f2.families, col = 'yellow', add = T, outline = F)
boxplot(m6.f2.families, col = 'purple', add = T, outline = F)

plot(colMeans(m5.f2.families), type = "l", ylim = c(0,17))
lines(colMeans(m8.f2.families), col = "orange")
lines(colMeans(m3.f2.families), col = "red")
lines(colMeans(m2.f2.families), col = "red")
lines(colMeans(m1.cf.families), col = "red")
lines(colMeans(m6.f2.families), col = "red")

