#2.1
clouds = read.table("clouds.txt", header=T)

#Histograms,boxplots,qqplots to see if data is normal
hist(clouds[,1])
hist(clouds[,2])
boxplot(clouds[,1],clouds[,2])
qqnorm(clouds[,1])
qqnorm(clouds[,2])
#These graphs should show the data is not normal

#Two sample t-test (I think... maybe not yet, maybe paired=T)
t.test(clouds[ ,1],clouds[ ,2], conf.level=0.95)
#Mann-Whitney test
wilcox.test(clouds[ ,1],clouds[ ,2], conf.level=0.95)
#Kolmogorov-Smirnov test
ks.test(clouds[,1],clouds[,2])

#2.2
clouds = sqrt(clouds)

#Histograms,boxplots,qqplots to see if data is normal
hist(clouds[,1])
hist(clouds[,2])
boxplot(clouds[,1],clouds[,2])
qqnorm(clouds[,1])
qqnorm(clouds[,2])
#These graphs should show the data is not normal

#Two sample t-test (I think... maybe not yet, maybe paired=T)
t.test(clouds[ ,1],clouds[ ,2], conf.level=0.95)
#Mann-Whitney test
wilcox.test(clouds[ ,1],clouds[ ,2], conf.level=0.95)
#Kolmogorov-Smirnov test
ks.test(clouds[,1],clouds[,2])

#2.3
clouds = sqrt(clouds)

#Histograms,boxplots,qqplots to see if data is normal
hist(clouds[,1])
hist(clouds[,2])
boxplot(clouds[,1],clouds[,2])
qqnorm(clouds[,1])
qqnorm(clouds[,2])
#These graphs should show the data is not normal

#Two sample t-test (I think... maybe not yet, maybe paired=T)
t.test(clouds[ ,1],clouds[ ,2], conf.level=0.95)
#Mann-Whitney test
wilcox.test(clouds[ ,1],clouds[ ,2], conf.level=0.95)
#Kolmogorov-Smirnov test
ks.test(clouds[,1],clouds[,2])