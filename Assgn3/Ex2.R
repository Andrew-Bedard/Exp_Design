#2.1
clouds = read.table("clouds.txt", header=T)
len = length(clouds[,1])
nrml = rnorm(len)

#Histograms,boxplots,qqplots to see if data is normal
hist(clouds[,1])
hist(clouds[,2])
qqnorm(clouds[,1])
qqnorm(clouds[,2])
#These graphs should show the data is not normal

#Two sample t-test 
t.test(clouds[ ,1],clouds[ ,2])
#Mann-Whitney test
wilcox.test(clouds[ ,1],clouds[ ,2])
#Kolmogorov-Smirnov test
ks.test(clouds[,1],clouds[,2])

#2.2
sqclouds = sqrt(clouds)

#Histograms,boxplots,qqplots to see if data is normal
hist(sqclouds[,1])
hist(sqclouds[,2])
qqnorm(sqclouds[,1])
qqnorm(sqclouds[,2])
#These graphs should show the data is not normal

#Two sample t-test 
t.test(sqclouds[ ,1],sqclouds[ ,2])
#Mann-Whitney test
wilcox.test(sqclouds[ ,1],sqclouds[ ,2])
#Kolmogorov-Smirnov test
ks.test(sqclouds[,1],sqclouds[,2])

#2.3
sq2clouds = sqrt(sqclouds)

#Histograms,boxplots,qqplots to see if data is normal
hist(sq2clouds[,1])
hist(sq2clouds[,2])
qqnorm(sq2clouds[,1])
qqnorm(sq2clouds[,2])
#These graphs should show the data is not normal

#Two sample t-test
t.test(sq2clouds[ ,1],sq2clouds[ ,2])
#Mann-Whitney test
wilcox.test(sq2clouds[ ,1],sq2clouds[ ,2])
#Kolmogorov-Smirnov test
ks.test(sq2clouds[,1],sq2clouds[,2])

hist(nrml, prob=TRUE)
curve(dnorm(x,mean=mean(nrml),sd=sd(nrml)), add=TRUE)
qqnorm(nrml)