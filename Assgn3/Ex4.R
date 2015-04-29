#4.1

dogs = read.table("dogs.txt", header=T)
len=length(dogs[,1])
boxplot(dogs,ylab="Concentration (1.0e-9g/mm)")

#4.2
par(mfrow=c(2,2))
qqnorm(dogs[,1],ylab="Isofluorane Concentration")
qqnorm(dogs[,2],ylab="Halothane Concentration")
qqnorm(dogs[,3],ylab="Cyclopropane Concentration")

#4.3
dogframe = data.frame(yield=as.vector(as.matrix(dogs)),
                      variety=factor(rep(1:3,each=10)))
dogaov=lm(yield~variety,data=dogframe)
anova(dogaov)
summary(dogaov)

#Calculate expected value
u1=0.4340; 
u2=0.0350+u1
u3=0.4190+u1

print(u1,u2,u3)
#4.4

attach(dogframe)
kruskal.test(yield,variety)
par(mfrow=c(1,1));qqnorm(dogaov$residuals)

#Calculate and print population variances
for (i in 1:3){
  print(sum((dogs[,i]-mean(dogs[,i]))^2)/(len-1))
}