#4.1

dogs = read.table("dogs.txt", header=T)

boxplot(dogs)

#4.2
par(mfrow=c(2,2))
for (i in 1:3){
  qqnorm(dogs[,i])
}

#4.3
dogframe = data.frame(yield=as.vector(as.matrix(dogs)),
                      variety=factor(rep(1:3,each=10)))
dogaov=lm(yield~variety,data=dogframe)
anova(dogaov)
summary(dogaov)

#u1=0.4340, u2=0.0350+0.4340, u3=0.4190+4340

#4.4

attach(dogframe)
kruskal.test(yield,variety)
par(mfrow=c(1,1));qqnorm(dogaov$residuals)