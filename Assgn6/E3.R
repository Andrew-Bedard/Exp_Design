data = read.table("africa.txt", header=T)
attach(data)

#3.1
par(mfrow=c(2,3))
hist(rpois(20,5))
hist(rpois(20,10))
hist(rpois(20,30))
hist(rpois(100,5))
hist(rpois(100,10))
hist(rpois(100,30))

hist(rpois(300,10))
hist(rpois(300,20))
hist(rpois(300,30))
hist(rpois(400,10))
hist(rpois(400,20))
hist(rpois(400,30))

#3.2
hist(rpois(400,20))
hist(rpois(400,30))
hist(rpois(400,40))
hist(rpois(400,50))
hist(rpois(400,60))
hist(rpois(400,70))
par(mfrow=c(1,1))

#3.3

fullafricalm = glm(miltcoup~oligarchy+pollib+parties+pctvote
               +popn+size+numelec+numregim, family=poisson,data=data)
summary(fullafricalm)

#3.4
#remove numelec, has highest p-value

africalm = glm(miltcoup~oligarchy+pollib+parties+pctvote
               +popn+size+numregim, family=poisson,data=data)
summary(africalm)

#remove numregim, has highest p-value

africalm = glm(miltcoup~oligarchy+pollib+parties+pctvote
               +popn+size, family=poisson,data=data)
summary(africalm)

#remove size, has highest p-value

africalm = glm(miltcoup~oligarchy+pollib+parties+pctvote
               +popn, family=poisson,data=data)
summary(africalm)

#remove popn, has highest p-value

africalm = glm(miltcoup~oligarchy+pollib+parties+pctvote,
               family=poisson,data=data)
summary(africalm)

#remove pctvote, has highest p-value

africalm = glm(miltcoup~oligarchy+pollib+parties,
               family=poisson,data=data)
summary(africalm)

confint(africalm)
coef(africalm)

#3.5

#plot of model
plot(fitted(africalm),residuals(africalm))
plot(log(fitted(africalm)),residuals(africalm))
plot(log(fitted(africalm)),residuals(africalm,type="response"))
#plots of full model
plot(fitted(fullafricalm),residuals(fullafricalm))
plot(log(fitted(fullafricalm)),residuals(fullafricalm))
plot(log(fitted(fullafricalm)),residuals(fullafricalm,type="response"))

pairs(miltcoup~oligarchy+pollib+parties,data=data)

data$pollib = as.factor(data$pollib)
africalm = glm(miltcoup~oligarchy+pollib+parties,
               family=poisson,data=data)
summary(africalm)

pairs(miltcoup~oligarchy+parties,data=data)