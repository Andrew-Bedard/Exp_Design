#2.1
data = read.table("psi.txt", header=T)
attach(data)

#Table and histogram
tot=xtabs(~psi+passed); tot
hist(data[,1],main="passed")

#2.2

#Estimation
passedlm = glm(passed~psi+gpa,family=binomial)
summary(passedlm)

data$psi=factor(data$psi)
data$gpa=factor(data$gpa)
passedlm2 = glm(passed~psi+gpa, family=binomial)
summary(passedlm2)

plot(c(0,coef(passedlm2)[2:3]),type="l")

#2.3
#Addording to p-values, psi works

#2.4

#function to predict passing, x=psi, y=gpa
passest <- function(x,y) {-11.602 + 2.338*x + 3.063*y}

passest(1,3)
passest(0,3)