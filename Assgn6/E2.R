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
passest <- function(x,y) {exp(-11.602 + 2.338*x + 3.063*y)/(1 + exp(-11.602 + 2.338*x + 3.063*y))}

#student with gpa 3 and received psi
passest(1,3)
#student with gpa 3 and did not receive psi
passest(0,3)

#2.5
#Increase in odds from having psi vs not having psi
passodds <- function(p,q) {exp(-11.602 + 2.338*p + 3.063*q)}

#2.6
x=matrix(c(3,15,8,6),2,2);x
#if we look at tot, we can see 15 represents students that
#did not receive psi, and did not pass
#6 are those students who did receive psi, but still did not pass
fisher.test(x)

#test concludes that the effects of psi and pass are not
#independant

#2.7
