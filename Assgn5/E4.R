library('corrplot')
data = read.table("expensescrime.txt", header=T)
attach(data)

pairs(data)
dataframe = data.frame(bad,crime,lawyers,employ,pop,expend)
cor(dataframe)

summary(lm(expend~pop, data=data))$r.squared
summary(lm(expend~employ, data=data))$r.squared
summary(lm(expend~lawyers, data=data))$r.squared
summary(lm(expend~crime, data=data))$r.squared
summary(lm(expend~bad, data=data))$r.squared

summary(lm(expend~employ+pop, data=data))$r.squared
summary(lm(expend~employ+lawyers, data=data))$r.squared
summary(lm(expend~employ+crime, data=data))$r.squared
summary(lm(expend~employ+bad, data=data))$r.squared

<<<<<<< HEAD
summary(lm(expend~employ+lawyers+pop, data=data))$r.squared
summary(lm(expend~employ+lawyers+crime, data=data))$r.squared
summary(lm(expend~employ+lawyers+bad, data=data))$r.squared
=======
summary(lm(expend~employ+lawyers+pop, data=data))
summary(lm(expend~employ+lawyers+crime, data=data))
summary(lm(expend~employ+lawyers+bad, data=data))

datalm = lm(expend~employ+lawyers, data=data)
round(cooks.distance(datalm),2)
plot(cooks.distance(datalm))

round(cor(data[,3:7]),2)


plot(residuals(lm(expend~pop, data=data)),pop)
plot(residuals(lm(expend~employ, data=data)),employ)
plot(residuals(lm(expend~lawyers, data=data)),lawyers)
plot(residuals(lm(expend~crime, data=data)),crime)
plot(residuals(lm(expend~bad, data=data)),bad)

# after this one the axis from the other plots will disappear
# so this is done last
corrplot(cor(dataframe), method="shade", type="lower")
>>>>>>> 55268b9cdf30f74060548212894d0a17373d7090
