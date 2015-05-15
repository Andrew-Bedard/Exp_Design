library('corrplot')
data = read.table("expensescrime.txt", header=T)
attach(data)

# looking at correlations, 
# the correlation plot is on the last line of the program
pairs(data)
dataframe = data.frame(bad,crime,lawyers,employ,pop,expend)
round(cor(dataframe), 2)

# step-up algorithm
# examine r squared
summary(lm(expend~pop))$r.squared
summary(lm(expend~employ))$r.squared
summary(lm(expend~lawyers))$r.squared
summary(lm(expend~crime))$r.squared
summary(lm(expend~bad))$r.squared

summary(lm(expend~employ+pop))$r.squared
summary(lm(expend~employ+lawyers))$r.squared
summary(lm(expend~employ+crime))$r.squared
summary(lm(expend~employ+bad))$r.squared

summary(lm(expend~employ+lawyers+pop))$r.squared
summary(lm(expend~employ+lawyers+crime))$r.squared
summary(lm(expend~employ+lawyers+bad))$r.squared

summary(lm(expend~employ+lawyers))

# step-down algorithm
# same result as step-up so continue with this
summary(lm(expend~employ+lawyers+bad+pop+crime))
summary(lm(expend~employ+lawyers+bad+pop))
summary(lm(expend~employ+lawyers+bad))
summary(lm(expend~employ+lawyers))

# influence points are the ones that stand out in cooks distance
datalm = lm(expend~employ)
round(cooks.distance(datalm),2)
par(mfrow=c(1,1))
plot(cooks.distance(datalm))

# problems with colinearty, the corrplot is on the bottom
round(cor(data[,3:7]),2)

# looking at the different residuals
par(mfrow=c(1,1))
plot(residuals(datalm),employ)
par(mfrow=c(2,3))
plot(residuals(datalm),lawyers)
plot(residuals(datalm),pop)
plot(residuals(datalm),crime)
plot(residuals(datalm),bad)
plot(residuals(datalm),state)
par(mfrow=c(1,2))
plot(residuals(datalm),expend)
plot(residuals(datalm),fitted(datalm))
par(mfrow=c(1,1))
qqnorm(residuals(datalm))

pairs(~expend+employ+lawyers)
cor(employ,lawyers)