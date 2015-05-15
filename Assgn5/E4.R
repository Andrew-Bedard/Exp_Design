library('corrplot')
data = read.table("expensescrime.txt", header=T)
attach(data)

# looking at correlations, 
# the correlation plot is on the last line of the program
pairs(data)
dataframe = data.frame(bad,crime,lawyers,employ,pop,expend)
round(cor(dataframe), 2)

# step-up algorithm
# stop after second step
summary(lm(expend~pop))
summary(lm(expend~employ))
summary(lm(expend~lawyers))
summary(lm(expend~crime))
summary(lm(expend~bad))

summary(lm(expend~employ+pop))
summary(lm(expend~employ+lawyers))
summary(lm(expend~employ+crime))
summary(lm(expend~employ+bad))

summary(lm(expend~employ+lawyers+pop))
summary(lm(expend~employ+lawyers+crime))
summary(lm(expend~employ+lawyers+bad))

# step-down algorithm
# same result as step-up so continue with this
summary(lm(expend~employ+lawyers+bad+pop+crime))
summary(lm(expend~employ+lawyers+bad+pop))
summary(lm(expend~employ+lawyers+bad))
summary(lm(expend~employ+lawyers))

# influence points are the ones that stand out in cooks distance
datalm = lm(expend~employ+lawyers)
round(cooks.distance(datalm),2)
par(mfrow=c(1,1))
plot(cooks.distance(datalm))

# problems with colinearty, the corrplot is on the bottom
round(cor(data[,3:7]),2)

# looking at the different residuals
par(mfrow=c(1,2))
plot(residuals(datalm),employ)
plot(residuals(datalm),lawyers)
par(mfrow=c(2,2))
plot(residuals(datalm),pop)
plot(residuals(datalm),crime)
plot(residuals(datalm),bad)
par(mfrow=c(1,2))
plot(residuals(datalm),expend)
plot(residuals(datalm),fitted(datalm))
par(mfrow=c(1,1))
qqnorm(residuals(datalm))

# after this one the axis from the other plots somehow disappeared
# so this is done last
corrplot(cor(dataframe), method="shade", type="lower")
corrplot(cor(data[,3:7]), method="shade", type="lower")