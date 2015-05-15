data = read.table("airpollution.txt")

#2.1
pairs(data)

#2.2

#create simple models, plot for reference
par(mfrow=c(2,3))

oxday = lm(oxidant~day, data=data)
plot(oxidant~day, data=data); abline(oxday)

oxwind = lm(oxidant~wind, data=data)
plot(oxidant~wind, data=data); abline(oxwind)

oxtemp = lm(oxidant~temperature, data=data)
plot(oxidant~temperature, data=data); abline(oxtemp)

oxhum = lm(oxidant~humidity, data=data)
plot(oxidant~humidity, data=data); abline(oxhum)

oxins = lm(oxidant~insolation, data=data)
plot(oxidant~insolation, data=data); abline(oxins)

summary(oxday)$r.squared
summary(oxwind)$r.squared
summary(oxtemp)$r.squared
summary(oxhum)$r.squared
summary(oxins)$r.squared

#wind has highest R squared, so add to model

summary(lm(oxidant~wind+day,data=data))$r.squared
summary(lm(oxidant~wind+temperature,data=data))$r.squared
summary(lm(oxidant~wind+humidity,data=data))$r.squared
summary(lm(oxidant~wind+insolation,data=data))$r.squared

#temperature has highest R squared, add to model

summary(lm(oxidant~wind+temperature+day ,data=data))$r.squared
summary(lm(oxidant~wind+temperature+humidity ,data=data))$r.squared
summary(lm(oxidant~wind+temperature+insolation ,data=data))$r.squared

#adding humidity still increased R squared, add to model

summary(lm(oxidant~wind+temperature+humidity+day ,data=data))$r.squared
summary(lm(oxidant~wind+temperature+humidity+insolation ,data=data))$r.squared

#adding either day or insolation yields insignificant explanatory variables
#so we stop at previous step

summary(lm(oxidant~wind+temperature+humidity,data=data))

#summary gives coefficients of model in estimate column

#2.3

summary(lm(oxidant~day+wind+temperature+humidity+insolation, data=data))

#Try removing day, (highest p-value)
summary(lm(oxidant~wind+temperature+humidity+insolation, data=data))

#removing insolation, (highest p-value)
summary(lm(oxidant~wind+temperature+humidity, data=data))

#removing humidity, p-value too high still
summary(lm(oxidant~wind+temperature, data=data))

#2.4
#Present stepwise increase, less variables, but similar result

#2.5
step_up = lm(oxidant~wind+temperature+humidity,data=data)

par(mfrow=c(1,2))
qqnorm(residuals(step_up))
plot(fitted(step_up),residuals(step_up))