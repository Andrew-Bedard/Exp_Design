data = read.table("airpollution.txt")

#2.1
pairs(data)

#2.2
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

##What test to show these are usefull?!

summary(lm(oxidant~wind+temperature+humidity,data=data))

#summary gives coefficients of model in estimate column

