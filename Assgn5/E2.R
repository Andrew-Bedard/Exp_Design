data = read.table("airpollution.txt")

#2.1
pairs(data)

#2.2
par(mfrow=c(2,3))

oxday = lm(oxidant~day, data=data)
plot(oxidant~day, data=data); abline(oxday)
summary(oxday)

oxwind = lm(oxidant~wind, data=data)
plot(oxidant~wind, data=data); abline(oxwind)
summary(oxwind)

oxtemp = lm(oxidant~temperature, data=data)
plot(oxidant~temperature, data=data); abline(oxtemp)
summary(oxtemp)

oxhum = lm(oxidant~humidity, data=data)
plot(oxidant~humidity, data=data); abline(oxhum)
summary(oxhum)

oxins = lm(oxidant~insolation, data=data)
plot(oxidant~insolation, data=data); abline(oxins)
summary(oxins)

#2.3
oxfull = lm(oxidant~day+wind+temperature+humidity+insolation, data=data)
summary(oxfull)

oxdaywindtemphum = lm(oxidant~day+wind+temperature+humidity, data=data)
summary(oxdaywindtemphum)

oxdaywindtemp = lm(oxidant~day+wind+temperature, data=data)
summary(oxdaywindtemp)

par(mfrow=c(1,1))
oxwindtemp = lm(oxidant~wind+temperature, data=data)
summary(oxwindtemp)
plot(oxwindtemp, data=data); abline(oxins)

