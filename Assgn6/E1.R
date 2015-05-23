data = read.table("fruitflies.txt", header=T)
attach(data)

#1.1
#loading data
data$loglongevity = log(longevity)
attach(data)

#1.2
#Various plots
par(mfrow=c(1,2))
plot(activity, loglongevity)
plot(thorax, loglongevity); abline(thorax, loglongevity)
par(mfrow=c(1,1))
pairs(data)


#1.3
activitylm = lm(loglongevity~activity,data=data)
# we reject H0 so the activity has an influence on the longevity
anova(activitylm)

#1.4
#box plots of log(longevity) and activity
boxplot(loglongevity~activity)
iso = data[activity=="isolated", ]
low = data[activity=="low", ]
high = data[activity=="high", ]
summary(iso)
summary(low)
summary(high)
summary(activitylm)

#1.5
activityThoraxlm = lm(loglongevity~activity+thorax,data=data)
# we reject H0 so the activity and thorax has an influence on the longevity
anova(activityThoraxlm)

#1.6
summary(activityThoraxlm)
mean(data$thorax)
#high
mean(data$thorax)*2.97899+1.21893
#isolated
mean(data$thorax)*2.97899+1.21893+0.40998
#low
mean(data$thorax)*2.97899+1.21893+0.28570

min(data$thorax)
#high
min(data$thorax)*2.97899+1.21893
#isolated
min(data$thorax)*2.97899+1.21893+0.40998
#low
min(data$thorax)*2.97899+1.21893+0.28570

#1.7
plot(thorax,loglongevity, xlab="thorax length",ylab="log(longevity)")
par(mfrow = c(1,3))
boxplot(loglongevity ~ thorax, data=iso, ylim = c(2.8, 4.6), main="iso")
boxplot(loglongevity ~ thorax, data=low, ylim = c(2.8, 4.6), main="low")
boxplot(loglongevity ~ thorax, data=high, ylim = c(2.8, 4.6), main="high")

#1.9
par(mfrow = c(1,2))
qqnorm(residuals(activityThoraxlm))
plot(fitted(activityThoraxlm), residuals(activityThoraxlm))

#1.10
longevitylm = lm(longevity~activity+thorax)
anova(longevitylm)
par(mfrow = c(1,2))
qqnorm(residuals(longevitylm))
plot(fitted(longevitylm), residuals(longevitylm))
