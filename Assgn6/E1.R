data = read.table("fruitflies.txt", header=T)
attach(data)

#1.1
data$loglongevity = log(longevity)
attach(data)

#1.2
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
boxplot(loglongevity~activity)
iso = data[activity=="isolated", ]
low = data[activity=="low", ]
high = data[activity=="high", ]
summary(iso)
summary(low)
summary(high)

#1.5
activityThoraxlm = lm(loglongevity~activity+thorax,data=data)
# we reject H0 so the activity and thorax has an influence on the longevity
anova(activityThoraxlm)

#1.6
summary(activityThoraxlm)

#1.7
plot(loglongevity, thorax)
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
