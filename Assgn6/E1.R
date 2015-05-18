data = read.table("fruitflies.txt", header=T)
attach(data)

#1.1
data$loglongevity = log(longevity)
attach(data)

#1.2
par(mfrow=c(1,2))
plot(activity, loglongevity)
plot(thorax, loglongevity); abline(thorax, loglongevity)
