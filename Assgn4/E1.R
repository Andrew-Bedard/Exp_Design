bread = read.table("bread.txt")
hours = bread$hours
humidity = bread$humidity
environment = bread$environment

#1.1
sample_slices = sample(1:18, 18)

#1.2
boxplot(hours~humidity)
boxplot(hours~environment)

#1.3
interaction.plot(humidity, environment, hours)
interaction.plot(environment, humidity, hours)

#1.4
aovenvhum = lm(hours ~ environment*humidity, data=bread)
anova(aovenvhum)

#1.7
qqnorm(residuals(aovenvhum))

#1.8
par(mfrow=c(2,2))
plot(aovenvhum)
