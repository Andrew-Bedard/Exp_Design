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
aovenvhum = lm(hours ~ environment+humidity, data=bread)
anova(aovenvhum)

aovenv = lm(hours ~ environment, data=bread)
anova(aovenv)

aovhum = lm(hours ~ humidity, data=bread)
anova(aovhum)

#1.5
residuals(aovhum)
qqnorm(residuals(aovhum))
qqnorm(residuals(aovenv))
qqnorm(residuals(aovenvhum))
