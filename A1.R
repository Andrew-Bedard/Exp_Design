load(file="assign1.RData")

par(mfrow=c(1,2))
hist(x1)
qqnorm(x1)

hist(x2)
qqnorm(x2)

hist(x3)
qqnorm(x3)

hist(x4)
qqnorm(x4)

hist(x5)
qqnorm(x5)

par(mfrow=c(1,1))
par(pin=c(2.5,5))
x1_2=rnorm(20)
qqnorm(x1_2)

x2_2=rnorm(100)
qqnorm(x2_2)

x3_2=rnorm(100)
qqnorm(x3_2)

x4_2=rnorm(30)
qqnorm(x4_2)

x5_2=rnorm(40)
qqnorm(x5_2)