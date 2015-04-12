# Exercise 1

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
qqnorm(rnorm(20))

qqnorm(rnorm(100))

qqnorm(rnorm(100))

qqnorm(rnorm(30))

qqnorm(rnorm(40))



# Exercise 2
#2.1
m=30
n=30
mu=180
nu=180
sd=10
B=1000
p=numeric(B)

for (b in 1:B) {
  x=rnorm(m,mu,sd)
  y=rnorm(n,nu,sd)
  p[b]=t.test(x,y,var.equal=TRUE)[[3]]
}

sum(p<0.05)
sum(p<0.1)
hist(p)

#2.2
m=30
n=30
mu=180
nu=180
sd=1
B=1000
p=numeric(B)

for (b in 1:B) {
  x=rnorm(m,mu,sd)
  y=rnorm(n,nu,sd)
  p[b]=t.test(x,y,var.equal=TRUE)[[3]]
}

sum(p<0.05)
sum(p<0.1)
hist(p)