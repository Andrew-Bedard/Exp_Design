data = read.table('calcium.dat.txt', na.strings="_", header=TRUE)

pairs(data)

#from pairs you can already see, lab 3 messed up the data
#they were probably using the wrong units

#we can also see there are a few lab numbers that are too
#large to make sense

#few errors in sex labels

#few errors in age


#Getting rid of nonsense values, like age greater than
#110, dont ask me how this works!!! I DONT KNOWWW!!!
data <- data.frame(data)

data[!is.na(data$AGE) & !(data$AGE <= 110),]$AGE<-NA

data[!is.na(data$SEX) & !(data$SEX == 1|data$SEX ==2),]$SEX<-NA

data[!is.na(data$LAB) & !(data$LAB < 6),]$LAB<-NA

data[!is.na(data$PHOSMMOL) & !(data$PHOSMMOL < 2),]$PHOSMMOL<-NA



#replacing the messed up values of lab 3 with the values divided by 10
#seems reasonable
data$CAMMOL[!is.na(data$CAMMOL) & (data$CAMMOL > 10)] <- (data$CAMMOL[!is.na(data$CAMMOL) & (data$CAMMOL > 10)])/10

#new pairs with some crazy values removed

pairs(data)

#still some outliers that need to be dealt with
#but its looking much better

data$SEX <- as.factor(data$SEX)
data$LAB <- as.factor(data$LAB)
data$AGEGRP <- as.factor(data$AGEGRP)

qqnorm(data$PHOSMMOL)
plot(data$PHOSMMOL)
hist(data$PHOSMMOL)

data$PHOSMMOL = (data$PHOSMMOL)^2


qqnorm(data$PHOSMMOL)
plot(data$PHOSMMOL)
hist(data$PHOSMMOL)

qqnorm(data$CAMMOL)
plot(data$CAMMOL)
hist(data$CAMMOL)

data$CAMMOL = (data$CAMMOL)^2

qqnorm(data$CAMMOL)
plot(data$CAMMOL)
hist(data$CAMMOL)


attach(data)

boxplot(ALKSPHOS~AGEGRP)
boxplot(CAMMOL~AGEGRP)
boxplot(PHOSMMOL~AGEGRP)

boxplot(ALKSPHOS~SEX)
boxplot(CAMMOL~SEX)
boxplot(PHOSMMOL~SEX)

boxplot(ALKSPHOS~LAB)
boxplot(CAMMOL~LAB)
boxplot(PHOSMMOL~LAB)

alklm = lm(CAMMOL~AGE+SEX+LAB+ALKSPHOS+AGEGRP+PHOSMMOL,data=data)
summary(alklm)

summary(lm(CAMMOL~AGE))$r.squared
summary(lm(CAMMOL~SEX))$r.squared
summary(lm(CAMMOL~ALKSPHOS))$r.squared
summary(lm(CAMMOL~LAB))$r.squared
summary(lm(CAMMOL~PHOSMMOL))$r.squared
summary(lm(CAMMOL~AGEGRP))$r.squared

summary(lm(CAMMOL~SEX+AGE))$r.squared
summary(lm(CAMMOL~SEX+ALKSPHOS))$r.squared
summary(lm(CAMMOL~SEX+LAB))$r.squared
summary(lm(CAMMOL~SEX+PHOSMMOL))$r.squared
summary(lm(CAMMOL~SEX+AGEGRP))$r.squared

summary(lm(CAMMOL~SEX+LAB+AGE))$r.squared
summary(lm(CAMMOL~SEX+LAB+ALKSPHOS))$r.squared
summary(lm(CAMMOL~SEX+LAB+PHOSMMOL))$r.squared
summary(lm(CAMMOL~SEX+LAB+AGEGRP))$r.squared

summary(lm(CAMMOL~SEX+LAB+ALKSPHOS+AGE))$r.squared
summary(lm(CAMMOL~SEX+LAB+ALKSPHOS+PHOSMMOL))$r.squared
summary(lm(CAMMOL~SEX+LAB+ALKSPHOS+AGEGRP))$r.squared
