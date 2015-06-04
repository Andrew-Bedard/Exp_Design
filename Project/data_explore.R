data = read.table('calcium.dat.txt', na.strings="_", header=TRUE)

library(corrplot)

pairs(data)

# from pairs you can already see, lab 3 messed up the data
# they were probably using the wrong units

# we can also see there are a few lab numbers that are too
# large to make sense

# few errors in sex labels

# few errors in age


# Getting rid of nonsense values, like age greater than
# 110, dont ask me how this works!!! I DONT KNOWWW!!!
# you don't need to cast to a data frame
#data <- data.frame(data)

# set all age data which is not na and not smaller than 110 (so the outliers) at NA
data[!is.na(data$AGE) & !(data$AGE <= 110),]$AGE<-NA

data[!is.na(data$SEX) & !(data$SEX == 1|data$SEX ==2),]$SEX<-NA

data[!is.na(data$LAB) & !(data$LAB < 6),]$LAB<-NA

data[!is.na(data$PHOSMMOL) & !(data$PHOSMMOL < 2),]$PHOSMMOL<-NA



# replacing the messed up values of lab 3 with the values divided by 10
# seems reasonable
data$CAMMOL[!is.na(data$CAMMOL) & (data$CAMMOL > 10)] <- (data$CAMMOL[!is.na(data$CAMMOL) & (data$CAMMOL > 10)])/10

# new pairs with some crazy values removed

pairs(data)

# still some outliers that need to be dealt with
# but its looking much better

#Correlation plot

no_na <- na.omit(data)
M <- cor(no_na)
corrplot(M, method = "color", type = "lower")

#Few things should be factors
data$SEX <- as.factor(data$SEX)
data$LAB <- as.factor(data$LAB)
data$AGEGRP <- as.factor(data$AGEGRP)

#Lets see if we can normalize some of the data
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

qqnorm(data$ALKSPHOS)
plot(data$ALKSPHOS)
hist(data$ALKSPHOS)

boxplot(data$ALKSPHOS~data$AGEGRP)
boxplot(data$CAMMOL~data$AGEGRP)
boxplot(data$PHOSMMOL~data$AGEGRP)

boxplot(data$ALKSPHOS~data$SEX)
boxplot(data$CAMMOL~data$SEX)
boxplot(data$PHOSMMOL~data$SEX)

boxplot(data$ALKSPHOS~data$LAB)
boxplot(data$CAMMOL~data$LAB)
boxplot(data$PHOSMMOL~data$LAB)


#!!!!!!!!!!!!!!!!!!!!!!!#
#This is not working nicely with the interraction
#plots, it doesnt like NA values, but it also doesnt
#like the lengths being different
#tomorrow I will try to replace NA values with the mean
#as this shouldnt affect the interraction plots outcome

sex = data$SEX
sex[is.na(sex)]<-mean(na.omit(data$SEX))
lab = data$LAB
lab[is.na(lab)]<-mean(na.omit(data$LAB))
cammol = data$CAMMOL
cammol[is.na(cammol)]<-mean(na.omit(data$CAMMOL))
alksphos = data$ALKSPHOS
alksphos[is.na(alksphos)]<-mean(na.omit(data$ALKSPHOS))
phosmmol = data$PHOSMMOL
phosmmol[is.na(phosmmol)]<-mean(na.omit(data$PHOSMMOL))

#!!!!!!!!!!!!!!!!!!!!!!!#

#Interaction Plots
#We need to justify the assumptions that concentrations will
#Depend only on sex and lab, ie, not on age/age group
interaction.plot(sex,lab,cammol)
interaction.plot(lab,sex,cammol)

interaction.plot(sex,lab,alksphos)
interaction.plot(lab,sex,alksphos)

interaction.plot(sex,lab,phosmmol)
interaction.plot(lab,sex,phosmmol)

#Top down linear models
#CAMMOL
summary(lm(CAMMOL^2~AGE+SEX+LAB+AGEGRP,data=data))
summary(lm(CAMMOL~AGE+SEX+LAB,data=data))
summary(lm(CAMMOL~SEX+LAB,data=data))
camlm1 = lm(CAMMOL~SEX+LAB,data=data)
plot(cooks.distance(camlm1))
qqnorm(log(residuals(camlm1)))
plot(fitted(camlm1),log(residuals(camlm1)))



summary(lm(CAMMOL~AGE))$r.squared
summary(lm(CAMMOL~SEX))$r.squared
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

lm2 = lm(CAMMOL~SEX+LAB+ALKSPHOS)
plot(cooks.distance(lm2))

