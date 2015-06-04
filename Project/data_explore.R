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
#the interraction plots dont like NA values

sex = no_na$SEX
lab = no_na$LAB
age = no_na$AGE
agegrp = no_na$AGEGRP
cammol = no_na$CAMMOL
alksphos = no_na$ALKSPHOS
phosmmol = no_na$PHOSMMOL


#Interaction Plots

interaction.plot(sex,lab,cammol)
interaction.plot(lab,sex,cammol)
interaction.plot(lab,agegrp,cammol)
interaction.plot(agegrp,lab,cammol)
interaction.plot(sex,agegrp,cammol)
interaction.plot(agegrp,sex,cammol)


interaction.plot(sex,lab,alksphos)
interaction.plot(lab,sex,alksphos)
interaction.plot(lab,agegrp,alksphos)
interaction.plot(agegrp,lab,alksphos)
interaction.plot(sex,agegrp,alksphos)
interaction.plot(agegrp,sex,alksphos)

interaction.plot(sex,lab,phosmmol)
interaction.plot(lab,sex,phosmmol)
interaction.plot(lab,agegrp,phosmmol)
interaction.plot(agegrp,lab,phosmmol)
interaction.plot(sex,agegrp,phosmmol)
interaction.plot(agegrp,sex,phosmmol)

agegrp<-as.factor(agegrp)
sex<-as.factor(sex)
lab<-as.factor(lab)
id<-no_na$OBSNO
friedman.test(cammol,agegrp,sex)
friedman.test(cammol,sex,id)

data$SEX <- as.factor(data$SEX)
data$LAB <- as.factor(data$LAB)
data$AGEGRP <- as.factor(data$AGEGRP)
attach(data)


#Top down linear models
#CAMMOL
summary(lm(CAMMOL~AGE+SEX+LAB+AGEGRP,data=data))
summary(lm(CAMMOL~AGE+SEX+LAB,data=data))
summary(lm(CAMMOL~SEX+LAB,data=data))
camlm1 = lm(log(CAMMOL)~SEX+LAB,data=data)
plot(cooks.distance(camlm1))
qqnorm(residuals(camlm1))
plot(fitted(camlm1),residuals(camlm1))




summary(lm(log(CAMMOL)~AGE))$r.squared
summary(lm(log(CAMMOL)~SEX))$r.squared
summary(lm(log(CAMMOL)~LAB))$r.squared
summary(lm(log(CAMMOL)~AGEGRP))$r.squared
summary(lm(log(CAMMOL)~AGE*SEX*LAB*AGEGRP))$r.squared

summary(lm(log(CAMMOL)~AGE*SEX*LAB*AGEGRP+AGE))$r.squared
summary(lm(log(CAMMOL)~AGE*SEX*LAB*AGEGRP+SEX))$r.squared
summary(lm(log(CAMMOL)~AGE*SEX*LAB*AGEGRP+LAB))$r.squared
summary(lm(log(CAMMOL)~AGE*SEX*LAB*AGEGRP+AGEGRP))$r.squared

lm2 = lm(log(CAMMOL)~AGE*SEX*LAB*AGEGRP+AGEGRP)
plot(cooks.distance(lm2))
qqnorm(residuals(lm2))
plot(fitted(lm2),residuals(lm2))

data=data[-149,]
data=data[-22,]

shapiro.test(residuals(lm2))


labaov=lm(CAMMOL~AGE+LAB,data=data)
anova(labaov)
drop1(labaov,test="F")

aovlab=lm(CAMMOL~LAB*AGE,data=data)
summary(aovlab)


camglm=glm(CAMMOL~AGE+SEX+LAB+AGEGRP,data=data)
summary(camglm)

camsex = lm(CAMMOL~SEX,data=data)
summary(camsex)

alksex = lm(ALKSPHOS~SEX,data=data)
summary(alksex)

phosex = lm(PHOSMMOL~SEX,data=data)
summary(phosex)

qqnorm(data$CAMMOL)