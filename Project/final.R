data = read.table('calcium.dat.txt', na.strings="_", header=TRUE)

# exploration
pairs(data)
par(mfrow=c(1,3))
qqnorm(data$CAMMOL)
qqnorm(data$ALKSPHOS)
qqnorm(data$PHOSMMOL)

# data preparation
data[!is.na(data$AGE) & !(data$AGE <= 110),]$AGE<-NA
data[!is.na(data$SEX) & !(data$SEX == 1|data$SEX ==2),]$SEX<-NA
data[!is.na(data$LAB) & !(data$LAB < 6),]$LAB<-NA
data[!is.na(data$PHOSMMOL) & !(data$PHOSMMOL < 2),]$PHOSMMOL<-NA
data$CAMMOL[!is.na(data$CAMMOL) & (data$CAMMOL > 10)] <- (data$CAMMOL[!is.na(data$CAMMOL) & (data$CAMMOL > 10)])/10
data[!is.na(data$CAMMOL) & ((data$CAMMOL < 1.9)|(data$CAMMOL >= 3)),]$CAMMOL <- NA
data[!is.na(data$ALKSPHOS) & ((data$ALKSPHOS > 150)|(data$ALKSPHOS < 20)),]$ALKSPHOS <- NA
data$SEX <- as.factor(data$SEX)
data$LAB <- as.factor(data$LAB)
data$AGEGRP <- as.factor(data$AGEGRP)

# exploration
par(mfrow=c(1,1))
pairs(data)
par(mfrow=c(1,3))
qqnorm(data$CAMMOL)
qqnorm(data$ALKSPHOS)
qqnorm(data$PHOSMMOL)
summary(data$AGEGRP)

# data preperation, replace NA with mean
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

# data preperation, make sex M and F instead of 1 and 2
Sex = as.character(sex)
Sex[sex == 1] = "M"
Sex[sex == 2] = "F"
Sex = as.factor(Sex)
AgeGroup = AGEGRP

# interaction plots
interaction.plot(lab,Sex,cammol)
interaction.plot(lab,Sex,alksphos)
interaction.plot(lab,Sex,phosmmol)
interaction.plot(AgeGroup,Sex,cammol)
interaction.plot(AgeGroup,Sex,alksphos)
interaction.plot(AgeGroup,Sex,phosmmol)

# step down linear regression
#ALKSPHOS
summary(lm(ALKSPHOS~AGE+SEX+LAB+AGEGRP,data=data))
summary(lm(ALKSPHOS~AGE+SEX+LAB,data=data))
summary(lm(ALKSPHOS~SEX+LAB,data=data))
#CAMMOL
summary(lm(CAMMOL~AGE+SEX+LAB+AGEGRP,data=data))
summary(lm(CAMMOL~AGE+SEX+LAB,data=data))
summary(lm(CAMMOL~SEX+LAB,data=data))
#PHOSMMOL
summary(lm(PHOSMMOL~AGE+SEX+LAB+AGEGRP,data=data))
summary(lm(PHOSMMOL~AGE+SEX+LAB,data=data))
summary(lm(PHOSMMOL~AGE+SEX,data=data))

#If you looks at graphs, I dont think its fair to
#assume our data is normal. So use non-parametric test
#instead of 1 way anova
kruskal.test(CAMMOL,SEX,data=data)
kruskal.test(CAMMOL,LAB,data=data)
kruskal.test(CAMMOL,AGEGRP,data=data)

kruskal.test(ALKSPHOS,SEX,data=data)
kruskal.test(ALKSPHOS,LAB,data=data)
kruskal.test(ALKSPHOS,AGEGRP,data=data)

kruskal.test(PHOSMMOL,SEX,data=data)
kruskal.test(PHOSMMOL,LAB,data=data)
kruskal.test(PHOSMMOL,AGEGRP,data=data)

#due to difficulties with blocks being of different
#sizes we cannot use friedman test, regular 2-way 
#anova will be used.

anova(lm(CAMMOL~SEX*LAB,data=data))
anova(lm(CAMMOL~SEX*AGEGRP,data=data))
anova(lm(CAMMOL~LAB*AGEGRP,data=data))

anova(lm(ALKSPHOS~SEX*LAB,data=data))
anova(lm(ALKSPHOS~SEX*AGEGRP,data=data))
anova(lm(ALKSPHOS~LAB*AGEGRP,data=data))

anova(lm(PHOSMMOL~SEX*LAB,data=data))
anova(lm(PHOSMMOL~SEX*AGEGRP,data=data))
anova(lm(PHOSMMOL~LAB*AGEGRP,data=data))