data = read.table('calcium.dat.txt', na.strings="_", header=TRUE)

# Getting rid of nonsense values, like age greater than
# 110, dont ask me how this works!!! I DONT KNOWWW!!!
# you don't need to cast to a data frame
#data <- data.frame(data)

par(mfrow=c(1,3))
qqnorm(data$CAMMOL)
qqnorm(data$ALKSPHOS)
qqnorm(data$PHOSMMOL)

# set all age data which is not na and not smaller than 110 (so the outliers) at NA
data[!is.na(data$AGE) & !(data$AGE <= 110),]$AGE<-NA

data[!is.na(data$SEX) & !(data$SEX == 1|data$SEX ==2),]$SEX<-NA

data[!is.na(data$LAB) & !(data$LAB < 6),]$LAB<-NA

data[!is.na(data$PHOSMMOL) & ((data$PHOSMMOL > 2)|(data$PHOSMMOL < 0.2)),]$PHOSMMOL<-NA

# replacing the messed up values of lab 3 with the values divided by 10
# seems reasonable
data$CAMMOL[!is.na(data$CAMMOL) & (data$CAMMOL > 10)] <- (data$CAMMOL[!is.na(data$CAMMOL) & (data$CAMMOL > 10)])/10
data[!is.na(data$CAMMOL) & ((data$CAMMOL < 1.9)|(data$CAMMOL >= 3)),]$CAMMOL <- NA

data[!is.na(data$ALKSPHOS) & ((data$ALKSPHOS > 150)|(data$ALKSPHOS < 20)),]$ALKSPHOS <- NA


qqnorm(data$CAMMOL)
qqnorm(data$ALKSPHOS)
qqnorm(data$PHOSMMOL)


data$SEX <- as.factor(data$SEX)
data$LAB <- as.factor(data$LAB)
data$AGEGRP <- as.factor(data$AGEGRP)

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

#for cammol and alksphos, sex and lab are significant
#for phosmmol sex and age group are significant


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


#NO interactions

#plots for checking normality of risiduals

par(mfrow=c(3,3))
qqnorm(residuals(lm(CAMMOL~SEX*LAB,data=data)))
qqnorm(residuals(lm(CAMMOL~SEX*AGEGRP,data=data)))
qqnorm(residuals(lm(CAMMOL~LAB*AGEGRP,data=data)))

qqnorm(residuals(lm(ALKSPHOS~SEX*LAB,data=data)))
qqnorm(residuals(lm(ALKSPHOS~SEX*AGEGRP,data=data)))
qqnorm(residuals(lm(ALKSPHOS~LAB*AGEGRP,data=data)))

qqnorm(residuals(lm(PHOSMMOL~SEX*LAB,data=data)))
qqnorm(residuals(lm(PHOSMMOL~SEX*AGEGRP,data=data)))
qqnorm(residuals(lm(PHOSMMOL~LAB*AGEGRP,data=data)))

plot(fitted(lm(CAMMOL~SEX*LAB,data=data)),residuals(lm(CAMMOL~SEX*LAB,data=data)),ylab='')
plot(fitted(lm(CAMMOL~SEX*AGEGRP,data=data)),residuals(lm(CAMMOL~SEX*AGEGRP,data=data)),ylab='')
plot(fitted(lm(CAMMOL~LAB*AGEGRP,data=data)),residuals(lm(CAMMOL~LAB*AGEGRP,data=data)),ylab='')

plot(fitted(lm(ALKSPHOS~SEX*LAB,data=data)),residuals(lm(ALKSPHOS~SEX*LAB,data=data)),ylab='')
plot(fitted(lm(ALKSPHOS~SEX*AGEGRP,data=data)),residuals(lm(ALKSPHOS~SEX*AGEGRP,data=data)),ylab='')
plot(fitted(lm(ALKSPHOS~LAB*AGEGRP,data=data)),residuals(lm(ALKSPHOS~LAB*AGEGRP,data=data)),ylab='')

plot(fitted(lm(PHOSMMOL~SEX*LAB,data=data)),residuals(lm(PHOSMMOL~SEX*LAB,data=data)),ylab='')
plot(fitted(lm(PHOSMMOL~SEX*AGEGRP,data=data)),residuals(lm(PHOSMMOL~SEX*AGEGRP,data=data)),ylab='')
plot(fitted(lm(PHOSMMOL~LAB*AGEGRP,data=data)),residuals(lm(PHOSMMOL~LAB*AGEGRP,data=data)),ylab='')