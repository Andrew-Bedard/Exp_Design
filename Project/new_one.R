data = read.table('calcium.dat.txt', na.strings="_", header=TRUE)

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
data$CAMMOL[!is.na(data$CAMMOL) & (data$CAMMOL < 1.9),]$CAMMOL <- NA

data$CAMMOL[!is.na(data$ALKSPHOS) & (data$ALKSPHOS > 150),]$ALKSPHOS <- NA

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

data = na.omit(data)
data$SEX <- as.factor(data$SEX)
data$LAB <- as.factor(data$LAB)
data$AGEGRP <- as.factor(data$AGEGRP)


anova(lm(CAMMOL~SEX*LAB,data=data))
anova(lm(CAMMOL~SEX*AGEGRP,data=data))
anova(lm(CAMMOL~LAB*AGEGRP,data=data))

anova(lm(ALKSPHOS~SEX*LAB,data=data))
anova(lm(ALKSPHOS~SEX*AGEGRP,data=data))
anova(lm(ALKSPHOS~LAB*AGEGRP,data=data))

anova(lm(PHOSMMOL~SEX*LAB,data=data))
anova(lm(PHOSMMOL~SEX*AGEGRP,data=data))
anova(lm(PHOSMMOL~LAB*AGEGRP,data=data))

#most results suggest there is no interaction effect
#between the factors with the one exception being
#Lab and Agegroup for PHOSMMOL