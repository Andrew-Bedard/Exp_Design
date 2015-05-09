#Note: Interest was in the effect of the type of Yogurt
#used as a starter!


cream = read.table("cream.txt",header=T)
cream$starter = factor(cream$starter)
cream$position = factor(cream$position)

#3.1

anovacid = lm(acidity~starter+batch+position,data=cream)
anova(anovacid)

#3.2
summary(anovacid)

#3.3

#cream = read.table("cream.txt",header=T)
library(lme4)
creamlmer = lmer(acidity~starter+position+(1|batch),data=cream,REML=FALSE)

#creamlm = lm(acidity~starter*position+batch+batch:starter,data=cream)
summary(creamlmer)

creamlmer1 = lmer(acidity~position+(1|batch),data=cream,REML=FALSE)
anova(creamlmer1,creamlmer)
