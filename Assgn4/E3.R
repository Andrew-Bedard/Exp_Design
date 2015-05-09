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
tmp = glht(creamlm, linfct=mcp(starter="Tukey"))
summary(tmp)

#3.4
confint(tmp)
