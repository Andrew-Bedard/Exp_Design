library(multcomp)

cream = read.table("cream.txt",header=T)
cream$starter = factor(cream$starter)
cream$position = factor(cream$position)

#3.1

creamlm = lm(acidity~starter+batch+position,data=cream)
anova(creamlm)

#3.2
summary(creamlm)

#3.3
tmp = glht(creamlm, linfct=mcp(starter="Tukey"))
summary(tmp)

#3.4
confint(tmp)
