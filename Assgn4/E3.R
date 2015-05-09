library(multcomp)

cream = read.table("cream.txt", header=T)
acidity = cream$acidity
cream$starter=as.factor(cream$starter)
cream$batch=as.factor(cream$batch)
cream$position=as.factor(cream$position)

#3.1
creamlm = lm(acidity ~ starter+batch+position, data=cream)
anova(creamlm)

#3.2
summary(creamlm)

#3.3
tmp = glht(creamlm, linfct=mcp(starter="Tukey"))
summary(tmp)
confint(tmp)