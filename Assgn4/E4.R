library(lme4)

cow = read.table("cow.txt",header=T)
cow$order = factor(cow$order)
order = cow$order
milk = cow$milk
cow$treatment = factor(cow$treatment)
treatment = cow$treatment

#4.1
cowlm = lm(milk ~ treatment + order, data=cow)
anova(cowlm)
summary(cowlm)

#4.2
summary(cow$milk)

#4.3
cowlmer = lmer(milk ~ treatment + order+(1|id), data=cow, REML=FALSE)
anova(cowlmer)
summary(cowlmer)

#4.4
attach(cow)
t.test(milk[treatment=="A"],milk[treatment=="B"],paired=TRUE)
