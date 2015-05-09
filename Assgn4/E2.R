search = read.table("search.txt")

time =search$time
skill = search$skill
interface = search$interface

#2.1
searchframe = data.frame(time=as.vector(as.matrix(search)),
                         student=factor(rep(1:3,each=15)))

library(multcomp)
require(multcomp)
srcaov = lm(time~skill*interface,data=search)
summary(srcaov)

srcmult = glht(srcaov,linfct=mcp(interface="Tukey"))
#2.2


boxplot(time~skill,xlab="skill",ylab="time", main="Time per Skill")
boxplot(time~interface,xlab="interface",ylab="time", main="Time per Interface")

interaction.plot(skill,interface,time,lwd=2,col=rainbow(3))
interaction.plot(interface,skill,time,lwd=2,col=rainbow(8))

#2.3
kruskal.test(time,interface)

#2.4

xtabs(time~skill+interface,data=search)
#2.5

aovpen=lm(time~skill+interface,data=search)
anova(aovpen)
summary(aovpen)

qqnorm(residuals(aovpen))
plot(fitted(aovpen),residuals(aovpen))

#2.6

friedman.test(time,skill,interface)
