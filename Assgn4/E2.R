#Note: The interface is of greatest importance

search = read.table("search.txt")

time =search$time
skill = search$skill
interface = search$interface

#2.1
#student = as.vector(as.matrix)

I=3; B=5; N=1
for (i in 1:B) print(sample(1:(N*I)))

#2.2


boxplot(time~skill,xlab="skill",ylab="time", main="Time per Skill")
boxplot(time~interface,xlab="interface",ylab="time", main="Time per Interface")

interaction.plot(skill,interface,time,lwd=2,col=rainbow(3))
interaction.plot(interface,skill,time,lwd=2,col=rainbow(8))

#2.3
kruskal.test(time,interface)
#look at previous assignment for this one


#2.4
#library(multcomp)
#srcaov = lm(time~skill*interface,data=search)
#summary(srcaov)
#srcmult = glht(srcaov,linfct=mcp(skill="Tukey"))

aovsrch=lm(time~skill+interface,data=search)
anova(aovsrch)
summary(aovsrch)

anovskill = lm(time~skill,data=search)
anova(anovskill)
anovinter = lm(time~interface, data=search)
anova(anovinter)
#2.5



qqnorm(residuals(aovsrch))
plot(fitted(aovsrch),residuals(aovsrch))

#2.6

friedman.test(time,skill,interface)

#2.7

anv = lm(time~skill*interface,data=search)
anova(anv)
