#2.1

#2.2
search = read.table("search.txt")

time =search$time
skill = search$skill
interface = search$interface

boxplot(time~skill)
boxplot(time~interface)

interaction.plot(skill,interface,time,lwd=2,col=rainbow(3))
interaction.plot(interface,skill,time,lwd=2,col=rainbow(8))

#2.3
kruskal.test(time,interface)

#2.4

