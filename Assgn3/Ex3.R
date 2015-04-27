#3.1
genal = read.table("genal.txt", header=T)

boxplot(genal)

#3.2
#Loops through, creating QQ-plot for each mutation probability
for (i in 1:7) {
  qqnorm(genal[,i],main =paste("Normal Q-Q Plot for Mutation Probabiliy =",i/10))
}

#3.3
sqgenal=sqrt(genal)

#Loops through, creating QQ-plot for each mutation probability
for (i in 1:7) {
  qqnorm(sqgenal[,i],main =paste("Normal Q-Q Plot for Mutation Probabiliy =",i/10))
}

#3.4