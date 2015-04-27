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

#Create data-frames for the origional genal data, and the squareroot genal data
genalframe=data.frame(yield=as.vector(as.matrix(genal)),
                      variety=factor(rep(1:7,each=100)))

sqframe=data.frame(yield=as.vector(as.matrix(sqgenal)),
                      variety=factor(rep(1:7,each=100)))

