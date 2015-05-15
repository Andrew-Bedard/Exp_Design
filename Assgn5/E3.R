data = read.table("genal2.txt")

#3.1
y = c()
mut = c()
# gives X0.005, so not useable
names = names(data)
length(data[1,])
length(data[,1])
for (i in 1:length(data[1,])){
  for (j in 1:length(data[,1])){
    y = c(y, data[j,i])
    mut = c(mut, 0.005*i)
  }
}
genal2frame=data.frame(y,mut)

#3.2
boxplot(data)

#3.3
genal2frame$mut2=genal2frame$mut^2
genal2lm=lm(y~mut+mut2,data=genal2frame)
summary(genal2lm)

#3.4