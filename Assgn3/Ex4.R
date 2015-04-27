#4.1

dogs = read.table("dogs.txt", header=T)

boxplot(dogs)

#4.2
for (i in 1:3){
  qqnorm(dogs[,i])
}

#4.3