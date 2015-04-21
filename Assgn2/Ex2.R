data79 = read.table("light1879.txt", header=F)
data82 = read.table("light1882.txt", header=F)

#2.1
data79 = as.matrix(data79)
hist(data79)
data82 = as.matrix(data82)
hist(data82)

#2.2
error = qnorm(0.975)*sd(data79)/sqrt(length(data79))
left = mean(data79)-error
right = mean(data79)+error
c(left, right)

#2.3
error = qnorm(0.975)*sd(data82)/sqrt(length(data82))
left = mean(data82)-error
right = mean(data82)+error
c(left, right)

#2.4
error = qnorm(0.975)*sd(data79)/sqrt(length(data79))
left = median(data79)-error
right = median(data79)+error
c(left, right)

#2.5
error = qnorm(0.975)*sd(data82)/sqrt(length(data82))
left = median(data82)-error
right = median(data82)+error
c(left, right)