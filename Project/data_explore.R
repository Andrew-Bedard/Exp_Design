data = read.table('calcium.dat.txt', na.strings="_", header=TRUE)

pairs(data)

#from pairs you can already see, lab 3 messed up the data
#they were probably using the wrong units

#we can also see there are a few lab numbers that are too
#large to make sense

#few errors in sex labels

#few errors in age


#Getting rid of nonsense values, like age greater than
#110, dont ask me how this works!!! I DONT KNOWWW!!!
data <- data.frame(data)

data[!is.na(data$AGE) & !(data$AGE <= 110),]$AGE<-NA

data[!is.na(data$SEX) & !(data$SEX == 1|data$SEX ==2),]$SEX<-NA

data[!is.na(data$LAB) & !(data$LAB < 6),]$LAB<-NA

#replacing the messed up values of lab 3 with the values divided by 10
#seems reasonable
data$CAMMOL[!is.na(data$CAMMOL) & (data$CAMMOL > 10)] <- (data$CAMMOL[!is.na(data$CAMMOL) & (data$CAMMOL > 10)])/10

#new pairs with some crazy values removed

pairs(data)

#still some outliers that need to be dealt with
#but its looking much better