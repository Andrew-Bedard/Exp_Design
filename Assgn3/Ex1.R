#1.1
peru = read.table("peruvians.txt", header=T)
peru = peru[,-c(5,6,7)]
pairs(peru)

#This shows the pearson rank correlation against all variables
#cor(peru)
#This shows the spearman rank correlation against all variables
#cor(peru, method="spearman")

#To me age was the only variable that seemed to correlate, so 
#it is the only one tested here in both of these
cor(peru[1:2])
cor(peru[1:2], method="spearman")
