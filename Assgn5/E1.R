data = read.table("nauseatable.txt")

#1.1

rnames = rownames(data)
nausea = c()
medicin = c()
for (j in 1:length(data[,1])){
  for (i in 1:length(data[1,])){
    nausea = c(nausea, rep(i-1, data[j,i]))
    medicin = c(medicin, rep(rnames[j], data[j,i]))
  }
}
medicin <- factor(medicin)
nausea.frame=data.frame(nausea,medicin)

#1.2

#Create contingency table
cont_table = xtabs(~medicin+nausea)

#Contingency table has to be in matrix form for calculations
cont_matrix = matrix(c(100,52,32,35,48,37), byrow=TRUE, ncol=2 ,nrow=3, 
                     dimnames=list(c(rownames(data)[1],
                                     rownames(data)[2],rownames(data)[3]),c("No Nausea","Nausea")))


#chisquared test for contigency matrix
chisq.test(cont_matrix)

#1.3

teststat.obs = chisq.test(xtabs(~medicin+nausea))[[1]]

B = 1000
tstar=numeric(B)
for (i in 1:B)
{
  medstar = sample(medicin) ## permuting labels
  tstar[i] = chisq.test(xtabs(~nausea+medstar))[[1]]
}


hist(tstar)

#p-value

sum(tstar>teststat.obs)/B

#1.4

#chisquared test for contigency matrix with simulated p value
chisq.test(cont_matrix,simulate.p.value=TRUE)