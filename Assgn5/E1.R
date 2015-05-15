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
nausea.frame=data.frame(nausea,medicin)

#1.2
xtabs(~medicin+nausea)

#1.3
#??
chisq.test(xtabs(~medicin+nausea))[[1]]
