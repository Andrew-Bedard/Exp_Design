nauseatable = read.table("nauseatable.txt")

#1.1
rnames = rownames(nauseatable)
nausea = c()
medicin = c()
for (j in 1:length(nauseatable[,1])){
  for (i in 1:length(nauseatable[1,])){
    nausea = c(nausea, rep(i-1, nauseatable[j,i]))
    medicin = c(medicin, rep(rnames[j], nauseatable[j,i]))
  }
}
nausea.frame=data.frame(nausea,medicin)

#1.2
xtabs(~medicin+nausea)

#1.3