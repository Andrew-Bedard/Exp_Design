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
medicin <- factor(medicin)
nausea.frame=data.frame(nausea,medicin)

#1.2
xtabs(~medicin+nausea)

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

pl = sum(tstar<teststat.obs)/B
pr = sum(tstar>teststat.obs)/B