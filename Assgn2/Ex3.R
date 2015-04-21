library(BSDA)

#3.1
klm = scan(file = "klm.txt")

par(mfrow=c(1,2))
hist(klm)
boxplot(klm)

SIGN.test(klm, md=35, conf.level = 0.995)

#3.2
sum(klm)
sum(klm > 70)
binom.test(14,3464,conf.level = 0.995)
