library(BSDA)

klm = scan(file = "klm.txt")

hist(klm)

SIGN.test(klm, md=35, conf.level = 0.995)
