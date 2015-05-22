data = read.table("psi.txt", header=T)
attach(data)

pairs(data)

gpalm = lm(gpa~psi)
plot(residuals(gpalm), psi)
qqnorm(residuals(gpalm))