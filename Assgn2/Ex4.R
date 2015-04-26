library(BSDA)

run = read.table(file = "run.txt")

#4.1
before = as.numeric(run[,1])
after = as.numeric(run[,2])
par(mfrow=c(2,2))
hist(before)
boxplot(before)
hist(after)
boxplot(after)

#4.2
lemo = run[which(run[,3]=="lemo"), 1:2]
energy = run[which(run[,3]=="energy"), 1:2]
lemobefore = as.numeric(lemo[,1])
energybefore = as.numeric(energy[,1])
lemoafter = as.numeric(lemo[,2])
energyafter = as.numeric(energy[,2])
par(mfrow=c(2,2))
hist(lemobefore)
hist(lemoafter)
hist(energybefore)
hist(energyafter)

#4.3
lemo = run[which(run[,3]=="lemo"), 1:2]
energy = run[which(run[,3]=="energy"), 1:2]
deltaLemo = as.numeric(lemo[,2]) - as.numeric(lemo[,1])
deltaEnergy = as.numeric(energy[,2]) - as.numeric(energy[,1])
par(mfrow=c(1,2))
plot(deltaLemo)
lines(deltaLemo)
plot(deltaEnergy)
lines(deltaEnergy)
