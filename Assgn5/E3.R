data = read.table("genal2.txt")

#3.1
y = c()
mut = c()
# gives X0.005, so not useable
names = names(data)
length(data[1,])
length(data[,1])
for (i in 1:length(data[1,])){
  for (j in 1:length(data[,1])){
    y = c(y, data[j,i])
    mut = c(mut, 0.005*i)
  }
}
genal2frame=data.frame(y,mut)

#3.2
boxplot(data)

#3.3
genal2frame$mut2=genal2frame$mut^2
genal2lm=lm(y~mut+mut2,data=genal2frame)
summary(genal2lm)


#3.4

func1 = function(x) (0.605358-23.78772*x+417.96154*x^2)

B=1000
ypoints = numeric(B)
xpoints = seq(0.004,0.08,length=B)

for (i in 1:B){
  ypoints[i] = func1(xpoints[i])
}


plot(ypoints, xaxt = "n",xlab="mut value",ylab="y value",pch=20, cex=0.2, col="blue")
axis(1,at=c(1,500,1000), labels=c(xpoints[1],round(xpoints[500],digits = 4),xpoints[1000]))
#Optimal mutation value correspons to xpoint where y is smallest
round(xpoints[match(min(ypoints),ypoints)],digits=4)