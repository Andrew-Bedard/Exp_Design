library('corrplot')
data = read.table("expensescrime.txt", header=T)

attach(data)
pairs(data)
dataframe = data.frame(bad,crime,lawyers,employ,pop,expend)
cor(dataframe)
corrplot(cor(dataframe), method="shade", type="lower")

summary(lm(expend~pop, data=data))$r.squared
summary(lm(expend~employ, data=data))$r.squared
summary(lm(expend~lawyers, data=data))$r.squared
summary(lm(expend~crime, data=data))$r.squared
summary(lm(expend~bad, data=data))$r.squared

summary(lm(expend~employ+pop, data=data))$r.squared
summary(lm(expend~employ+lawyers, data=data))$r.squared
summary(lm(expend~employ+crime, data=data))$r.squared
summary(lm(expend~employ+bad, data=data))$r.squared

summary(lm(expend~employ+lawyers+pop, data=data))$r.squared
summary(lm(expend~employ+lawyers+crime, data=data))$r.squared
summary(lm(expend~employ+lawyers+bad, data=data))$r.squared
