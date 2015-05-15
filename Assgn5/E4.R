library('corrplot')
data = read.table("expensescrime.txt", header=T)

attach(data)
pairs(data)
dataframe = data.frame(bad,crime,lawyers,employ,pop,expend)
cor(dataframe)
corrplot(cor(dataframe), method="shade", type="lower")

summary(lm(expend~pop, data=data))
summary(lm(expend~employ, data=data))
summary(lm(expend~lawyers, data=data))
summary(lm(expend~crime, data=data))
summary(lm(expend~bad, data=data))

summary(lm(expend~employ+pop, data=data))
summary(lm(expend~employ+lawyers, data=data))
summary(lm(expend~employ+crime, data=data))
summary(lm(expend~employ+bad, data=data))

summary(lm(expend~employ+lawyers+pop, data=data))
summary(lm(expend~employ+lawyers+crime, data=data))
summary(lm(expend~employ+lawyers+bad, data=data))
