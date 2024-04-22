#
rm(list=ls())
data = read.csv("C:\\Users\\STUDENT\\Desktop\\dataset_4.csv")
attach(data)
par(mfrow=c(2,2))
plot(y,Year,type="l",main="Yt against t")
residuals = rstandard(lm(y~x))
plot(x,residuals,main="Standarised Residual vs t",type="l")
detach(data)

dulbin = 0.173888
roh = 1 - (dulbin/2)
x = data$x
y.star = y[-1] - roh*y[-46]
x.star = x[-1] - roh*x[1:45]
model = lm(y.star~x.star)
model ; roh