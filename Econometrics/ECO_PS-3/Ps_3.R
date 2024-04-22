#==================================================#
#                Question No - 01                  #
#==================================================#
rm(list=ls())
wages = c(8.4, 8.4, 8.6, 8.7, 8.9, 9.0,8.9, 9.1, 9.3, 9.3, 9.4, 9.6,9.5, 9.8, 9.9, 10.3, 10.3, 10.5,10.3, 10.6, 10.9, 11.3, 11.5, 11.7,11.6, 11.8, 12.1, 12.5, 12.7, 13.1)
y = wages
workers = rep(seq(100,500,100),each=6,by=6)
x = workers
model = lm(wages~workers)
summary(model)
plot(wages,workers)
residuals = residuals(model)
plot(residuals,fitted(model))
abline(h=0,col="RED")

ui = abs(resid(model))
x1 = 1/x
x2 = 1/sqrt(x)
x3 = sqrt(x)
x4 = x
model1 = summary(lm(ui~x1)) ; model1
model2 = summary(lm(ui~x2)) ; model2
model3 = summary(lm(ui~x3)) ; model3
model1 = summary(lm(ui~x4)) ; model1

model.1  = summary(lm(y[1:10]~x[1:10])) ; model.1
rss1 = 0.23
model.2  = summary(lm(y[21:30]~x[21:30])) ; model.2
rss2 = 0.4962
F = rss2/rss1 ; F
qf(0.95,8,8)

Y = y/x
X = 1/x
m = summary(lm(Y~X)) ; m

