#==================================================#
#                Question No - 01                  #
#==================================================#
rm(list=ls())
wages = c(8.4, 8.4, 8.6, 8.7, 8.9, 9.0,8.9, 9.1, 9.3, 9.3, 9.4, 9.6,9.5, 9.8, 9.9, 10.3, 10.3, 10.5,10.3, 10.6, 10.9, 11.3, 11.5, 11.7,11.6, 11.8, 12.1, 12.5, 12.7, 13.1)
workers = rep(seq(100,500,100),each=6,by=6)
model = lm(wages~workers)
summary(model)
plot(wages,workers)
residuals = residuals(model)
plot(residuals)
abline(h=0,col="RED")
