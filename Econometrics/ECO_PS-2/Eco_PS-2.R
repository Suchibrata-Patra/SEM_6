#Question NO - 01
rm(list=ls())
data  = read.csv("/Users/suchibratapatra/Desktop/Practical/Econometrics/ECO_PS-2/Ps-2_Qno_1.csv")
attach(data)
model = glm(GNP~Money_Supply)
summary(model)
detach(data)


#Question NO - 02
rm(list=ls())
data = read.csv("/Users/suchibratapatra/Desktop/Practical/Econometrics/ECO_PS-2/Ps-2_Qno_2.csv")
attach(data)
Z = 100/(100-Y)
W = (1/X)
model = glm(Z~W)
summary(model)
detach(data)


#Question NO - 03
rm(list=ls())
x=c(172.8,166.0,199.9,164.4,168.8,165.2,170.0,163.5,169.4,167.8,160.3,164.4,161.2,164.0,159.1)
y = c(100.9,83.6,81.3,85.4,83.9,81.1,84.9,81.1,84.9,82.7,78.1,85.4,81.7,80.9,79.6)
model = lm(y~x)
summary(model)
plot(x,y,col="RED")






