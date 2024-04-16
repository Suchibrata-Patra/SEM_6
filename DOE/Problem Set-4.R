##PROBLEM SET 4

#QUESTION 1

rm(list=ls())
data=c(22,32,35,55,44,40,60,39,31,43,34,47,45,37,50,41,25,29,50,46,38,36,54,47)
blocks=as.factor(c(1,2,2,1,2,1,1,2,3,4,4,3,3,4,4,3,5,5,6,6,6,6,5,5))
A=rep(0:1,times=12)
B=rep(0:1,each=2,times=6)
C=rep(0:1,each=4,times=3)
anova.fit=aov(data~A*B*C+blocks)
summary(anova.fit)

data.frame(data,blocks)




#QUESTION 2

rm(list=ls())
data=c(142,106,148,101,185,113,200,130,129,88,146,140,200,166,215,145,114,106,108,114,162,88,164,83,109,98,195,72,79,172,118,110)
sum(data)
blocks=as.factor(c(1,4,3,2,3,2,1,4,2,3,4,1,4,1,2,3,2,3,4,1,4,1,2,3,1,4,3,2,3,2,1,4))
sum(ifelse(blocks==3,1,0))
A=rep(0:1,each=16,times=1)
B=rep(0:1,each=8,times=2)
C=rep(0:1,each=4,times=4)
D=rep(0:1,each=2,times=8)
E=rep(0:1,each=1,times=16)
anova.fit=aov(data~blocks+A*B*C*D*E)
summary(anova.fit)

anova.fit2=aov(data~blocks+A*B+A*C+A*D+A*E+B*C+B*D+B*E+C*D+C*E+D*E)
summary(anova.fit2)

#Alternative
anova.fit2=aov(data~blocks+(A+B+C+D+E)^2)
summary(anova.fit2)

#Alternative for 4,5-factor not needed
anova.fit2=aov(data~blocks+(A+B+C+D+E)^3)
summary(anova.fit2)
