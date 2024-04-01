# ==================
# Question no - 01
# ==================
rm(list=ls())
virus_growth = c(21,22,23,28,20,26,25,26,24,25,29,27,37,39,38,38,35,36,31,34,29,33,30,35)
time= rep(c("12","18"),each=12)
culture_medium = rep(rep(c("A","B"),each=6),2)
data = data.frame(culture_medium, time, virus_growth)
data$culture_medium = as.factor(data$culture_medium)
data$time = as.factor(data$time)
anova_results = aov(virus_growth ~time*culture_medium, data=data)
summary(anova_results)
qf(0.95,1,20)
data

# ==================
# Question no - 02
# ==================
rm(list=ls())
data = c(90,93,98,74,78,85,81,85,88,83,80,84,77,78,82,81,80,85,88,82,88,73,70,79,98,95,100,72,76,82,87,83,91,85,86,86,99,90,98,79,75,81,87,84,86,80,80,85)
Block = factor(rep(1:3,16))
A = factor(rep(rep(c("1","a"),each=3),8))
B = factor(rep(rep(c("1","b"),each=6),4))
C = factor(rep(rep(c("1","c"),each=12),2))
D = factor(rep(rep(c("1","d"),each=24),1))

Chemical.process = data.frame(data,Block,A,B,C,D)
View(Chemical.process)
Chem.Pro.aov = aov(data ~ Block+A*B*C*D,data=Chemical.process)
summary(Chem.Pro.aov)
names(Chem.Pro.aov)
qf(0.95,1,30)
just checking

