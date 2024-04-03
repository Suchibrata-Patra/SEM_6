#====================
#  Question No - 01
#====================
# Part - 01
rm(list=ls())
x = c(40,20,12,13) ; m = 100
p = x/m
p_star = p[1]/(1-sum(p[2:4])) ; p_star
estimate = (m- sum(18+9+7))*p_star
estimate

# Part - 02
roh_12_estimate = -sqrt((p[1]*p[2])/((1-p[1])*(1-p[2])))
roh_12_estimate

# Part - 03
p3 = p4 = 10/100
roh_12.34= -sqrt((p[1]*p[2])/((1-p[2]-p[3]-p[4])*(1-p[1]-p[3]-p[4]))) ; roh_12.34

# Part - 04











# Rough Work for Code
roh_1.234 = sqrt(1- ((1-p[1]*p[1])/(1-p[1])))
roh_1.234

x = c(8,4,2,1)
m = 20 ; p = x/m
n = m - sum(x[-1]) ; n
p_star = p[1]/(1-sum(p[-1])) ; p_star
dbinom(8,15,p_star)


##### Code by 
# part 1
x1=40;x2=20;x3=12;x4=13
p1=x1/100;p2=x2/100;p3=x3/100;p4=x4/100
p1.star=p1/(1-(p2+p3+p4));p1.star
x2=18;x3=9;x4=7
expected=(100-(x2+x3+x4))*p1.star;expected
