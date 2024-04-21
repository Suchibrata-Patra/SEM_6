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
dbinom(10,20,p1)
x1=8;x2=4;x3=2;x4=1
x=20-x2-x3-x4
dbinom(8,x,p1.star)

#Part - 05
ro1.234= sqrt(1-((1-p1.star)/(1-p1)));ro1.234
ro1.234^2




#====================
#  Question No - 02
#===================
rm(list=ls())
f  = seq(1,6,1)
p = c(0.10,0.30,0.25,0.20,0.09,0.06)

#Part - 01
roh_25 = sqrt( p[2]*p[5]/((1-p[2])*(1-p[5])))
roh_25
roh_25.13 = sqrt( p[2]*p[5]/((1-p[2]-p[3]-p[1])*(1-p[5]-p[1]-p[3])))
roh_25.13








