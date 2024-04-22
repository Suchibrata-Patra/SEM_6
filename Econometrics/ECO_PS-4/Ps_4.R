rm(list=ls())
data=read.csv("/Users/suchibratapatra/Desktop/Practical/Econometrics/ECO_PS-4/Ps_4.csv");data
attach(data)

model= lm(y~x);model
summary(model)

# run test 
N1= sum(ifelse(u>0,1,0));N1
N= length(u);N
N2= N-N1;N2
R=5		# total number of runs (find in minitab easily or do manually) 

mean= ((2*N1*N2)/N)+1;mean
v= (2*N1*N2*(2*N1*N2-N))/(N*N*(N-1));v	test with interval
u= mean+qnorm(0.975)*sqrt(v);u
l= mean-qnorm(0.975)*sqrt(v);l
# r lies outside 7the interval.. i.e. residuals are not random.. autocorrelation is present
T= (R-mean)/sqrt(v);T		# test with statistics
qnorm(0.975)
#test statistic rejected...same interpretation 

#  durbin watson test statistics

u= model$resid;u
ut= u[-1];ut
ut_1= u[1:45];ut_1
d= sum((ut-ut_1)^2)/sum(u^2);d

#	====== remedies =====

yt=y[-1];yt
yt_1= y[1:45];yt_1
xt= x[-1];xt
xt_1= x[1:45];xt_1

# using durbin watson value
ro.1 = 1-d/2;ro.1
y1s= y[1]*sqrt(1- ro.1^2)
ys= yt-ro.1*yt_1
y.star= c(y1s,ys)
x1s= x[1]*sqrt(1- ro.1^2)
xs= xt-ro.1*xt_1
x.star= c(x1s,xs)
summary(lm(y.star~x.star))

# using residuals
summary(lm(ut~ut_1-1))
ro= 0.88759
y1s= y[1]*sqrt(1- ro^2)
ys= yt-ro*yt_1
y.star= c(y1s,ys)
x1s= x[1]*sqrt(1- ro^2)
xs= xt-ro*xt_1
x.star= c(x1s,xs)
summary(lm(y.star~x.star))


detach(data)