#Question No - 01
rm(list=ls())
data = c(9.3,8.8,10.7,11.5,8.2,9.7,10.3,8.6,11.3,10.7,11.2,9.0,
         9.8,9.3,9.9,10.3,10.0,10.1,9.6,10.4)
z = ifelse(data>9.9,1,0)
k = sum(z) ; k
T_obs = (k-10)/sqrt(5) ; T_obs
tau = qnorm(0.975) ; tau
binom.test(k,)


rm(list=ls())
length=c(9.3,8.8,10.7,11.5,8.2,9.7,10.3,8.6,11.3,10.7,11.2,9,9.8,9.3,9.9,10.3,10,10.1,9.6,10.4)
z=ifelse(length>9.9,1,0);z
k=sum(z);k

T= (k-10)/sqrt(5);T
tao= qnorm(0.975);tao

binom.test(k,20,1/2)		

#Question No - 04
rm(list=ls())
data = chickwts
sample_size = 50
names(data)
s = sample(1:72,sample_size) ; s

