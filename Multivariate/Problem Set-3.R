#======================#
# Question No - 01
#======================#
rm(list=ls())
data = c(9.3,8.8,10.7,11.5,8.2,9.7,10.3,8.6,11.3,10.7,11.2,9.0,
         9.8,9.3,9.9,10.3,10.0,10.1,9.6,10.4)
z = ifelse(data>9.9,1,0)
k = sum(z) ; k
T_obs = (k-10)/sqrt(5) ; T_obs
tau = qnorm(0.975) ; tau
binom.test(k,20,1/2)


#======================#
# Question No - 02
#======================#
rm(list=ls())
before=c(10,16,7,4,7,2);after=c(18,19,11,3,5,3)
d= before-after
z=ifelse(d>0,1,0);z
k=sum(z);k

T= (k-3)/sqrt(6/4);T
tao= qnorm(0.975);tao
binom.test(k,6,1/2)	

#======================#
# Question No - 03
#======================#
rm(list=ls())
x=c(12.46,12.43,12.77,11.89,12.12,12.33,12.76,11.85,12.56,11.95,12.24,12.65,12.78,12.28,12.13)
y=c(12.05,12.38,12.22,12.45,12.44,12.39,11.97,12.37,12.21,12.64)
n1=length(x);n2=length(y);n=n1+n1
comb=sort(c(x,y));comb
W=sum(match(y,comb));W

#======================#
# Question No - 04
#======================#
rm(list=ls())
attach(chickwts);chickwts
rank=match(weight,sort(weight));rank
rio.bar=aggregate(rank~as.factor(feed),data=chickwts,mean)$rank;rio.bar
ni=aggregate(rank~as.factor(feed),data=chickwts,length)$rank;ni
n=sum(ni);mean=(n+1)/2;var=(n+1)*n/12
Q= sum(ni*(rio.bar-mean)^2)/var;Q

kruskal.test(weight,as.factor(feed))		#alternative



