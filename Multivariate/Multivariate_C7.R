rm(list=ls())
S.inv=matrix(c(2,1,-1,1,3,-2,-1,-2,4),nrow=3,byrow=T);S.inv
S=matrix(c(0.615,-0.1538,0.0769,-0.1538,0.5384,0.2307,0.0769,0.2307,0.3846),byrow=T,nrow=3)
S=solve(S.inv);S
R=cov2cor(S)
R11=det(R[-1,-1])
rho=sqrt(1-det(R)/R11);rho
rho^2
S11=S[-2,-2];S11
S21=matrix(S[-2,-c(1,3)],ncol=2);S21
S12=matrix(S[-c(1,3),-2],nrow=2);S12
S22=matrix(S[2,2],nrow=1);S22
mu.1=c(1,0)
mu.2=0

S12%*%solve(S22)
SS=S11-S12%*%solve(S22)%*%S21;SS

x2=1
-det(R[-1,-3])/sqrt(det(R[-1,-1])*det(R[-3,-3]))

mu.c=c(1,0)+S12%*%solve(S22)*(x2-mu.2);mu.c
mu.c[1]-mu.c[2]

SS[1,1]+SS[2,2]-2*SS[1,2]

1-pnorm(0,0.286,sqrt(0.57))

rho12.3=SS[1,2]/sqrt(SS[1,1]*SS[2,2]);rho12.3



#Regression Equation of X1 on X2,X3
S.11=S[1,1];S.11
S.12=matrix(c(S[1,2],S[1,3]),nrow=1);S.12
S.21=matrix(c(S[2,1],S[3,1]),nrow=2);S.21
S.22=S[-1,-1];S.22
S.12%*%solve(S.22)
S.11-S.12%*%solve(S.22)%*%S.21
