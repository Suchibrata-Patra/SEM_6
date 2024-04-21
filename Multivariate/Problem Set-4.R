#===================#
# Problem No - 01   #
#===================#

rm(list=ls())
x1=c(14,14.1,13.5,13,14.5,15,16.1,17,15.5,14.6)
x2=c(18.5,17.0,19.2,20.1,15.2,16.8,18.5,19.3,20.5,22.3)
x3=c(4.5,4.1,4.2,5.0,5.5,6.0,5.2,5.9,5.1,5.5)
x4=c(4.5,5.1,4.9,5.2,5.3,5.1,4.2,4.3,5.1,5.2)
m=cbind(x1,x2,x3,x4);m
x_curl_bar=apply(m,2,mean);x_curl_bar
sigma=round(var(m),4);sigma
eig_value=round(eigen(sigma)$values,4);eig_value
eig_vectors=round(eigen(sigma)$vectors,4);eig_vectors   # take column wise

# checking orthonormality
round(eig_vectors%*%t(eig_vectors),3)

b1_curl=eig_vectors[,1];b1_curl
b2_curl=eig_vectors[,2];b2_curl
b3_curl=eig_vectors[,3];b3_curl
b4_curl=eig_vectors[,4];b4_curl

summary(prcomp(m));sqrt(eig_value)
summary(princomp(covmat=sigma));sqrt(eig_value)

