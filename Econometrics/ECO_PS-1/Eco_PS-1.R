# ===========================#
# ==== Problem No - 01 ======#
# ===========================#
rm(list=ls())
data = read.csv("/Users/suchibratapatra/Desktop/Practical/Econometrics/ECO_PS-2/Ps-2_Qno_1.csv")
attach(data)
model = lm(GNP ~Money_Supply)
summary(model)
detach(data)


# ===========================#
# ==== Problem No - 02 ======#
# ===========================#
rm(list=ls())
data = read.csv("/Users/suchibratapatra/Desktop/Practical/Econometrics/ECO_PS-2/Ps-2_Qno_2.csv")
attach(data)
z = 100/(100-Y)
w = (1/X)
model = glm(z~w)
summary(model)
#plot(model)
detach(data)


# ===========================#
# ==== Problem No - 03 ======#
# ===========================#
rm(list=ls())
data = read.csv("/Users/suchibratapatra/Desktop/Practical/Econometrics/ECO_PS-1/Ps-1_Qno_3.csv")
names(data)
plot(data$length, data$weight)
summary(lm(data$weight ~ data$length))
y = -8.38492 + (0.07564*data$length)

#Finding out the uniqe values present in the length of the 
uniquevalues = unique(data$length) 

# For Sample Size = 50
n = 50 
N = 350
Nk = as.vector(table(data$length))
nk = round(n*Nk/N,0) ; sum(nk)
set.seed(1234)
sample_1 = numeric(50)
counter = 1
while (counter <= 50) {
  for (i in 1:7) {
    stratum = data[data$length == uniquevalues[i], ]
    sampled_values = sample(stratum$weight, nk[i], replace = FALSE)
    sample_1[counter:(counter+nk[i]-1)] = sampled_values
    counter = counter + nk[i]
  }
}
print(sample_1)
length_1 = rep(uniquevalues,nk)
model_1 = lm(sample_1~length_1)
summary(model_1)
y2.fitted = -8.848348+ (0.078966*length_1)


# For Sample Size = 100
n = 100 
N = 350
Nk = as.vector(table(data$length))
nk = round(n*Nk/N,0) ; nk[7] = 100 - sum(nk[1:6])
set.seed(1234)
sample_2 = numeric(100)
counter = 1
while (counter <= 100) {
  for (i in 1:7) {
    stratum = data[data$length == uniquevalues[i], ]
    sampled_values = sample(stratum$weight, nk[i], replace = FALSE)
    sample_2[counter:(counter+nk[i]-1)] = sampled_values
    counter = counter + nk[i]
  }
}
print(sample_2)
#Fitting Regression line on Second Sample
length_2 = rep(uniquevalues,nk)
model_2 = lm(sample_2~length_2)
summary(model_2)
y3.fitted = -8.533678 + (0.075865*length_2)


#Fitting the Dataset and Plotting all the three Regression Equations
plot(data$length,data$weight,main="Regression Equations",xlab="Length of Dry Jute Fiber",ylab="Weight of dry jute fiber")
lines(data$length,y,col="RED",type="l")
lines(length_1,y2.fitted,col="GREEN")
lines(length_2,y3.fitted,col="BLUE")



