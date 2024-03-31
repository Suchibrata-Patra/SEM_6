# ===============
# Block no - 01
# ===============

# Part - 1
rm(list=ls())
names = c("Blitox","Dithane","Breastan","Control")
data = c(678,703,736,556,510,689,574,510,531,611,594.66,500)
treatment = rep(c(1,2,3,4),3)
block = c(1,1,1,1,2,2,2,2,3,3,3,3)
model = aov(data~as.factor(treatment)+as.factor(block))
summary(model)
SSE_x_hat_hat = 10286 ; SSE_x_hat_hat
x_hat_hat = 1642/3 ; x_hat_hat
data_HO = c(678,703,736,556,510,689,574,510,531,611,x_hat_hat,500)
model_HO = aov(data_HO~as.factor(treatment)+as.factor(block))
summary(model_HO)
qf(0.95,3,5)

# Part - 2
SSE = 10286
SSE_HO = 35022+11406
v = 4 ; b=3
F_obs = ((SSE_HO - SSE)/(v-1))/(SSE/((v-1)*(b-1)-1)) ; F_obs
qf(0.95,3,5)
if(F_obs>qf(0.95,3,5)){
  print("Reject")
  }else{
    print("Accept")
  }
sum(data)

# Part - 3
y01_bar = (678+510+531)/3 ; y01_bar
y03_bar = (736+574+594.66)/3 ; y03_bar
MSE = 10286/5
E_hat = (y01_bar - y03_bar) ; E_hat
V_hat = MSE*((2/b)+(v/(b*(b-1)*(v-1)))) ; V_hat
t = E_hat/sqrt(V_hat) ; t

