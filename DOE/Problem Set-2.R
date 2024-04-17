# ==================
# Question no - 01
# ==================

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


# Part - 4
Avg_variance = (6*(MSE/b) + 3*MSE*(2/b + v/(b*(v-1)*(b-1))))/6
Avg_variance


# ==================
# Question no - 02
# ==================

# Part - 1
rm(list=ls())
v = 5
row = c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5))
col = rep(seq(1,5,1),5)
treatment = c("A", "B", "C", "D", "E", "B", "C", "D", "E", "A", "C", "D", "E", "A", "B", "D", "E", "A", "B", "C", "E", "A", "B", "C", "D")
weight = c(30, 35, 47, 50, 38, 36, 42, 48, NA, 32, 41, 52, 40, 34, 38, 54, 40, 35, 34, 45, 40, 34, 38, 42, 50)
data = data.frame(row,col,treatment,weight)
model = aov(weight~as.factor(row)+as.factor(col)+as.factor(treatment))
summary(model)
C_j = sum(na.omit(data$weight[col =="4"]))
R_j = sum(na.omit(data$weight[row =="2"]))
T_j = sum(na.omit(data$weight[treatment =="E"]))
G = sum(na.omit(weight))
x_hat = (v*(R_j+C_j+T_j) - (2*G))/((v-1)*(v-2))



# Part - 2
# Finding OUt whether it has Significant Treatment Effect or not
rm(list=ls())
v = 5
row = c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5))
col = rep(seq(1,5,1),5)
x.hat = 35.8333
treatments = c("A","B","C","D","E","B","C","D","E","A","C","D","E","A","B","D","E","A","B","C","E","A","B","C","D")
weight = c(30,35,47,50,38,36,42,48,x.hat,32,41,52,40,34,38,54,40,35,34,45,40,34,38,42,50)
model_1 = aov(weight~as.factor(row)+as.factor(col)+as.factor(treatments))
summary(model_1)
SSE_x.hat = 49.3 

x.hat.hat = 38.4375
weight_x.hat = c(30,35,47,50,38,36,42,48,x.hat.hat,32,41,52,40,34,38,54,40,35,34,45,40,34,38,42,50)
model_2 = aov(weight_x.hat~as.factor(row)+as.factor(col))
summary(model_2)

SSE_HO_x.hat.hat = 1006.0 
F = ((SSE_HO_x.hat.hat - SSE_x.hat)/(SSE_x.hat))*(11/4) ; F
qf(0.95,4,11)

MSE_x.hat = SSE_x.hat /((v-1)*(v-2)-1)
V_Ti_Tj = MSE_x.hat*((2/v)+(1/((v-1)*(v-2))))

data = data.frame(treatments,weight)
yio.bar=aggregate(weight~as.factor(treatments),data=data,mean)$weight;yio.bar
yio.bar_x.hat=aggregate(weight_x.hat~as.factor(treatments),data=data,mean)$weight;yio.bar
t_obs = array(0);k=1;len=5
t_1 = array(0)
t_2 = array(0)
for(i in 1:(len-1))
  {
    for(j in (i+1):len)
      {
  t_obs[k] = (yio.bar_x.hat[i] - yio.bar_x.hat[j])/sqrt(V_Ti_Tj)
  t_1[k] = i 
  t_2[k] = j
  k=k+1
    }
  }
t_obs
qt(.975,15)
decision=ifelse(abs(t_obs)>qt(0.975,15),"Reject","Accept")
data.frame(t_1,t_2,t_obs,decision)

