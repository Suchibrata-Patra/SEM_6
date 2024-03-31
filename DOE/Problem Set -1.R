# ===============
# Block no - 01
# ===============
rm(list=ls())
set.seed(123)
random_no = sample(seq(1,18,1),18,replace=F) ; random_no
data = c(rep("A",4),rep("B",5),rep("C",3),rep("D",2),rep("E",4))
treatment = data[random_no]
Design_Matrix = matrix(treatment,nrow=3,byrow=TRUE)
Design_Matrix

# ===============
# Block No - 02
# ===============
rm(list=ls())
set.seed(123)
Design_matrix = matrix(nrow=4, ncol=5)
treatment = c("A","B","C","D","E")
for(i in 1:4){
  Design_matrix[i,] = sample(treatment, 5, replace=FALSE)
}
print(Design_matrix)

#Alternative Code for the Above 
rm(list=ls())
set.seed(123)
block_1 = sample(c("A","B","C","D","E"),5)
block_2 = sample(c("A","B","C","D","E"),5)
block_3 = sample(c("A","B","C","D","E"),5)
block_4 = sample(c("A","B","C","D","E"),5)
data = c(block_1,block_2,block_3,block_4)
Design_matrix = matrix(data,nrow=4,byrow=T)
Design_matrix

# ===============
# Question No - 03
# ===============
rm(list=ls())
create_LSD = function(n, treatments) {
  LSD = matrix(NA, nrow = n, ncol = n)
    for (i in 1:n) {
    for (j in 1:n) {
      LSD[i, j] = treatments[(i + j - 1) %% n + 1]
    }
  }
    return(LSD)
}
# Part- 1 [4x$ Order Matrix]
treatments = c("A", "B", "C", "D")
treatments = sample(treatments,length(treatments),replace=F)
M_4 = create_LSD(4, treatments)
M_4
# Part- 2 [5x5 Order Matrix]
treatments = c("A", "B", "C", "D","E")
treatments = sample(treatments,length(treatments),replace=F)
M_5 = create_LSD(5, treatments)
M_5
# Part- 3 [5x5 Order Matrix]
treatments = c("A", "B", "C", "D","E","F")
treatments = sample(treatments,length(treatments),replace=F)
M_6 = create_LSD(6, treatments)
M_6



# ===============
# Question No - 04
# ===============
rm(list=ls())
time = c(19,22,20,20,29,24,30,24,26,25,16,22,28,25,31,28,27,16,27,20)
drill = c("D1","D3","D4","D1","D5","D2","D5","D3","D2","D4","D1","D2","D5","D4","D5","D4","D4","D2","D2","D3")
data = data.frame(time,drill)
#Test for Treatment Effect
anova_table= summary(aov(time~as.factor(drill)));anova_table
summary(anova_table)
qf(0.95,4,15)

#test for pair significance
yi0.bar=aggregate(time~as.factor(drill),data=data,mean)$time;yi0.bar
n = aggregate(time~as.factor(drill),data=data,length)$time;n
t_obs = array(0);k=1;len=5
t_1 = array(0)
t_2 = array(0)
for(i in 1:(len-1))
  {
    for(j in (i+1):len)
      {
  t_obs[k] = (yi0.bar[i]-yi0.bar[j])/sqrt(9.04*((1/n[i])+(1/n[j])))
  t_1[k] = i 
  t_2[k] = j
  k=k+1
    }
  }
t_obs
qt(.975,15)
decision=ifelse(abs(t_obs)>qt(0.975,15),"Reject","Accept")
data.frame(t_1,t_2,T,decision)


# =================
# Question No - 05
# =================
rm(list=ls())
data = data.frame(
  Make = rep(c("A", "B", "C", "D"), each = 5),
  Speed = rep(c(25, 35, 50, 60, 70), times = 4),
  MPG = c(20.6, 19.5, 18.1, 17.9, 16.0,
          19.5, 19.0, 15.6, 16.7, 14.1,
          20.5, 18.5, 16.3, 15.2, 13.7,
          16.2, 16.5, 15.7, 14.8, 12.7)
)
anova_result = aov(MPG ~ as.factor(Speed) + as.factor(Make), data = data)
summary(anova_result)

qf(0.95, 3, 12)
mean_MPG = aggregate(MPG ~ Make, data = data, mean)$MPG
mean_MPG

len= 4
k = 1
t_obs = array(0, dim = choose(len, 2))
for (i in 1:(len-1)) {
  for (j in (i+1):len) {
    t_obs[k] = (mean_MPG[i] - mean_MPG[j]) / sqrt(2 * 0.618 / 5)
    k = k + 1
  }
}
qt(.975, 12)
decision = ifelse(abs(t_obs) > qt(0.975, 12), "Reject", "Accept")
data.frame(t_obs, decision)




# =================
# Question No - 06
# =================
rm(list=ls())
OrchardSprays
attach(OrchardSprays)
rowpos = as.factor(rowpos)
colpos = as.factor(colpos)
treatment = as.factor(treatment)
anova_table= aov(decrease~ rowpos+colpos+treatment);anova_table
summary(anova_table)
qf(.95,7,42)
yio.bar=aggregate(decrease~as.factor(treatment),data=OrchardSprays,mean)$decrease;yio.bar
len=8
k=1
t_obs=array(0)
for(i in 1:(len-1))
  {
  for(j in (i+1):len)
    {
    t_obs[k]=(yio.bar[i]-yio.bar[j])/sqrt(2*381/8)
  k=k+1
    }
  }
t_obs
qt(.975,42)
decision = ifelse(abs(t_obs)>qt(0.975,42),"Reject","Accept")
data.frame(t_obs,decision)
detach(OrchardSprays)

