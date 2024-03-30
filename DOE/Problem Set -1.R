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
treatments <- c("A", "B", "C", "D","E")
treatments = sample(treatments,length(treatments),replace=F)
M_5 = create_LSD(5, treatments)
M_5
# Part- 3 [5x5 Order Matrix]
treatments <- c("A", "B", "C", "D","E","F")
treatments = sample(treatments,length(treatments),replace=F)
M_6 = create_LSD(6, treatments)
M_6



# ===============
# Question No - 04
# ===============

#Question No - 04
Y = c(19,22,20,20,29,24,30,24,26,25,16,22,28,25,31,28,27,16,27,20)
X = c("D1","D3","D4","D1","D5","D2","D5","D3","D2","D4","D1","D2","D5","D4","D5","D4","D4","D2","D2","D3")
data = data.frame(X,Y)
oneway = aov(Y~as.factor(X),data=data)
summary(oneway)



                     
t = numeric(20)
k = 1
for(i in 1:5){
 for(j in 1:4){
     t[k] = (mean(y)-mean())

   } 
 }




#Question No - 05
data = c(20.6,19.5,18.1,17.9,16,19.5,19,15.6,16.7,14.1,20.5,18.5,16.3,15.2,13.7,16.2,16.5,15.7,14.8,12.7)
length(data)
factors = c(rep(c(25,35,50,60,70),4)) ;factors 
letwo_way  = aov(M~factors)
summary(two_way)



for (seed_value in 1:1000) {  # Adjust the range as needed
  rm(list=ls())  # Remove all variable names before each iteration
  desired_output <- c(1, 7, 11, 17, 3, 4, 12, 15, 16, 9, 13, 14, 8, 18, 2, 5, 6, 10)
  set.seed(seed_value)
  random_sample <- sample(1:20, 18, replace = FALSE)  # Generate random sample of 18 numbers
  
  if (all(sort(random_sample) == desired_output)) {
    cat("Seed value:", seed_value, "\n")
    break
  }
}



                                                                                                   