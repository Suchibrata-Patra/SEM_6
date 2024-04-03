#====================
#  Question No - 01
#====================
rm(list=ls())
no = seq(1,12,1)
x1 = c(58.6,55.5,67.1,54.7,43.2,60.1,52.7,51.7,66.5,50.4,61.1,68.0)
x2 = c(67.8,68.7,71.2,65.0,58.3,70.8,63.4,67.4,79.9,66.5,82.2,69.6)
x3 = c(71.2,67.0,73.0,77.5,75.0,78.5,71.0,69.0,87.0,72.5,85.0,70.5)
x4 = c(60.0,73.0,69.0,60.5,57.0,43.0,55.5,61.5,46.0,52.5,57.5,47.0)
data = data.frame(no,x1,x2,x3,x4)
data
model = lm(x1~x2+x3+x4) ; model

# Either 
summary(model)
# OR
estimate = predict(model)
r1.234_square = var(estimate)/var(x1) ;r1.234_square

y = 30.3303 + (1.1220-0.5346-0.1858)*50 ; y
# r12.34
e1.34 = resid(lm(x1~x3+x4))
e2.34 = resid(lm(x2~x3+x4))
r12.34 = cor(e1.34,e2.34) ;r12.34
# r13.24
e1.24 = resid(lm(x1~x2+x4))
e3.24 = resid(lm(x3~x2+x4))
r13.24 = cor(e1.24,e3.24) ;r13.24
# r14.23
e1.23 = resid(lm(x1~x2+x3))
e4.23 = resid(lm(x4~x2+x3))
r14.23 = cor(e1.23,e4.23) ;r14.23
abs(r12.34)
abs(r13.24)
abs(r14.23)


#====================
#  Question No - 02
#====================
rm(list=ls())
data = c(1,0.365,0.526,0.518,0.547,0.365,1,0.243,0.266,0.307,0.526,0.243,1,
         0.628,0.553,0.518,0.266,0.628,1,0.489,0.547,0.307,0.553,0.489,1)

Matrix = matrix(data,nrow=5) ; Matrix
minor_matrix = function(matrix_input, row_pos, col_pos) {
  minor_matrix = matrix_input[-row_pos, -col_pos, drop = FALSE] 
  return(minor_matrix)
}
#Finding Out r.1
r1.2 = 0.365
#Finding out r1.23
row_pos = c(4,5) ; col_pos = c(4,5)
R3_11 = minor_matrix(Matrix, row_pos, col_pos) ; R3_11
row_pos = c(1,1) ; col_pos = c(1,1)
R_11= minor_matrix(R3_11, row_pos, col_pos) ; R_11
r1.23 = sqrt(1-det(R3_11)/det(R_11))
r1.23

#Finding out r1.234 
row_pos = 5 ; col_pos = 5
R4_11 = minor_matrix(Matrix, row_pos, col_pos) ; R4_11
row_pos = 1 ; col_pos = 1
R_11= minor_matrix(R4_11, row_pos, col_pos) ; R_11
r1.234 = sqrt(1-det(R4_11)/det(R_11))
r1.234

#Finding out r1.2345
row_pos = 1 ; col_pos = 1
R_11= minor_matrix(Matrix, row_pos, col_pos) ; R_11
r1.2345 = sqrt(1-det(Matrix)/det(R_11))
r1.2345

# Residual Variance 
s1_sq = (9.82^2)
v_e1.2 = s1_sq*(1-r1.2^2) ; v_e1.2
v_e1.23 = s1_sq*((1-r1.23^2)) ; v_e1.23
v_e1.234 = s1_sq*((1-r1.234^2)) ; v_e1.234
v_e1.2345 = s1_sq*((1-r1.2345^2)) ; v_e1.234




