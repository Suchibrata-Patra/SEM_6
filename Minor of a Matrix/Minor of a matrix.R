rm(list=ls())
minor_matrix = function(matrix_input, row_pos, col_pos) {
  minor_matrix = matrix_input[-row_pos, -col_pos, drop = FALSE] 
  return(minor_matrix)
 }
input = matrix(1:9, nrow = 3, ncol = 3)
row_pos = 1
col_pos = 10
x = minor_matrix(input, row_pos, col_pos)
x