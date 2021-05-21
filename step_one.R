#########################################
# this file is to simulate step one in the algorithm
# create a new matrix (N x (n-1) x (p-1)) x 9
#########################################
rm(list = ls())
source("/Users/rachaelshudde/Desktop/Image/slidingAlgorithm.R")
library(mclust)
rotate <- function(x) t(apply(x, 2, rev)) # necessary for proper imaging

N = 72
G = 15
  
### MNIST data
MNIST = read.csv("//Users//rachaelshudde//Desktop//Data//MNIST//mnist_test.csv", header = F)
test = MNIST[(MNIST$V1 == 1 | MNIST$V1 == 7), ]
Y = test[, 1]

data = test[sample(1:nrow(test), N, replace = FALSE), -1]

# thresh = 128
# G_VALUE = 10
# 
# M2 = data
# M2[data < thresh] = 0
# M2[data >= thresh] = 1
# 
# data = M2
# 


# set up some variables
n_columns = 28
n_rows = 28
size = 3 # size of small box is 3x3
buffer_sides = 1
numbers = 1:784
matrix_numbers = matrix(numbers, nrow = n_columns, ncol = n_rows, byrow = T)

new_matrix = matrix(NA, nrow = (n_columns - 2)*(n_rows - 2), ncol = size * size)
count = 1

for (i in 2:(n_columns - 1)) # loop over the rows
{
  for (j in 2:(n_columns- 1)) # loop over columns
  {
    nine_row = c()
    
    # get the corresponding rows before and after
    for (k in c(1, 0, -1))
    {
      temp = c(matrix_numbers[i-k, (j-buffer_sides):(j+buffer_sides)])

      nine_row = c(nine_row, temp)
    }
    
    # nine_row = c(temp_before, temp, temp_after)
    new_matrix[count, ] = nine_row
    count = count + 1
  }
}

###########################################################################################################################

step_one = matrix(NA, nrow = N*(n_columns - 2)*(n_rows - 2), ncol = size * size)
count = 1
# now, new_matrix has the orders of the data that we want 
for(i in 1:N) # loop over all the images
{
  for (j in 1:nrow(new_matrix))
  {
    step_one[count, ] = as.vector(unlist(data[i, new_matrix[j,]]))
    count = count + 1
  }
}

###########################################################################################################################

Z = step_one
p_temp = ncol(Z)
n_temp = nrow(Z)

# RJ steps
gg = tcrossprod(Z)/p_temp
gg_wodiag = gg - diag(diag(gg))
gg_wdiag = cbind(gg_wodiag, diag(gg))
GG_new = cbind(gg_wodiag + diag(colSums(gg_wodiag)/(n_temp-1)), diag(gg))

temp = Mclust(GG_new, model.names = "VVI", G = G, verbose = FALSE)
# clust_no_specification = Mclust(GG_new, G = 50, verbose = FALSE)


###########################################################################################################################
# imaging things
par(mfrow = c(3,5))
for (i in 1:G)
{
  group_i = which(temp$classification == i)
  group_i_average = colMeans(step_one[group_i,])
  group_i_matrix = matrix(group_i_average, ncol = 3, byrow = TRUE)
  
  # thresh = 128
  # 
  # M2 = group_i_matrix
  # M2[group_i_matrix < thresh] = 0
  # M2[group_i_matrix >= thresh] = 1
  # 
  # group_i_matrix = M2

  print(group_i_matrix)
  image(rotate(group_i_matrix), col= gray((0:256)/256), axes = FALSE, main = paste("Feature: ", i))
  Sys.sleep(1)
}

###########################################################################################################################
# throw out 0 groups and try again

# to_remove = c(which(temp$classification == 1), which(temp$classification == 5))
# 
# new_data = step_one[-to_remove, ]
# 
# Z = new_data
# p_temp = ncol(Z)
# n_temp = nrow(Z)
# 
# # RJ steps
# gg = tcrossprod(Z)/p_temp
# gg_wodiag = gg - diag(diag(gg))
# gg_wdiag = cbind(gg_wodiag, diag(gg))
# GG_new = cbind(gg_wodiag + diag(colSums(gg_wodiag)/(n_temp-1)), diag(gg))
# 
# temp1 = Mclust(GG_new, model.names = "VVI", G = 5, verbose = FALSE)
# # clust_no_specification = Mclust(GG_new, G = 50, verbose = FALSE)
# 
# 
# ###########################################################################################################################
# # imaging things
# 
# 
# par(mfrow = c(3,4))
# for (i in 1:10)
# {
#   group_i = which(temp1$classification == i)
#   group_i_average = colMeans(new_data[group_i,])
#   group_i_matrix = matrix(group_i_average, ncol = 3, byrow = TRUE)
#   
#   thresh = 128
#   
#   M2 = group_i_matrix
#   M2[group_i_matrix < thresh] = 0
#   M2[group_i_matrix >= thresh] = 1
#   
#   group_i_matrix = M2
#   
#   print(group_i_matrix)
#   image(rotate(group_i_matrix), col= gray((0:256)/256), axes = FALSE, main = paste("Feature: ", i))
#   Sys.sleep(1)
# }
