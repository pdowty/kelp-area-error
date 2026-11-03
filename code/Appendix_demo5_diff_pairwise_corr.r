###############################################################################
#  Appendix demo 5 
#
#  Effect of differences among pairwise correlations.
#  Goal is to produce to graphical outputs:
#   1) a bar graph: y:delta-stdev  x:discrete pairwise scenarios
#   2) facets of point or bar graphs:  y:pairwise correlation, x=6 pairs,
#      facets are the six discrete pairwise scenarios
#
#  November 2026
#
###############################################################################


library(tibble)
library(dplyr)
library(MASS)


############################################################################
#  set fixed parameters 
############################################################################
A <- 10   # kelp area, hectares
k <- 0.1
N <- 10000

 

############################################################################
# create correlation matrices for the six scenarios of pairwise correlation
############################################################################

corr_scenarios <- list()
init_matrix <- matrix(0.0, nrow=4, ncol=4)
diag(init_matrix) <- 1

### one pair correlated
hi_corr_value <- 0.3
corr_matrix <- init_matrix
corr_matrix[1,2] <- hi_corr_value
corr_scenarios[[1]] <- corr_matrix

# test eigen values of correlation matrix to ensure will work with mvrnorm
eigen_vals <- eigen(corr_matrix)$values
if (any(eigen_vals < 0)) {
  print("negative eigen value")
}


### two pair correlated
hi_corr_value <- 0.3
corr_matrix <- init_matrix
corr_matrix[1,2] <- hi_corr_value
corr_matrix[1,3] <- hi_corr_value
corr_scenarios[[2]] <- corr_matrix

### three pair correlated
hi_corr_value <- 0.3
corr_matrix <- init_matrix
corr_matrix[1,2] <- hi_corr_value
corr_matrix[1,3] <- hi_corr_value
corr_matrix[1,4] <- hi_corr_value
corr_scenarios[[3]] <- corr_matrix

### four pair correlated
hi_corr_value <- 0.3
corr_matrix <- init_matrix
corr_matrix[1,2] <- hi_corr_value
corr_matrix[1,3] <- hi_corr_value
corr_matrix[1,4] <- hi_corr_value
corr_matrix[2,3] <- hi_corr_value
corr_scenarios[[4]] <- corr_matrix

### five pair correlated
hi_corr_value <- 0.3
corr_matrix <- init_matrix
corr_matrix[1,2] <- hi_corr_value
corr_matrix[1,3] <- hi_corr_value
corr_matrix[1,4] <- hi_corr_value
corr_matrix[2,3] <- hi_corr_value
corr_matrix[2,4] <- hi_corr_value
corr_scenarios[[5]] <- corr_matrix

### six pair correlated
hi_corr_value <- 0.3
corr_matrix <- init_matrix
corr_matrix[1,2] <- hi_corr_value
corr_matrix[1,3] <- hi_corr_value
corr_matrix[1,4] <- hi_corr_value
corr_matrix[2,3] <- hi_corr_value
corr_matrix[2,4] <- hi_corr_value
corr_matrix[3,4] <- hi_corr_value
corr_scenarios[[6]] <- corr_matrix


  
############################################################################
#  create kelp area scenarios, A1 and A2
############################################################################
   








