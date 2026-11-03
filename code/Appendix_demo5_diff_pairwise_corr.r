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
#  set fixed parameters and initialize results df
############################################################################
A <- 10   # kelp area, hectares
k <- 0.1
N <- 10000

results_df <- data.frame(
  rho_e1_e2 = numeric(),
  rho_e1_e3 = numeric(),
  rho_e1_e4 = numeric(),
  rho_e2_e3 = numeric(),
  rho_e2_e4 = numeric(),
  rho_e3_e4 = numeric(),
  var_Ahat = numeric()
)
 

############################################################################
# create correlation matrices for the six scenarios of pairwise correlation
# and store in list corr_scenarios
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
# loop through pairwise scenarios, create df with simulations for each 
# scenario, save only scenario results 
############################################################################
for (i in seq_along(corr_scenarios)) {
  
  # use corr matrix to generate standard normal random variates as the  
  # starting point for creating the four error sources 
  sim_data <- mvrnorm(n=N, mu=rep(0,4), Sigma=corr_scenarios[[i]])
  iter_df <- as.data.frame(sim_data)
  names(iter_df) <- c("zX1","zX2","zX3","zX4")
  
  # all 4 error terms share same stdev
  error_stdev <- k*A
  
  # scale error z values to actual error values
  iter_df <- iter_df %>%
    mutate(e1 = zX1 * error_stdev,
           e2 = zX2 * error_stdev,
           e3 = zX3 * error_stdev,
           e4 = zX4 * error_stdev)
  
  # calc Ahat - estimate of kelp area A
  iter_df <- iter_df %>%
    mutate(Ahat = A + e1 + e2 + e3 + e4)
  
  # create result record for this iteration and append to results df 
  results_rec <- data.frame(
    rho_e1_e2 = cor(iter_df$e1, iter_df$e2),
    rho_e1_e3 = cor(iter_df$e1, iter_df$e3),
    rho_e1_e4 = cor(iter_df$e1, iter_df$e4),
    rho_e2_e3 = cor(iter_df$e2, iter_df$e3),
    rho_e2_e4 = cor(iter_df$e2, iter_df$e4),
    rho_e3_e4 = cor(iter_df$e3, iter_df$e4),
    var_Ahat = var(iter_df$Ahat)
  )
  results_df <- rbind(results_df, results_rec) 
  
}  # close for loop through corr scenarios

# calc new variable delta-stdev for each scenario. This is the difference
# between the given scenario and the all rho=0 scenario.
results_df2 <- results_df %>%
  mutate()

# make delta-stdev bar graphs - 1 bar for each scenario



# make scenario correlations point or bar graphs. One panel/facet for each
# scenario with correlation error-pairs on x and correlation on y.




