###############################################################################
#
# run_iterations()
#
# Runs iterations for kelp error study for generation of Appendix figure
# for kelp area with panels of change in stdev of area estimate.
#
# Calculations are repeated Niter times with each calculation producing
# estimates of source errors and Ahat.
#
# From the completed iterations is determined: 
#  - 6 pairwise correlations among the error terms
#  - variance of Ahat
#
# Function mvrnorm from the MASS package is used to generate correlated 
# error terms.
#
# October 2026
#
###############################################################################

library(tibble)
library(dplyr)
library(MASS)

run_iterations <- function(k,rho,A,Niter) {
  
  set.seed(73)

   
  # create pairwise correlation matrix for the 4 error sources
  cor_matrix <- matrix(rho, nrow=4, ncol=4)
  diag(cor_matrix) <- 1
  
  # test eigen values of correlation matrix to ensure will work with mvrnorm
  eigen_vals <- eigen(cor_matrix)$values
  if (any(eigen_vals < 0)) {
    print("negative eigen value")
  }
   
  # use cor_matrix to generate standard normal random variates for the four
  # error sources 
  sim_data <- mvrnorm(n=Niter, mu=rep(0,4), Sigma=cor_matrix)
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
  
  # summarize results from iter_df
  resultsObj <- list(
    rho_e1_e2 = cor(iter_df$e1, iter_df$e2),
    rho_e1_e3 = cor(iter_df$e1, iter_df$e3),
    rho_e1_e4 = cor(iter_df$e1, iter_df$e4),
    rho_e2_e3 = cor(iter_df$e2, iter_df$e3),
    rho_e2_e4 = cor(iter_df$e2, iter_df$e4),
    rho_e3_e4 = cor(iter_df$e3, iter_df$e4),
    var_Ahat = var(iter_df$Ahat)
  )
  
  return(resultsObj) 
  
}   # end function definition



