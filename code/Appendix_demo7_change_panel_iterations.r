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

run_chg_iterations <- function(k,rho,A1,A2,Niter) {
  
  # create pairwise correlation matrix for the 4 error sources
  cor_matrix <- matrix(rho, nrow=4, ncol=4)
  diag(cor_matrix) <- 1
  
  # test eigen values of correlation matrix to ensure will work with mvrnorm
  eigen_vals <- eigen(cor_matrix)$values
  if (any(eigen_vals < 0)) {
    print("ERROR:  negative eigen value within iteration function.")
  }
   
  # use cor_matrix to generate standard normal random variates for the four
  # error sources for time 1 and time 2
  z1_sim_data <- mvrnorm(n=Niter, mu=rep(0,4), Sigma=cor_matrix)
  z2_sim_data <- mvrnorm(n=Niter, mu=rep(0,4), Sigma=cor_matrix)
  iter_df_1 <- as.data.frame(z1_sim_data)
  iter_df_2 <- as.data.frame(z2_sim_data)
  names(iter_df_1) <- c("zX1","zX2","zX3","zX4")
  names(iter_df_2) <- c("zX1","zX2","zX3","zX4")
  
  # all 4 error terms at a survey time (1 or 2) share same stdev
  error_stdev_1 <- k*A1
  error_stdev_2 <- k*A2
  
  # scale error z values to actual error values
  iter_df_1 <- iter_df_1 %>%
    mutate(e1 = zX1 * error_stdev_1,
           e2 = zX2 * error_stdev_1,
           e3 = zX3 * error_stdev_1,
           e4 = zX4 * error_stdev_1)
  iter_df_2 <- iter_df_2 %>%
    mutate(e1 = zX1 * error_stdev_2,
           e2 = zX2 * error_stdev_2,
           e3 = zX3 * error_stdev_2,
           e4 = zX4 * error_stdev_2)
  
  # calc Ahat - estimate of kelp area A1 and A2 and variance of difference
  iter_df_1 <- iter_df_1 %>% mutate(A1hat = A1 + e1 + e2 + e3 + e4)
  iter_df_2 <- iter_df_2 %>% mutate(A2hat = A2 + e1 + e2 + e3 + e4)
  deltaA <- iter_df_2$A2hat - iter_df_1$A1hat
  
  # summarize results from iter_df
  resultsObj <- list(
    rho_A1_e1_e2 = cor(iter_df_1$e1, iter_df_1$e2),
    rho_A1_e1_e3 = cor(iter_df_1$e1, iter_df_1$e3),
    rho_A1_e1_e4 = cor(iter_df_1$e1, iter_df_1$e4),
    rho_A1_e2_e3 = cor(iter_df_1$e2, iter_df_1$e3),
    rho_A1_e2_e4 = cor(iter_df_1$e2, iter_df_1$e4),
    rho_A1_e3_e4 = cor(iter_df_1$e3, iter_df_1$e4),
    rho_A2_e1_e2 = cor(iter_df_2$e1, iter_df_2$e2),
    rho_A2_e1_e3 = cor(iter_df_2$e1, iter_df_2$e3),
    rho_A2_e1_e4 = cor(iter_df_2$e1, iter_df_2$e4),
    rho_A2_e2_e3 = cor(iter_df_2$e2, iter_df_2$e3),
    rho_A2_e2_e4 = cor(iter_df_2$e2, iter_df_2$e4),
    rho_A2_e3_e4 = cor(iter_df_2$e3, iter_df_2$e4),
    corr_A1_A2 = cor(iter_df_1$A1hat, iter_df_2$A2hat),
    var_deltaA = var(deltaA),
    var_A1hat = var(iter_df_1$A1hat),
    var_A2hat = var(iter_df_2$A2hat)
  )
  
  return(resultsObj) 
  
}   # end function definition



