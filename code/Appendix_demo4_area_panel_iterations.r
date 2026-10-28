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
# October 2026
#
###############################################################################

library(tibble)
library(dplyr)

run_iterations <- function(k,rho,A,Niter) {
  
  # generate normal random variates that will become the four error terms
  iter_df <- as.data.frame(
    matrix(rnorm(4*Niter), ncol=4, nrow=Niter)
  )  
  names(iter_df) <- c("zX1","z02","z03","z04")
  
  # calc correlated z values for the four error terms
  iter_df <- iter_df %>% 
    mutate(zX2 = rho * zX1 + sqrt(1.0 - rho*rho) * z02,
           zX3 = rho * zX1 + sqrt(1.0 - rho*rho) * z03,
           zX4 = rho * zX1 + sqrt(1.0 - rho*rho) * z04)
  
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



