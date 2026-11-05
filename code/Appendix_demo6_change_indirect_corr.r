###############################################################################
# Appendix demo 6
#
# For kelp area chnage show if there is indirect correlation due to joint
# dependence on Area
#
#
###############################################################################




##################### set parameters ####################

set.seed(164)

A1 <- 10  # true initial kelp area (hectares)
A2 <- c(9,8,7,6,5,4,3)

k <- 0.1  # fraction of kelp Area that is stdev of error distribution

nrows <- 10000   # number of simulated surveys 

# initialize results data frame
results_df <- data.frame(
  A1 = numeric(), A2 = numeric(), rho_e1_pair = numeric(), rho_e2_pair = numeric(),
  rho_e3_pair = numeric(), rho_e4_pair = numeric()
)  


##########  loop through A2 values. For each value simulate and get corr ######
for (iA2 in seq_along(A2)) {

  # generate A1 error terms  
  idf <- data.frame( 
      A1_e1 = k*A1*rnorm(nrows),
      A1_e2 = k*A1*rnorm(nrows),
      A1_e3 = k*A1*rnorm(nrows),
      A1_e4 = k*A1*rnorm(nrows),
      A2_e1 = k*A2[iA2]*rnorm(nrows),
      A2_e2 = k*A2[iA2]*rnorm(nrows),
      A2_e3 = k*A2[iA2]*rnorm(nrows),
      A2_e4 = k*A2[iA2]*rnorm(nrows)
  )
  
  results_rec <- data.frame(
   A1 = A1,
   A2 = iA2,
   rho_e1_pair = cor(idf$A1_e1, idf$A2_e1),
   rho_e2_pair = cor(idf$A1_e2, idf$A2_e2),
   rho_e3_pair = cor(idf$A1_e3, idf$A2_e3),
   rho_e4_pair = cor(idf$A1_e4, idf$A2_e4)
  )  

}  # end for loop through A2 values  


## create graph 1: x:error pair  y=corr  grouping=deltaA_percent  
  






