###############################################################################
# Appendix demo 6
#
# For kelp area change show if there is indirect correlation due to joint
# dependence on Area when each of the four error terms have their own
# distributions:
#   classification error:  centered for pos. and neg. errors
#   optical error:  negative values only
#   tidal error:  negative values only
#   phenology error: negative values only
#
#
###############################################################################


library(tidyr)
library(dplyr)
library(ggplot2)

# function generates random variates from normal distr. truncated to neg. values
get_neg_z <- function(n) {
  x <- rnorm(2.1*n)
  neg_x <- x[x<=0][1:n]
  if (length(neg_x) < n) {
    print("error with neg z")
  } 
  return(neg_x) 
}



##################### set parameters ####################

set.seed(64)

A1 <- 10  # true initial kelp area (hectares)
A2 <- c(9,8,7,6,5,4,3)

k <- 0.1  # fraction of kelp Area that is stdev of error distribution

nrows <- 10000   # number of simulated surveys 

# initialize results data frame
results_df <- data.frame(
  A1 = numeric(), A2 = numeric() 
)  


##########  loop through A2 values. For each value simulate and get corr ######
for (iA2 in seq_along(A2)) {

  # generate A1 and A2 error terms; e1,e2,e3 are truncated normal; e4 is normal 
  idf <- data.frame( 
      A1_e1 = k*A1*get_neg_z(nrows),
      A1_e2 = k*A1*get_neg_z(nrows),
      A1_e3 = k*A1*get_neg_z(nrows),
      A1_e4 = k*A1*rnorm(nrows),
      A2_e1 = k*A2[iA2]*get_neg_z(nrows),
      A2_e2 = k*A2[iA2]*get_neg_z(nrows),
      A2_e3 = k*A2[iA2]*get_neg_z(nrows),
      A2_e4 = k*A2[iA2]*rnorm(nrows)
  )
  
  results_rec <- data.frame(
   A1 = A1,
   A2 = A2[iA2],
  )
  results_df <- rbind(results_df, results_rec)

}


