###############################################################################
# Appendix demo 6
#
# For kelp area chnage show if there is indirect correlation due to joint
# dependence on Area
#
#
###############################################################################

library(tidyr)
library(ggplot2)


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
   A2 = A2[iA2],
   rho_e1_pair = cor(idf$A1_e1, idf$A2_e1),
   rho_e2_pair = cor(idf$A1_e2, idf$A2_e2),
   rho_e3_pair = cor(idf$A1_e3, idf$A2_e3),
   rho_e4_pair = cor(idf$A1_e4, idf$A2_e4)
  )  

  results_df <- rbind(results_df, results_rec)
  
}  # end for loop through A2 values  


## create graph 1: x:error pair  y=corr  grouping=deltaA_percent  
results_df <- results_df %>%
  mutate(rel_delta_A = (A2 - A1)/A1) %>%
  pivot_longer(cols=starts_with("rho"),
               names_to="error_pair",
               values_to="correlation") %>%
  mutate(rel_delta_A_fct = factor(rel_delta_A, levels=c(-0.7,-0.6,-0.5,-0.4,-0.3,
                                                 -0.2,-0.1)))


p1 <- ggplot(data = results_df,
             mapping = aes(x=error_pair,
                           y=correlation,
                           fill=rel_delta_A_fct,
                           color=rel_delta_A_fct, 
                           group=rel_delta_A_fct)) +
      geom_point() +
      geom_line() +
      theme_bw()






