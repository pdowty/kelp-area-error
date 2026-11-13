###############################################################################
# Appendix demo 5
#
# For kelp area change show if there is indirect correlation due to joint
# dependence on Area when all errors have same centered Gaussian error 
# distributions.
#
#
###############################################################################

library(tidyr)
library(dplyr)
library(ggplot2)


##################### set parameters ####################

set.seed(164)

A1 <- 10  # true initial kelp area (hectares)
A2 <- c(9,8,7,6,5,4,3)

k <- 0.1  # fraction of kelp Area that is stdev of error distribution

# number of simulated surveys for each A2 scenario that are used to calculate
# correlation coefficient.
nObs <- 5000 

# number of iterations where each iteration gives a correlation value
nIterations <- 10000

# initialize results data frame - to hold nrows = nIterations x length(A2)
results_df <- data.frame(
  A1 = numeric(), A2 = numeric(), corr_A1est_A2est = numeric() 
)  


##########  loop through A2 values. For each value simulate values ########## 
for (iA2 in seq_along(A2)) {
  
  # loop through the iterations - each iteration produces a correlation value
  for (iter in seq(1,nIterations)) {

    # generate nObs instances of the A1 error terms as rows in one data frame
    iter_df <- data.frame (
      A1_e1 = k*A1*rnorm(nObs)
      A1_e2 = k*A1*rnorm(nObs)
      A1_e3 = k*A1*rnorm(nObs)
      A1_e4 = k*A1*rnorm(nObs)
      A2_e1 = k*A2[iA2]*rnorm(nObs)
      A2_e2 = k*A2[iA2]*rnorm(nObs)
      A2_e3 = k*A2[iA2]*rnorm(nObs)
      A2_e4 = k*A2[iA2]*rnorm(nObs)
    )
  
    iter_df <- iter_df %>%
      mutate(A1 = rep(A1, times=nObs),
             A2 = rep(A2[iA2], times=nObs),
             A1est = A1 + A1_e1 + A1_e2 + A1_e3 + A1_e4i,
             A2est = A2 + A2_e1 + A2_e2 + A2_e3 + A2_e4
      )
    
    
    results_df <- rbind(results_df, results_rec)
  }  # end for loop through iterations 
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
      theme_bw() +
      theme(
        axis.title.x = element_text(margin=margin(t=10)),
        axis.title.y = element_text(margin=margin(r=15))
      ) +
      scale_x_discrete(name="Error Term Pair",
                       labels=c("e1 pair","e2 pair","e3 pair","e4 pair")) +
      scale_y_continuous(name="Correlation", limits=c(-0.1,0.1)) +
      scale_color_discrete(name="True Prop. Kelp Area Change") +
      scale_fill_discrete(name="True Prop. Kelp Area Change")






