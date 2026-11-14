###############################################################################
# Appendix demo 6 - modified code for testing only - not to be used in report 
#
# When the demo 6 results did not show any correlation between A1hat & A2hat,
# and some correlation was expected, this code was modified so that the same
# random variates used to generate error terms were used for both A1hat and
# A2hat.  This is not a useful model for the goals of this work, but is intended
# to create a condition where correlations should be present to ensure the math
# is working as intended.
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

# get start time
start_time <- Sys.time()



# function generates random variates from normal distr. truncated to neg. values
get_neg_z <- function(n) {
  x <- rnorm(2.1*n)
  neg_x <- x[x<=0][1:n]
  if (anyNA(neg_x)) {
    print("error with neg z")
  } 
  return(neg_x) 
}



##################### set parameters ####################

set.seed(64)

A1 <- 10  # true initial kelp area (hectares)
A2 <- c(9,8,7,6,5,4,3)

k <- 0.1  # fraction of kelp Area that is stdev of error distribution

# number of simulated surveys for each A2 scenario that are used to calculate
# correlation coefficient (counts as one iteration).
nObs <- 5000 

# number of iterations where each iteration gives a correlation value
nIterations <- 10000

# initialize results data frame - to hold nrows = nIterations x length(A2)
results_df <- data.frame(
  A1 = numeric(), A2 = numeric(), corr_A1est_A2est = numeric() 
)  




##########  loop through A2 values. For each value simulate and get corr ######
for (iA2 in seq_along(A2)) {

  # loop through the iterations - each iteration produces a correlation value
  for (iter in seq(1,nIterations)) {
  
    # write occasional update to console
    if (iter %% 50 == 0) {
      message(sprintf("Working on A2[%d] iteration %d",iA2,iter))
    }
    
    # generate A1 and A2 error terms; e1,e2,e3 are truncated normal; e4 is normal 
    neg_z_variates <- get_neg_z(nObs)
    centered_z_variates <- rnorm(nObs) 
    iter_df <- data.frame (
      A1 = rep(A1, times=nObs),
      A2 = rep(A2, times=nObs),
      A1_e1 <- k*A1*neg_z_variates,
      A1_e2 <- k*A1*neg_z_variates,
      A1_e3 <- k*A1*neg_z_variates,
      A1_e4 <- k*A1*centered_z_variates,
      A2_e1 <- k*A2[iA2]*neg_z_variates,
      A2_e2 <- k*A2[iA2]*neg_z_variates,
      A2_e3 <- k*A2[iA2]*neg_z_variates,
      A2_e4 <- k*A2[iA2]*centered_z_variates
    )
    iter_df <- iter_df %>%
      mutate(A1est = A1 + A1_e1 + A1_e2 + A1_e3 + A1_e4,
             A2est = A2 + A2_e1 + A2_e2 + A2_e3 + A2_e4)
    results_rec <- data.frame(A1=A1, A2=A2[iA2],
                            cor_A1est_A2est = cor(iter_df$A1est, iter_df$A2est))
    results_df <- rbind(results_df, results_rec)
   
  }  # end for loop through iterations 
}  # end for loop through A2 values  



##############################################################################
#  construct data to represent histograms as rectangles 
##############################################################################
hist_df <- results_df %>%
  group_by(A2) %>%
  reframe({
    h = hist(cor_A1est_A2est, breaks=50, plot=FALSE)
    data.frame(
      ymin = head(h$breaks, -1),
      ymax = tail(h$breaks, -1),
      count = h$counts,
      density = h$density
    )
  }) %>%
  # Scale bar width so the tallest bar spans a desired width on the x-axis (e.g., 0.8 units)
  mutate(bar_width = (count / max(count)) * 0.8)



##############################################################################
#  construct graph 
##############################################################################
p <- ggplot(hist_df) +
  # Histogram bars extending to the right of x_pos
  geom_rect(
    aes(
      xmin = A2,
      xmax = A2 + bar_width,
      ymin = ymin,
      ymax = ymax
    ),
    fill = "coral",
    color = "white",
    linewidth = 0.3
  ) +
  theme_bw() +
  theme(
   axis.title.x = element_text(margin=margin(t=15), size=12),
   axis.title.y = element_text(margin=margin(r=15), size=12),
   axis.text = element_text(size=10),
   panel.grid.minor.x = element_blank()
  ) +
  scale_x_continuous(breaks=c(3,4,5,6,7,8,9)) +
  labs(x = "True A2 (ha)", y = "Correlation  (A1 & A2 estimates)")


# get end time
end_time <- Sys.time()
elapsed_time <- end_time - start_time
print(elapsed_time)








