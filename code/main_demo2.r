###############################################################################
# Demo 2 showing how the variance of the sum of two
# random variables responds to covariance.  This is demonstrated with simulated
# data.
#
# Variables are named to follow:  y = x1 + x2
#
# September 2026
###############################################################################

library(stringr)


###############################################################################
# set parameters
###############################################################################
N <- 10000    # size of x1 and x2 simulated datasets

# parameters for normal distributions: x1 and the 'parent' of versions of x2 
x1_mean <- 10 
x1_stdev <- 2

p2_mean <- 12
p2_stdev <- 3

# correlation values to create range of covariance between random variables 
corr_design_vals <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)



###############################################################################
#  create data frame with values of random variables x1 and multiple sets of
#  x2 with different levels of covariance relative to x1
###############################################################################

# generate x1 random variates as first column of data frame 
z1 <- rnorm(N)   # vector of standard normal random variates
data <- data.frame(x1 = x1_mean + z1*x1_stdev)

# create multiple series of x2 vectors with different covariance with x1
for (icorr in corr_design_vals) {
 x2_parent <- p2_mean + rnorm(N)*p2_stdev
 x2 <- icorr * data$x1 + sqrt(1 - icorr^2) * x2_parent 
 corr_str <- sprintf("%.1f", icorr)
 suffix <- str_remove(corr_str, "\\.")
 x2name <- str_c("x2", suffix, sep="_")
 data[[x2name]] <- x2
}
  


###############################################################################
#  get x1-x2 correlations
###############################################################################
corr_vals <- cor(data[,1], data[,2:7], use = "pairwise.complete.obs")





###############################################################################
# calc x1+x2 sums, population variances, covariances, correlations
###############################################################################





###############################################################################
#  graph panels - freq. histograms of x1, x2, x1+x2 
###############################################################################






