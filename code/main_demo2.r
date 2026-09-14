###############################################################################
# Demo 2 showing how the variance of the sum of two random variables
# responds to covariance between the two random variables. This is 
# demonstrated with simulated data.
#
# Variables are named to follow  y = x1 + x2 where all are random variables.
# Variable z is a precursor random variable used to create x2.
#
# September 2026
###############################################################################

library(tidyr)
library(dplyr)
library(stringr)


###############################################################################
# set parameters
###############################################################################
N <- 10000    # size of x1 and x2 simulated vectors 

# parameters for normal distributions for freq. distributions of x1 and x2
x1_mean <- 10 
x1_stdev <- 2

x2_mean <- 12
x2_stdev <- 3

# correlation values to create range of covariance between x1 and x2 
corr_target_vals <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)



###############################################################################
#  create data frame with N rows and n columns with structure:
#    col 1: values of random variable x1 
#    col 2: random variates x2 with corr_vals[1] correlation with x2
#    col 2: random variates x2 with corr_vals[2] correlation with x2
#    col 2: random variates x2 with corr_vals[3] correlation with x2
#    .....
#    col n: random variates x2 with corr_vals[n] correlation with x2
###############################################################################

# first make data frame of same dimensions [N, n] with columns of independent
# standard normal random variates. Col 1 is std normal version of x1.
# Cols 2:n are all instances of z, which is also standard normal.
nrows <- N
ncols <- length(corr_target_vals) + 1
data1 <- as.data.frame(matrix(rnorm(nrows*ncols), nrow=nrows, ncol=ncols))

# Transform the z columns to standard normal versions of x2 with specified
# correlation with x1
data2 <- data1
for (icol in seq(2,ncols)) {
 target_corr <- corr_target_vals[icol - 1]
 data2[,icol] <- data2[,1]*target_corr + sqrt(1 - target_corr^2) * data2[,icol]
}

# Scale col 1 using x1 parameters, cols 2:n using x2 parameters
data3 <- data2 
data3[,1] <- x1_mean + data2[,1]*x1_stdev
for (icol in seq(2:ncols)) {
  data3[,icol] <- x2_mean + data2[,icol] * x2_stdev
}

# create meaningful column names
df_names <- character(ncols)
df_names[1] <- "x1"
for (icol in seq(2,ncols)) {
 icorr <- corr_target_vals[icol - 1]
 corr_str <- sprintf("%.1f", icorr)
 suffix <- str_remove(corr_str, "\\.")
 x2name <- str_c("x2", suffix, sep="_")
 df_names[icol] <- x2name 
}
names(data3) <- df_names  


###############################################################################
#  get x1-x2 correlations
###############################################################################
corr_vals <- cor(data3[,1], data3[,2:7], use = "pairwise.complete.obs")



###############################################################################
# calc x1+x2 sums, population variances, covariances
# covariance(x,y) = correlation(x,y) / (stdev(x) * stdev(y))
###############################################################################
for (icol in seq(2,ncols)) {
  colname <- str_c("sum_",names(data3)[icol])
  data3[[colname]] <- data3[,1] + data3[,icol]
}
col_vars <- sapply(data3, var)
cov_vals <- corr_vals * sqrt(col_vars[2:ncols] * col_vars[1])



###############################################################################
#  graph panels - freq. histograms of x1, x2, x1+x2 
###############################################################################
# make tidy
data_tidy <- data3 |>
  pivot_longer(cols=everything(), cols_vary="slowest", names_to="variable",
               values_to="value") |>
  mutate(target_correlation = ifelse(str_detect(variable, "_"),
                                     as.numeric(str_extract(variable,"[^_]+$"))/10,
                                     9999),
         data_category = case_when(
           str_detect(variable,"x1") ~ "x1",
           str_detect(variable,"sum") ~ "sum",
           str_detect(variable,"x2") ~ "x2"
         ))

# loop through correlation levels
for (icorr in corr_target_vals) {
  # filter for records with icorr correlation or 9999 which are x1 records
  data_filt <- data_tidy |> filter(target_correlation == icorr |
                                   target_correlation == 9999.0)
  
  # make 3 stacked graph panels for freq. histograms of x1, x2, x1+x2
  
  
  # make separte set of graphs with centering (0 x value) on means
  
  
}











