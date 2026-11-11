###############################################################################
#  Appendix demo 5 
#
#  Effect of differences among pairwise correlations.
#  Goal is to produce to graphical outputs:
#   1) a bar graph: y:delta-stdev  x:discrete pairwise scenarios
#   2) facets of point or bar graphs:  y:pairwise correlation, x=6 pairs,
#      facets are the six discrete pairwise scenarios
#
#  November 2026
#
###############################################################################


library(tibble)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(MASS)


############################################################################
#  set fixed parameters and initialize results df
############################################################################
A <- 5   # kelp area, hectares
k <- 0.1
N <- 10000

results_df <- data.frame(
  rho_e1_e2 = numeric(),
  rho_e1_e3 = numeric(),
  rho_e1_e4 = numeric(),
  rho_e2_e3 = numeric(),
  rho_e2_e4 = numeric(),
  rho_e3_e4 = numeric(),
  var_Ahat = numeric()
)
 

############################################################################
# create correlation matrices for the six scenarios of pairwise correlation
# and store in list corr_scenarios
############################################################################

corr_scenarios <- list()
init_matrix <- matrix(0.0, nrow=4, ncol=4)
diag(init_matrix) <- 1


### no pairwise correlation - baseline scenario
corr_scenarios[[1]] <- init_matrix


### one pair correlated
hi_corr_value <- 0.8
corr_matrix <- init_matrix
corr_matrix[1,2] <- hi_corr_value
corr_matrix[2,1] <- hi_corr_value
corr_scenarios[[2]] <- corr_matrix

# test eigen values of correlation matrix to ensure will work with mvrnorm
eigen_vals <- eigen(corr_matrix)$values
if (any(eigen_vals < 0)) {
  print("negative eigen value")
}

### two pair correlated
hi_corr_value <- 0.7
corr_matrix <- init_matrix
corr_matrix[1,2] <- hi_corr_value
corr_matrix[2,1] <- hi_corr_value
corr_matrix[1,3] <- hi_corr_value
corr_matrix[3,1] <- hi_corr_value
corr_scenarios[[3]] <- corr_matrix

# test eigen values of correlation matrix to ensure will work with mvrnorm
eigen_vals <- eigen(corr_matrix)$values
if (any(eigen_vals < 0)) {
  print("negative eigen value")
}


### three pair correlated
hi_corr_value <- 0.5
corr_matrix <- init_matrix
corr_matrix[1,2] <- hi_corr_value
corr_matrix[2,1] <- hi_corr_value
corr_matrix[1,3] <- hi_corr_value
corr_matrix[3,1] <- hi_corr_value
corr_matrix[1,4] <- hi_corr_value
corr_matrix[4,1] <- hi_corr_value
corr_scenarios[[4]] <- corr_matrix

# test eigen values of correlation matrix to ensure will work with mvrnorm
eigen_vals <- eigen(corr_matrix)$values
if (any(eigen_vals < 0)) {
  print("negative eigen value")
}



### four pair correlated
hi_corr_value <- 0.6
corr_matrix <- init_matrix
corr_matrix[1,2] <- hi_corr_value
corr_matrix[2,1] <- hi_corr_value
corr_matrix[1,3] <- hi_corr_value
corr_matrix[3,1] <- hi_corr_value
corr_matrix[1,4] <- hi_corr_value
corr_matrix[4,1] <- hi_corr_value
corr_matrix[2,3] <- hi_corr_value
corr_matrix[3,2] <- hi_corr_value
corr_scenarios[[5]] <- corr_matrix

# test eigen values of correlation matrix to ensure will work with mvrnorm
eigen_vals <- eigen(corr_matrix)$values
if (any(eigen_vals < 0)) {
  print("negative eigen value")
}



### five pair correlated
hi_corr_value <- 0.6
corr_matrix <- init_matrix
corr_matrix[1,2] <- hi_corr_value
corr_matrix[2,1] <- hi_corr_value
corr_matrix[1,3] <- hi_corr_value
corr_matrix[3,1] <- hi_corr_value
corr_matrix[1,4] <- hi_corr_value
corr_matrix[4,1] <- hi_corr_value
corr_matrix[2,3] <- hi_corr_value
corr_matrix[3,2] <- hi_corr_value
corr_matrix[2,4] <- hi_corr_value
corr_matrix[4,2] <- hi_corr_value
corr_scenarios[[6]] <- corr_matrix

# test eigen values of correlation matrix to ensure will work with mvrnorm
eigen_vals <- eigen(corr_matrix)$values
if (any(eigen_vals < 0)) {
  print("negative eigen value")
}



### six pair correlated
hi_corr_value <- 0.8
corr_matrix <- init_matrix
corr_matrix[1,2] <- hi_corr_value
corr_matrix[2,1] <- hi_corr_value
corr_matrix[1,3] <- hi_corr_value
corr_matrix[3,1] <- hi_corr_value
corr_matrix[1,4] <- hi_corr_value
corr_matrix[4,1] <- hi_corr_value
corr_matrix[2,3] <- hi_corr_value
corr_matrix[3,2] <- hi_corr_value
corr_matrix[2,4] <- hi_corr_value
corr_matrix[4,2] <- hi_corr_value
corr_matrix[3,4] <- hi_corr_value
corr_matrix[4,3] <- hi_corr_value
corr_scenarios[[7]] <- corr_matrix

# test eigen values of correlation matrix to ensure will work with mvrnorm
eigen_vals <- eigen(corr_matrix)$values
if (any(eigen_vals < 0)) {
  print("negative eigen value")
}

  
############################################################################
# loop through pairwise scenarios, create df with simulations for each 
# scenario, save only scenario results 
############################################################################
for (i in seq_along(corr_scenarios)) {
  
  # use corr matrix to generate standard normal random variates as the  
  # starting point for creating the four error sources 
  sim_data <- mvrnorm(n=N, mu=rep(0,4), Sigma=corr_scenarios[[i]])
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
  
  # create result record for this iteration and append to results df 
  results_rec <- data.frame(
    rho_e1_e2 = cor(iter_df$e1, iter_df$e2),
    rho_e1_e3 = cor(iter_df$e1, iter_df$e3),
    rho_e1_e4 = cor(iter_df$e1, iter_df$e4),
    rho_e2_e3 = cor(iter_df$e2, iter_df$e3),
    rho_e2_e4 = cor(iter_df$e2, iter_df$e4),
    rho_e3_e4 = cor(iter_df$e3, iter_df$e4),
    var_Ahat = var(iter_df$Ahat)
  )
  results_df <- rbind(results_df, results_rec) 
  
}  # close for loop through corr scenarios


############################################################################
# create final data frame for graphing - add stdev relative to rho=0 
############################################################################

# calc new variable delta-stdev for each scenario. This is the difference
# between the given scenario and the all rho=0 scenario.
baseline_stdev <- sqrt(results_df$var_Ahat[1])
results_df2 <- results_df %>%
  mutate(delta_stdev_Ahat = (sqrt(var_Ahat) - baseline_stdev)/
                              baseline_stdev,
         scenario = str_c("Scenario ",as.character(row_number() - 1))) %>%
  filter(scenario != "Scenario 0") 


############################################################################
# create graphs 
############################################################################
  
# make delta-stdev bar graphs - 1 bar for each scenario
p1 <- ggplot(data = results_df2,
             mapping = aes(x=scenario, y=delta_stdev_Ahat)) +
      geom_line(mapping = aes(group=1), color="gray40") +
      geom_point(size=4) +
      theme_bw() +
      theme(
        axis.title.x = element_text(margin = margin(t=10), size=16),
        axis.title.y = element_text(margin = margin(r=15)),
        axis.text = element_text(size=14)
      ) +
      scale_x_discrete(name="Scenario") +
      scale_y_continuous(name="delta-stdev-Ahat")


# make scenario facet graphs showing corr for each error pair
# first pivot longer to make tidy for use of faceting
pair_data <- results_df2 %>% 
  dplyr::select(-var_Ahat, -delta_stdev_Ahat) %>%
  pivot_longer(cols=c("rho_e1_e2","rho_e1_e3","rho_e1_e4","rho_e2_e3",
                      "rho_e2_e4","rho_e3_e4"), names_to="error_pair",
                      values_to="correlation")

p2 <- ggplot(data = pair_data,
             mapping = aes(x=error_pair, y=correlation)) +
      geom_point() +
      facet_grid(col=vars(scenario)) +
      theme_bw() +
      theme(
        axis.title.x = element_text(margin=margin(t=10)),
        axis.title.y = element_text(margin=margin(r=15))
      ) +
      scale_x_discrete(labels=c(1,2,3,4,5,6), name="Error term pair") +
      scale_y_continuous(name="Correlation")









