# script to source test_rate_of_sig_corr_no_A_dep.r multiple times and
# compile the rates of false positives

N <- 10  # number of times to source and get value of empirical alpha

empirical_alpha_vect <- numeric()

for (i in seq(1,N)) {
  source("code/test_rate_sig_corr_no_A_dep.r")
  empirical_alpha_vect[i] <- empirical_alpha
}


p10 <- ggplot(data = empirical_alpha_vect,
              mapping = aes(x=empirical_alpha_vect),
              y = 0) +
       geom_point() +
       theme_bw()