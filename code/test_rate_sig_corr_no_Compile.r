# script to source test_rate_of_sig_corr_no_A_dep.r multiple times and
# compile the rates of false positives

N <- 10  # number of times to source and get value of empirical alpha

empirical_alpha_vect <- numeric(N)

for (j in seq(1,N)) {
  source("code/test_rate_sig_corr_no_A_dep.r")
  empirical_alpha_vect[j] <- empirical_alpha
  cat(sprintf("j=%d   length=%d\n",j,length(empirical_alpha_vect)))
}

dfalpha <- data.frame(
  alpha_empirical = empirical_alpha_vect,
  y = rep(0, length(empirical_alpha_vect))
)
p10 <- ggplot(data = dfalpha, 
              mapping = aes(x=alpha_empirical, y=y)) +
       geom_point() +
       theme_bw() +
       theme(
         axis.text.y = element_blank(),
         axis.title.y = element_blank()
       ) +
       scale_x_continuous(limits=c(0, 0.1))
