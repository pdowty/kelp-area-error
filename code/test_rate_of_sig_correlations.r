###############################################################################
# Test rate of sig. correlation with four independently generated random vars
#
# October 2026
###############################################################################

library(dplyr)
library(ggplot2)
library(rstatix)


# generate 4 error vectors as function of A
nrows <- 20    #  20 years of monitoring data

nreps <- 2000  # number of simulations to run
bins <- seq(-0.8,0.8, by=0.05)
bin_upper_lim <- bins[-1]

# create empty lists to contain frequencies from each iteration
table_list <- list()
table_list_sig <- list()

set.seed(42)   # for reproducibility

# run through simulations and count cases with r bins with significance flag
sig_counter <- 1
for (i in seq(1,nreps)) {

  df <- data.frame( 
      e1 = rnorm(nrows),
      e2 = rnorm(nrows),
      e3 = rnorm(nrows),
      e4 = rnorm(nrows)
    )

  corr.out <- df %>% cor_test(e1, e2, e3, e4)
  corr.out <- corr.out %>%
    filter(cor != 1,
           !(var1=="e2" & var2=="e1"),
           !(var1=="e3" & var2=="e1"),
           !(var1=="e3" & var2=="e2"),
           var1 != "e4")
  binned_data <- cut(corr.out$cor, breaks=bins, include.lowest=TRUE)
 
  # save the current table to table list
  table_list[[i]] <- table(binned_data)
   
  # filter for statistically significant cases and bin separately
  corr.out.sig <- corr.out %>% filter(p <= 0.05)
  if (dim(corr.out.sig)[1] > 0) {
    binned_data_sig <- cut(corr.out.sig$cor, breaks=bins, include.lowest=TRUE)
    table_list_sig[[sig_counter]] <- table(binned_data_sig)
    sig_counter <- sig_counter + 1
  }
  
}  # close main for loop


# convert the list of tables into matrix and sum acrocc columns
combined_tbl <- rowSums(do.call(cbind, table_list))
combined_tbl_sig <- rowSums(do.call(cbind, table_list_sig))
# convert back to data frame 
freq_table_all <- as.data.frame(combined_tbl)
freq_table_sig <- as.data.frame(combined_tbl_sig)
 
df2 <- data.frame(
  bin_upper_lim = bin_upper_lim,
  freq_corr_all = freq_table_all$combined_tbl,
  freq_corr_sig = freq_table_sig$combined_tbl_sig
)




