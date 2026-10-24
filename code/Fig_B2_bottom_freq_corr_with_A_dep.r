###############################################################################
# Test rate of sig. correlation with four independently generated random vars
#
# October 2026
###############################################################################

library(dplyr)
library(ggplot2)
library(rstatix)

# get start time
start_time <- Sys.time()



##################### set parameters ####################

set.seed(184)

A <- 5  # true kelp area (hectares)

k <- 0.1  # fraction of kelp Area that is stdev of error distribution

A <- 5   #  true kelp area, i.e. 5 ha

nrows <- 100    #  length of simulated data record, e.g. 20 yrs of monitoring data

# number of iterations to run, with 4 simulations nested within each iteration
nreps <- 1000 

# set bins of correlation for graphing
bins <- seq(-0.4,0.4, by=0.020)
bin_upper_lim <- bins[-1]

# create empty lists to contain frequencies from each iteration
table_list <- list()
table_list_sig <- list()


target_alpha <- 0.05



################ iterations of simulating data and calc correlation ###########

sig_counter <- 1  # index for list of iterations with sig. correlation
for (i in seq(1,nreps)) {

  # write occasional update to console
  if (i %% 50 == 0) {
    message(sprintf("Working on iteration %d",i))
  }
  
  df <- data.frame( 
      e1 = k*A*rnorm(nrows),
      e2 = k*A*rnorm(nrows),
      e3 = k*A*rnorm(nrows),
      e4 = k*A*rnorm(nrows)
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
  corr.out.sig <- corr.out %>% filter(p <= target_alpha)
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

empirical_alpha <- sum(df2$freq_corr_sig) / sum(df2$freq_corr_all)

pA <- ggplot(data = df2, 
            mapping = aes(x=bin_upper_lim, y=freq_corr_all)) +
     geom_col(fill="gray80", color="white") +
     geom_col(aes(y=freq_corr_sig), fill="red2") +
     theme_bw() +
     theme(
      axis.text = element_text(size=10),
      axis.title.x = element_text(size=14, margin=margin(t=10)),
      axis.title.y = element_text(size=14, margin=margin(r=10))
     ) +
     scale_x_continuous(name="Pearson Correlation Coefficient") +
     scale_y_continuous(name="Frequency", limits=c(0,500))

# get end time
end_time <- Sys.time()
elapsed_time <- end_time - start_time
print(elapsed_time)


