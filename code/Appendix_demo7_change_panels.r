###############################################################################
#
#  Simulations and creation of panel fig for kelp change.  
#  Panels showing 
#    x dimension panels:  different kelp area at time 2 
#    y dimension panels:  different levels of k
#
#  October 2026
#
###############################################################################

library(dplyr)
library(ggplot2)

set.seed(93)


# get start time
start_time <- Sys.time()
cat("\nStarting....\n")

source("code/Appendix_demo7_change_panel_iterations.r")


###############################################################################
# set parameters
###############################################################################

A1 <- 10
A2vals <- c(2, 5, 8, 10)

# ordered with rho=0 first to serve as baseline
rho <- c(0.0, -0.3, -0.2, -0.1, 0.1, 0.2, 0.3)
k <- c(0.1, 0.2, 0.3)

N <- 2000  # number of iterations for each set of conditions

# initialize results data frame
results <- data.frame(k = numeric(), rho_target = numeric(), A1 = numeric(),
                      A2 = numeric(), N = numeric(),
                      rho_e1_e2 = numeric(), rho_e1_e3 = numeric(),
                      rho_e1_e4 = numeric(), rho_e2_e3 = numeric(), 
                      rho_e2_e4 = numeric(), rho_e3_e4 = numeric(), 
                      corr_A1A2 = numeric(), var_deltaA = numeric(),
                      var_A1hat = numeric(), var_A2hat = numeric())



###############################################################################
#  Use nested looping to step through all model conditions and run iterations
#  for each scenario. 
###############################################################################

for (ik in k) {
  for (irho in rho) {
    cat(sprintf("ik=%f  irho=%f\n",ik,irho))
    
    for (iA2 in A2vals) {
      iter_Obj <- run_chg_iterations(ik, irho, A1, iA2, N)
      results_rec <- data.frame(
        k = ik,
        rho_target = irho,
        A1 = A1,
        A2 = iA2,
        N = N,
        rho_A1_e1_e2 = iter_Obj[[1]],
        rho_A1_e1_e3 = iter_Obj[[2]],
        rho_A1_e1_e4 = iter_Obj[[3]],
        rho_A1_e2_e3 = iter_Obj[[4]],
        rho_A1_e2_e4 = iter_Obj[[5]],
        rho_A1_e3_e4 = iter_Obj[[6]],
        rho_A2_e1_e2 = iter_Obj[[7]],
        rho_A2_e1_e3 = iter_Obj[[8]],
        rho_A2_e1_e4 = iter_Obj[[9]],
        rho_A2_e2_e3 = iter_Obj[[10]],
        rho_A2_e2_e4 = iter_Obj[[11]],
        rho_A2_e3_e4 = iter_Obj[[12]],
        corr_A1A2 = iter_Obj[[13]],
        var_deltaA = iter_Obj[[14]],
        var_A1hat = iter_Obj[[15]],
        var_A2hat = iter_Obj[[16]]
      )   # close creation of results record
      
      results <- rbind(results, results_rec) 
    } 
  }
}

# add variable to results that is delta-stdev of Ahat
# first extract rho=0 results records, then join back onto main results df
rho0_results <- results %>% 
  filter(rho_target == 0.0)  %>%
  mutate(rho0_var_deltaA = var_deltaA) %>%
  dplyr::select(k,A2,rho0_var_deltaA)
results_jn <- results %>% left_join(rho0_results, by=join_by(k,A2))
results_final <- results_jn %>%
  mutate(
    delta_stdev_deltaAhat = (sqrt(var_deltaA) - sqrt(rho0_var_deltaA))/
                               sqrt(rho0_var_deltaA),
    rho_target_fct = factor(rho_target, levels=c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3)),
    k_fct = factor(k, levels=c(0.1, 0.2, 0.3)),
    A2_fct = factor(A2, levels=c(2,5,8,10))
  )
         

###############################################################################
#  Create panel figures
###############################################################################

pCorr <- ggplot(data=results_final,
            mapping=aes(x=rho_target_fct, y=corr_A1A2, fill=k_fct)) +
         geom_col(position="dodge", color="black", width=0.7) +
         facet_grid(cols=vars(A2_fct)) +
         theme_bw() +
         theme(
            axis.text = element_text(size=10)
         ) +
         scale_y_continuous(limits=c(-0.1, 0.1)) +
         scale_fill_discrete(name="k")
pVarA1hat <- ggplot(data=results_final,
              mapping=aes(x=rho_target_fct, y=var_A1hat, fill=k_fct)) +
          geom_col(position="dodge", color="gray20", width=0.7) +
          facet_grid(cols=vars(A2_fct)) +
          theme_bw() +
          theme(
            axis.text = element_text(size=10)
          )
pVarA2hat <- ggplot(data=results_final,
              mapping=aes(x=rho_target_fct, y=var_A2hat, fill=k_fct)) +
          geom_col(position="dodge", color="gray20", width=0.7) +
          facet_grid(cols=vars(A2_fct)) +
          theme_bw() +
          theme(
            axis.text = element_text(size=10)
          )

pVarDeltaA <- ggplot(data=results_final,
              mapping=aes(x=rho_target_fct, y=var_deltaA, fill=k_fct)) +
          geom_col(position="dodge", color="gray20", width=0.7) +
          facet_grid(cols=vars(A2_fct)) +
          theme_bw() +
          theme(
            axis.text = element_text(size=10)
          )
pDeltaStdev <- ggplot(data=results_final,
                      mapping=aes(x=rho_target_fct, y=delta_stdev_deltaAhat,
                                  fill=k_fct)) +
               geom_col(position="dodge", color="gray20", width=0.7) +
               facet_grid(cols=vars(A2_fct)) +
               theme_bw() +
               theme(
                 axis.text = element_text(size=10) 
               )

                  



# get end time
end_time <- Sys.time()
elapsed_time <- end_time - start_time
print(elapsed_time)
