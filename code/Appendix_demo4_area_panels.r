###############################################################################
#
#  Simulations and creation of panel fig for kelp area.  
#  Panels showing Stdev (Ahat) as function of A:
#    x dimension panels:  different levels of correlation
#    y dimension panels:  different levels of k
#
#  October 2026
#
###############################################################################

library(dplyr)
library(ggplot2)


# get start time
start_time <- Sys.time()
cat("\nStarting....\n")

source("code/Appendix_demo4_area_panel_iterations.r")


###############################################################################
# set parameters
###############################################################################

Avals <- c(0.1,seq(0.5,10, by=0.5))

# ordered with rho=0 first to serve as baseline
rho <- c(0.0, -0.3, -0.2, -0.1, 0.1, 0.2, 0.3)
k <- c(0.1, 0.2, 0.3)

N <- 10000  # number of iterations for each set of conditions

# initialize results data frame
results <- data.frame(k = numeric(), rho_target = numeric(), A = numeric(),
                      N = numeric(), rho_e1_e2 = numeric(), rho_e1_e3 = numeric(),
                      rho_e1_e4 = numeric(), rho_e2_e3 = numeric(), 
                      rho_e2_e4 = numeric(), rho_e3_e4 = numeric(), 
                      var_Ahat = numeric())



###############################################################################
#  Use nested looping to step through all model conditions and run iterations
#  at each step.
###############################################################################

for (ik in k) {
  for (irho in rho) {
    cat(sprintf("ik=%f  irho=%f\n",ik,irho))
    
    for (iA in Avals) {
      iter_Obj <- run_iterations(ik, irho, iA, N)
      results_rec <- data.frame(
        k = ik,
        rho_target = irho,
        A = iA,
        N = N,
        rho_e1_e2 = iter_Obj[[1]],
        rho_e1_e3 = iter_Obj[[2]],
        rho_e1_e4 = iter_Obj[[3]],
        rho_e2_e3 = iter_Obj[[4]],
        rho_e2_e4 = iter_Obj[[5]],
        rho_e3_e4 = iter_Obj[[6]],
        var_Ahat = iter_Obj[[7]]
      )   # close creation of results record
      
      results <- rbind(results, results_rec) 
    } 
  }
}

# add variable to results that is delta-stdev of Ahat
# first extract rho=0 results records, then join back onto main results df
rho0_results <- results %>% 
  filter(rho_target == 0.0)  %>%
  mutate(rho0_var_Ahat = var_Ahat) %>%
  select(k,A,rho0_var_Ahat)
results_jn <- results %>% left_join(rho0_results, by=join_by(k,A))
results_final <- results_jn %>%
  mutate(delta_stdev_Ahat = (sqrt(var_Ahat) - sqrt(rho0_var_Ahat))/
                               sqrt(rho0_var_Ahat))
         

###############################################################################
#  Create panel figure 
###############################################################################
pFig <- ggplot(data=results_final,
               mapping = aes(x=A, y=delta_stdev_Ahat)) +
        geom_point() +
        facet_grid(rows=vars(k), cols=vars(rho_target)) +
        theme_bw()

# panel fig doesn't not work visually.  Make alternate
A_filt_value <- 5
results_final_Afilt <- results_final %>% 
     filter(A == A_filt_value) %>%
     mutate(rho_target_fct = factor(rho_target, 
                                    levels=c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3)),
            k_fct = factor(k,levels=c(0.1, 0.2, 0.3)))
pFig2 <- ggplot(data=results_final_Afilt,
                mapping=aes(x=rho_target_fct, delta_stdev_Ahat, fill=k_fct)) +
         geom_col(position="dodge", color="black", width=0.7) +
         theme_bw()
                  



# get end time
end_time <- Sys.time()
elapsed_time <- end_time - start_time
print(elapsed_time)
