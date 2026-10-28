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

source("code/Appendix_demo4_area_panel_iterations.r")


###############################################################################
# set parameters
###############################################################################

Avals <- c(0.1,seq(0.5,10, by=0.5))

# ordered with rho=0 first to serve as baseline
rho <- c(0.0, -0.6, -0.3, -0.1, 0.1, 0.3, 0.6)
k <- c(0.1, 0.2, 0.3)

N <- 10000  # number of iterations for each set of conditions

# initialize results data frame
results <- data.frame(k = numeric(), rho = numeric(), A = numeric(),
                      N = numeric())



###############################################################################
#  Use nested looping to step through all model conditions and run iterations
#  at each step.
###############################################################################

for (ik in k) {
  for (irho in rho) {
    for (iA in Avals) {
      iter_Obj <- run_iterations(ik, irho, iA, N)
    
      # create results record and add to results data frame
      # k,rho,A,var(Ahat), delta_stdev_Ahat
      
    } 
  }
}


# loop through rho
# loop through k
# loop through A
# iteration loop

