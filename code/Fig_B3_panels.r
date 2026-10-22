###############################################################################
#
#  Simulations and creation of Figure B-3 .  
#  Panels showing Stdev (Ahat) as function of A:
#    x dimension panels:  different levels of correlation
#    y dimension panels:  different levels of k
#
#  October 2026
#
###############################################################################


###########  set parameters ###############

Avals <- c(0.1,seq(0.5,10, by=0.5))

rho <- c(-0.6, -0.3, -0.1, 0.1, 0.3, 0.6)
k <- c(0.1, 0.2, 0.3)


# loop through parameter values for k and A
for (ik in k) {
  for (iA in Avals) {
   
    # run iterations for baseline rho=0 
    iter_Obj <- run_iterations(k=ik, A=iA, rho=0, Nobs, Niter)
    
  
    
    
  }
}


# loop through rho
# loop through k
# loop through A
# iteration loop

