###############################################################################
# Configuration file for demo 1 showing propagation of error for a model
# of error on kelp area estimtaes.
#
# September 2026
###############################################################################

A <- 10    # true kelp area value in hectares

# parameters for classification error distribution modeled as a normal distr.
# Not sure how likely normal fits here as a mash-up of the occurrences of missed
# kelp with weak signal due to shallow submersion combined with the occurrences
# an inflated kelp area due to inclusion of shallow macroalgae.
eclass_mean <- 0
eclass_stdev <- 1

# parameters for tidal error distribution modelled as a lognormal distribution.
# By convention, these mean & stdev are of the parent normal distribution, 
# i.e. they are the mean & stdev of the distribution of log(error)
etide_mean <- 0
etide_stdev <- 0.5

# parameters for seasonal error distribution modelled as a normal distribution
eseas_mean <- 0
eseas_stdev <- 0.5

# parameters for optical error distriubiton modelled as a lognormals distrib.
# These are mean & stdev of the parent normal distribution
eopt_mean <- 0
eopt_stdev <- 1