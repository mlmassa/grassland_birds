# This requires the saved models generated in occu_modeling.R


# Setup -------------------------------------------------------------------

library(tidyverse)
library(unmarked)


# Data import -------------------------------------------------------------


# sp <- 'RWBL'
# sp <- 'EAME'
sp <- 'GRSP'
# sp <- 'FISP'

read_rds(paste0('output/all_fitted_models_', sp, '.rds')) %>% 
  list2env(.GlobalEnv)

read_rds('data/processed/umf.rds') %>% 
  list2env(.GlobalEnv)

umf <- umf_GRSP


 # Goodness of fit testing -------------------------------------------------

# Randomly draw a sample of n sites, where n is the sample size, 
# WITH replacement so sites can occur many times or never.
# Fit the model of interest to this data and save the estimates.
# Repeat many times (100, 1000?) and take the sample SD of parameters and other estimates

# Create an array to hold coefficient estimates
simrep <- 10
estimates <-
  array(NA, dim = c(length(coef(m.0)), simrep))
rownames(estimates) <- names(coef(m.0))
nyears <- 7 

site_sample <-
  sample(1:242, 242, replace = TRUE)

y_sample <- NULL
obsCovs_sample <- NULL
siteCovs_sample <- NULL

for(t in 1:nyears) {
  # detections for year T of subsample
  y_sample <- 
    rbind(y_sample, umf@y[7*site_sample-t,])
  obsCovs_sample <-
    rbind(obsCovs_sample, umf@obsCovs$disturbance[7*site_sample-t,])
}


