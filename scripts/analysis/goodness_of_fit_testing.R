# This requires the saved models generated in occu_modeling.R


# Setup -------------------------------------------------------------------

library(tidyverse)
library(unmarked)
source('scripts/wrangling/tidy_method_unmarked.R')

# Data import -------------------------------------------------------------


# sp <- 'RWBL'
# sp <- 'EAME'
sp <- 'GRSP'
# sp <- 'FISP'

read_rds(paste0('output/all_fitted_models_', sp, '.rds')) %>% 
  list2env(.GlobalEnv)

read_rds('data/processed/umf.rds') %>% 
  list2env(.GlobalEnv)


 # Goodness of fit testing -------------------------------------------------

# Randomly draw a sample of n sites, where n is the sample size, 
# WITH replacement so sites can occur many times or never.
# Fit the model of interest to this data and save the estimates.
# Repeat many times (100, 1000?) and take the sample SD of parameters and other estimates

# Create an array to hold coefficient estimates
simrep <- 10
nyears <- 7 

# Create an array to hold coefficient estimates
estimates <-
  array(NA, dim = c(length(coef(best_full)), simrep))

rownames(estimates) <- names(coef(best_full))

estimates

site_sample <-
  sample(unique(detections$grts), 242, replace = TRUE) %>% sort()

## Detection sample ----
y_sample <- 
  bind_cols(grts = site_sample) %>% left_join(detections) %>% 
  filter(species == sp)

nrow(y_sample) == nrow(detections %>% filter(species == sp))

## Obs cov sample ----
combos <- expand.grid(site_sample, c(1:6, 8))

obsCovs_sample <- 
  map(
    1:length(obs_covs),
    function(x) sprintf('%s_%s', combos[,1], combos[,2]) %>% as_tibble_col(column_name = "grts_year") %>% left_join(pluck(obs_covs, x) %>% rownames_to_column(var = "grts_year")))

# NEED TO FIX SORT

names(obsCovs_sample) <- names(obs_covs)

## Site cov sample ----
  
siteCovs_sample <- NULL



for(t in 1:nyears) {
  # detections for year T of subsample
  y_sample <- 
    rbind(y_sample, umf@y[7*site_sample-t,])
  obsCovs_sample <-
    rbind(obsCovs_sample, umf@obsCovs$disturbance[7*site_sample-t,])
}


