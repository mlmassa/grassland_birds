# This script generates a table of covariates, their means etc., divided
# by the level of modeling they influence.


# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Import point count data:

read_rds('data/processed/birds.rds') %>% 
  list2env(.GlobalEnv)


# Generate untidy dataset:

birds <-
  counts %>% 
  left_join(visits, by = 'visit_id') %>% 
  left_join(points %>% select(grts, park), by = 'grts')



# Point covariate data ----------------------------------------------------


read_rds('data/processed/static_point_covs2.rds') %>% 
mutate(
  grts = as.numeric(grts),
  leased = as.factor(leased),
  harvest_limit = as.factor(harvest_limit)) %>% 
select(where(is.numeric)) %>% 
pivot_longer(
  cols = 2:18,
  names_to = 'variable') %>% 
  group_by(variable) %>% 
  summarize(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    se = sd/sqrt(length(value)))



# Detection covariates ----------------------------------------------------

visits %>% 
  filter(visit == 1 | visit == 2) %>% 
  mutate(
    doy = 
      lubridate::ymd(date) %>% 
      lubridate::yday(),
    disturbance = as.factor(disturbance),
    wind = as.factor(wind),
    year = as.factor(year),
    observer = as.factor(observer)) %>% 
  select(visit_id, temperature, doy, start_sun) %>%
pivot_longer(
  cols = 2:4,
  names_to = 'variable') %>% 
group_by(variable) %>% 
summarize(
  mean = mean(value),
  se = sd(value)/sqrt(length(value)))
    

# Generate yearly site covs -----------------------------------------------

read_rds('data/processed/annual_burn_covs.rds') %>% 
  select(-t_since_burn) %>% 
  mutate(as.factor(burn_class)) %>% 
  # Only data thru 2019 (expand when I get new data)
  filter(year < 2020) %>% 
  summary()


read_rds('data/processed/annual_harvest_covs.rds') %>% 
  # Only data thru 2019 (expand when I get new data)
  filter(year < 2020) %>% 
  # Scale the variable
  mutate(harvest_day = scale(harvest_day)) %>% 
  complete(grts, year) %>% 
  pivot_wider(
    names_prefix = 'harvest_doy_',
    names_from = year,
    values_from = harvest_day,
    names_sep = '_') %>% 
  right_join(points %>% select(grts), by = 'grts') %>% 
  column_to_rownames(var = 'grts')
    
    
