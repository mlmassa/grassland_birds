
# SETUP -------------------------------------------------------------------

library(unmarked)
library(tidyverse)
library(AICcmodavg)

# READ IN DATA ------------------------------------------------------------


# Import point count data:

read_rds('data/processed/birds.rds') %>% 
  list2env(.GlobalEnv)


# Generate untidy dataset:

birds <-
  counts %>% 
  left_join(visits, by = 'visit_id') %>% 
  left_join(points %>% select(grts, park), by = 'grts')


# Import covariate data:

point_covs <- 
  read_rds('data/processed/static_point_covs2.rds') %>% 
  mutate(
    grs_1km = scale(grs_1km),
    grs_5km = scale(grs_5km),
    dvp_1km = scale(dvp_1km),
    dvp_5km = scale(dvp_5km),
    shrub_mean = scale(shrub_mean),
    clin_max = scale(clin_max),
    leased = as.factor(leased),
    harvest_limit = as.factor(harvest_limit)) %>% 
  filter(grts %in% visits[visits$year == 2016, ]$grts) %>% 
  select(
    park = Park,
    grs_1km,
    grs_5km,
    dvp_1km,
    dvp_5km,
    shrub_mean,
    clin_max,
    field_type,
    leased,
    harvest_limit) %>% 
  as.data.frame()


# Generate detection table ------------------------------------------------


# Create annual point visit table

annual_points <-
  visits %>% 
  # Get unique points for each year
  group_by(year) %>%
  select(grts, year) %>% 
  distinct() %>% 
  # Mark these as visited
  mutate(visited = 1) %>% 
  ungroup() %>% 
  # Get full table of points per year
  complete(year, nesting(grts), fill = list(visited = 0)) %>% 
  arrange(grts, year)


# Generate detection history
  
detections <-
  birds %>% 
  
    # No flyover detections: birds in habitat only
    filter(
      flyover == 0#,
    # Birds only within count circle
    # distance_band == 
      ) %>% 
  
    # For each species at each pointxyearxvisit...
    group_by(grts, year, visit, species) %>% 
  
    # Get abundance
    summarize(abund = sum(bird_count), .groups = 'drop') %>% 
  
    # Add empty combinations not present
    right_join(
      birds %>% expand(grts, year, visit, species),
      by = c('grts', 'year', 'visit', 'species')) %>% 
  
    # Add whether pointxyear combo was visited
    left_join(
      annual_points, by = c('year', 'grts')) %>%
  
    # Add detection (1/0 if visited, keep NA if not)
    mutate(
      det = case_when(
              is.na(abund) & visited == 1 ~ 0,
              abund > 0 ~ 1)) %>% 
  
    # Retain only focal species, visit 1-2
    filter(
      species %in% c('GRSP', 'FISP', 'EAME', 'RWBL'),
      visit == 1 | visit == 2,
      # ONLY FOR THIS EXAMPLE:
      year == 2016) %>% 
  
    # Cut irrelevant variables
    select(grts, year, visit, species, det) %>% 
  
    # Make wide data with column for each year-visit detection
    # detection is "y"
    pivot_wider(
      names_prefix = 'y_', 
      names_from = c(year, visit),
      values_from = det,
      names_sep = '_') 


# Species detection tables ------------------------------------------------

y_GRSP <-
  detections %>% 
  filter(species == 'GRSP',
         grts %in% visits[visits$year == 2016, ]$grts) %>% 
  select(-grts, -species)
  

# Generate obsCovs (visit-level) ------------------------------------------

obs_covs <-
  
  list(
    
    # Observer name
    observer =
      visits %>% 
        filter(visit == 1 | visit == 2,
               # ONLY FOR THIS EXAMPLE
               year == 2016
               ) %>% 
        select(grts, visit, year, observer) %>% 
        pivot_wider(
          names_prefix = 'observer_',
          names_from = c(year, visit),
          values_from = observer,
          names_sep = '_') %>% 
        select(-grts) %>% 
      as.data.frame(),
    
    # Date of survey
    doy = 
      visits %>% 
        filter(visit == 1 | visit == 2,
               # ONLY FOR THIS EXAMPLE
               year == 2016
               ) %>% 
        mutate(
          doy = 
            lubridate::ymd(date) %>% 
            lubridate::yday()) %>% 
        select(grts, visit, year, doy) %>%
        # SCALE THE VARIABLE
        mutate(doy = scale(doy)) %>% 
        pivot_wider(
          names_prefix = 'doy_',
          names_from = c(year, visit),
          values_from = doy,
          names_sep = '_') %>% 
      select(-grts) %>% 
      as.data.frame(),
    
    # Disturbance
    disturbance =
      visits %>% 
        filter(visit == 1 | visit == 2,
               # ONLY FOR THIS EXAMPLE
               year == 2016
               ) %>% 
        select(grts, visit, year, disturbance) %>% 
        # SCALE THE VARIABLE: Is this appropriate?
        mutate(disturbance = scale(disturbance)) %>% 
        pivot_wider(
          names_prefix = 'disturb_',
          names_from = c(year, visit),
          values_from = disturbance,
          names_sep = '_') %>% 
        select(-grts) %>% 
      as.data.frame(),
    
    # Wind speed
    wind = 
      visits %>% 
        filter(visit == 1 | visit == 2,
               # ONLY FOR THIS EXAMPLE
               year == 2016
               ) %>% 
        select(grts, visit, year, wind) %>% 
        # SCALE VARIABLE: Is this appropriate?
        mutate(wind = scale(wind)) %>% 
        pivot_wider(
          names_prefix = 'wind_',
          names_from = c(year, visit),
          values_from = wind,
          names_sep = '_') %>% 
      select(-grts) %>% 
      as.data.frame())

# Create UMF --------------------------------------------------------------

# Multi-year counts will require unmarkedMultiFrame.
# Here I use unmarkedFrameOccu as a tester.

umf <-
  unmarkedFrameOccu(
    y = y_GRSP,
    siteCovs = point_covs,
    obsCovs = obs_covs)

summary(umf)

# Null model --------------------------------------------------------------

psi_p <-
  occu(formula = 
         # p constant
         ~1 
         # psi constant
         ~1, 
       data = umf)

# Back-transformations of psi and p
backTransform(psi_p, type = 'state')
backTransform(psi_p, type = 'det')

# Model 1 -----------------------------------------------------------------

psi.1_p <-
  occu(formula = 
         # p constant
         ~1 
         # psi: clinometer, park, grass near, develop far
         ~clin_max + park + grs_1km + dvp_5km, 
       data = umf)


# Model 2 -----------------------------------------------------------------

psi.all_p.all <-
  occu(formula = 
         # p all
         ~observer + doy + wind + disturbance
         # psi all
         ~park + grs_1km + grs_5km + dvp_1km + dvp_5km + shrub_mean + clin_max + field_type + leased + harvest_limit,
       data = umf)

psi.all_p.all
