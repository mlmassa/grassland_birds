# This generates covariates and detection for SITExYEAR setup,
# for use in a pseudoreplicate 'hack' of the static occu model.
# It produces umf.rds, which is all covs needed to build umf.
# Current as of 2023-01-18

# SETUP -------------------------------------------------------------------

library(unmarked)
library(tidyverse)

# READ IN DATA ------------------------------------------------------------

# Import point count data:
read_rds('data/processed/birds.rds') |> 
  list2env(.GlobalEnv)

# Generate untidy dataset:
birds <-
  counts |> 
  left_join(visits, by = 'visit_id') |> 
  left_join(points |> select(grts, park), by = 'grts')

# Generate detection table ------------------------------------------------

# Create annual point visit table
annual_points <-
  visits |> 
  # Get unique points for each year
  group_by(year) |>
  select(grts, year) |> 
  distinct() |> 
  # Mark these as visited
  mutate(visited = 1) |> 
  ungroup() |> 
  # Get full table of points per year
  complete(year, nesting(grts), fill = list(visited = 0)) |> 
  arrange(grts, year)

# Generate detection history
detections <-
  birds |> 
    # No flyover detections: birds in habitat only
    filter(flyover == 0) |> 
    # For each species at each pointxyearxvisit...
    group_by(grts, year, visit, species) |> 
    # Get abundance
    summarize(abund = n(), .groups = 'drop') |> 
    # Add empty combinations not present
    right_join(
      birds |> expand(grts, year, visit, species),
      by = c('grts', 'year', 'visit', 'species')) |> 
    # Add whether pointxyear combo was visited
    left_join(
      annual_points, by = c('year', 'grts')) |>
    # Add detection (1/0 if visited, keep NA if not)
    mutate(
      det = case_when(
        is.na(abund) & visited == 1 ~ 0,
        abund > 0 ~ 1)) |> 
    # Retain only focal species, visit 1-2
    filter(
      species %in% c('GRSP', 'EAME'),
      visit == 1 | visit == 2) |> 
    # Create combined grtsxyear variable (to serve as site)
    mutate(grts_year = str_c(grts, '_', year - 2013)) |> 
    # Cut irrelevant variables
    select(grts_year, grts, year, visit, species, det) |> 
    # Make wide data with column for each year-visit detection
    # detection is "y"
    pivot_wider(
      names_prefix = 'y_', 
      names_from = c(visit),
      values_from = det,
      names_sep = '_') |> 
    arrange(grts, year) 

# Let's make abundance just in case we want it
# Not most efficient way to write this, repeats code above
abundance <-
  birds |> 
    # No flyover detections: birds in habitat only
    filter(flyover == 0) |> 
    # For each species at each pointxyearxvisit...
    group_by(grts, year, visit, species) |> 
    # Get abundance
    summarize(abund = n(), .groups = 'drop') |> 
    # Add empty combinations not present
    right_join(
      birds |> expand(grts, year, visit, species),
      by = c('grts', 'year', 'visit', 'species')) |> 
    # Add whether pointxyear combo was visited
    left_join(
      annual_points, by = c('year', 'grts')) |>
    # Add detection (1/0 if visited, keep NA if not)
    mutate(
      det = case_when(
        is.na(abund) & visited == 1 ~ 0,
        abund > 0 ~ abund)) |> 
    # Retain only focal species, visit 1-2
    filter(
      species %in% c('GRSP', 'EAME'),
      visit == 1 | visit == 2) |> 
    # Create combined grtsxyear variable (to serve as site)
    mutate(grts_year = str_c(grts, '_', year - 2013)) |> 
    # Cut irrelevant variables
    select(grts_year, grts, year, visit, species, det) |> 
    # Make wide data with column for each year-visit detection
    # detection is "y"
    pivot_wider(
      names_prefix = 'y_', 
      names_from = c(visit),
      values_from = det,
      names_sep = '_') |> 
    arrange(grts, year) 

# Generate observation covariates -----------------------------------------

# Format:
# siteCovs: single 1,694 (n_sitexyear) row x n_covariates column DF 
# obsCovs: list of 1,694 (n_sitexyear) row x 2 column DFs

obs_covs <-
  list(
    ## Observer ----
    observer = 
      visits |> 
        filter(visit == 1 | visit == 2) |> 
        select(grts, visit, year, observer) |> 
        mutate(observer = as.factor(observer)) |> 
        pivot_wider(
          names_prefix = 'observer_',
          names_from = c(visit),
          values_from = observer,
          names_sep = '_') |> 
      # Standardize formatting
      arrange(grts, year) |> 
      mutate(year = year - 2013) |> 
      unite('grts_year', c(grts, year)) |> 
      full_join(
        detections |> select(grts_year) |> unique()) |> 
      column_to_rownames(var = 'grts_year'),
  
    ## Date of survey ----
    doy =
      visits |> 
        filter(visit == 1 | visit == 2) |> 
        mutate(
          doy = 
            lubridate::ymd(date) |> 
            lubridate::yday()) |> 
        select(grts, visit, year, doy) |>
        # SCALE THE VARIABLE
        # mutate(doy = scale(doy)) |> 
        pivot_wider(
          names_prefix = 'doy_',
          names_from = c(visit),
          values_from = doy,
          names_sep = '_') |> 
      # Standardize formatting
      arrange(grts, year) |> 
      mutate(year = year - 2013) |> 
      unite('grts_year', c(grts, year)) |>
      full_join(
        detections |> select(grts_year) |> unique()) |> 
      column_to_rownames(var = 'grts_year'),
      
  ## Disturbance (factor) ----
  disturbance =
    visits |>
      filter(visit == 1 | visit == 2) |>
      select(grts, visit, year, disturbance) |>
      # Factorize the variable
      mutate(disturbance = as.factor(disturbance)) |>
      pivot_wider(
        names_prefix = 'disturb_',
        names_from = c(visit),
        values_from = disturbance,
        names_sep = '_') |>
      # Standardize formatting
      arrange(grts, year) |>
      mutate(year = year - 2013) |>
      unite('grts_year', c(grts, year)) |>
      full_join(
        detections |> select(grts_year) |> unique()) |>
      column_to_rownames(var = 'grts_year'),

  ## Wind speed (factor) ----
  wind = 
    visits |> 
      filter(visit == 1 | visit == 2) |> 
      select(grts, visit, year, wind) |> 
      # Factorize the variable
      mutate(wind = as.factor(wind)) |> 
      pivot_wider(
        names_prefix = 'wind_',
        names_from = c(visit),
        values_from = wind,
        names_sep = '_') |> 
      # Standardize formatting
      arrange(grts, year) |> 
      mutate(year = year - 2013) |> 
      unite('grts_year', c(grts, year)) |> 
      full_join(
        detections |> select(grts_year) |> unique()) |> 
      column_to_rownames(var = 'grts_year'),
  
  ## Temperature (scaled) ----
  temperature =
    visits |> 
      filter(visit == 1 | visit == 2) |> 
      select(grts, visit, year, temperature) |> 
      # SCALE VARIABLE
      # mutate(temperature = scale(temperature)) |> 
      pivot_wider(
        names_prefix = 'temp',
        names_from = c(visit),
        values_from = temperature,
        names_sep = '_') |>  
      # Standardize formatting
      arrange(grts, year) |> 
      mutate(year = year - 2013) |> 
      unite('grts_year', c(grts, year)) |> 
      full_join(
        detections |> select(grts_year) |> unique()) |> 
      column_to_rownames(var = 'grts_year'),
  
  ## Start time (minutes since sunrise, scaled) ----
  time =
    visits |> 
      filter(visit == 1 | visit == 2) |> 
      select(grts, visit, year, start_sun) |> 
      # SCALE VARIABLE
      # mutate(start_sun = scale(start_sun)) |> 
      pivot_wider(
        names_prefix = 'time',
        names_from = c(visit),
        values_from = start_sun,
        names_sep = '_') |> 
      # Standardize formatting
      arrange(grts, year) |> 
      mutate(year = year - 2013) |> 
      unite('grts_year', c(grts, year)) |> 
      full_join(
        detections |> select(grts_year) |> unique()) |> 
      column_to_rownames(var = 'grts_year')
  )


# Generate sitexyear covariates -------------------------------------------

# Format:
# siteCovs: single 1,694 (n_sitexyear) row x n_covariates column DF 
# About to create this
# obsCovs: list of 1,694 (n_sitexyear) row x 2 column DFs
sapply(obs_covs, dim)


siteyear_covs <-
  
  detections |> select(grts_year, grts, year) |> distinct() |> 
  
  # Annual covariates
  
  # Burned (Y/N)
  left_join(
    read_rds('data/processed/annual_covs.rds') |> 
      mutate(
        ever_burned = if_else(is.na(t_since_burn), 0, 1) |> 
        factor(levels = c(0, 1))) |> 
      select(grts, year, ever_burned),
    by = c('grts', 'year')) |> 
  
  # Burn class (0, 1-2, 3+, NA)
  left_join(
    read_rds('data/processed/annual_covs.rds') |> 
      mutate(
        burn_class = 
          case_when(
            t_since_burn %in% c(0, 2) ~ "0-2",
            t_since_burn >= 3 ~ "3+") |> 
          factor(levels = c("3+", "0-2"))) |> 
      select(grts, year, burn_class),
    by = c('grts', 'year')) |> 

  # In harvest limit (Y/N)
  left_join(
    read_rds('data/processed/annual_covs.rds') |> 
      mutate(
        year = as.numeric(year),
        harvest_limit = as.factor(harvest_limit),
        harvest_day = factor(harvest_day, levels = c(227, 197, 196, 182))),
    by = c('grts', 'year')) |> 

  # True static covariates    
  left_join(
    read_rds('data/processed/static_covs.rds') |> 
      # factorize variables of interest
      mutate(
        shrub = shrub/100,
        park = as.factor(park),
        habitat = factor(habitat,
          levels = c("unleased", "WSG unleased", "crop", "CSG pasture", "CSG hayfield", "WSG hayfield")),
        field_type = factor(habitat_type, 
          levels = c("unleased", "crop", "pasture", "hayfield")),
        grass_type = factor(grass_type, levels = c("CSG", "WSG")),
        ever_burned = factor(ever_burned, levels = c(0, 1)),
        leased = factor(leased, levels = c(0, 1))
        # Scale all numerics
        # across(c(where(is.numeric), -grts), scale)
        ) |> 
      # Select only variables of interest
      select(-c(ever_burned, trees, harvest_limit, habitat_type)),
    by = 'grts') |> 
  
  mutate(
    year = year - 2013,
    year_fct = as.factor(year)) |> 
  
  # Final formatting
  arrange(grts, year) |> 
  column_to_rownames(var = 'grts_year')

summary(siteyear_covs)
    
# Create UMF --------------------------------------------------------------

umfs <- c('detections', 'abundance', 'siteyear_covs', 'obs_covs')

write_rds(
  mget(umfs),
  'data/processed/umf.rds')

# The actual UMF will be built immediately prior to modeling.
