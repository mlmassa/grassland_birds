# This script must be run AFTER 'data/scripts/wrangling/spatial_covariates.R'
# As this script builts upon the static_point_covs.rds
# Products: static_point_covs2, annual_harvest_covs

# setup -------------------------------------------------------------------

# Load required packages
library(tidyverse)
library(lubridate)

# Import data: previous work on points
nps_points <- 
  read_rds('data/processed/static_point_covs.rds') %>% 
  select(-geometry)

# Import data: veg data
veg <- 
  read_csv('data/raw/FinalVegData.csv') %>% 
  mutate(GRTS_Order = as.character(GRTS_Order),
         # Fix clinometer readings: topo slope -> degrees
         across(starts_with('Clin'), ~ atan(. / 66) * 180 / pi))

# Import data: harvest data
harvest <-
  read_csv('data/raw/nps_harvest.csv') %>% 
  left_join(
    read_csv('data/raw/nps_harvest.csv') %>% 
    filter(leased == 1) %>% 
    select(grts, newlimit = harvest_limit),
    by = 'grts') %>% 
  select(grts, leased, harvest_date, harvest_limit = newlimit)

# Separate point data from visit data -------------------------------------

# Point data: permanent veg info
nps_point_covs <- 
  nps_points %>% 
    left_join(
      veg %>% 
        mutate(
          shrub_mean = 
            rowMeans(select(veg, c(Shrub_N:Shrub_W)), 
                     na.rm = TRUE),
          clin_mean = 
            rowMeans(select(veg, c(Clin_N:Clin_W)), 
                     na.rm = TRUE)) %>% 
        select(
          grts = GRTS_Order,
          trees = Trees,
          shrub_mean,
          clin_mean,
          clin_max = Clin_Max),
      by = 'grts')

# Get all veg data by visit
visit_veg <-
  veg %>% 
    select(
      grts = GRTS_Order, 
      date = Date_1,
      height = Height_1,
      type = Type_1,
      observer = Observer_1) %>% 
  mutate(visit = 1) %>% 
  bind_rows(
    veg %>% 
      select(
        grts = GRTS_Order,
        date = Date_2,
        height = Height_2,
        type = Type_2,
        observer = Observer_2) %>% 
      mutate(visit = 2)) 

# Get only veg height (detection and habitat cov)
veg_height <-
  visit_veg %>% 
  select(grts,
         date,
         height,
         visit)


# Get field type ----------------------------------------------------------


# Get only field type; reassign mismatch
field_type <-
  veg %>% 
  select(grts = GRTS_Order,
         Type_1,
         Type_2) %>% 
  mutate(
    field_type =
      case_when(
        # Match? No problem
        Type_1 == Type_2 ~ Type_1,
        # If ever row crop, it's row crop
        Type_1 != Type_2 & Type_1 == 'Row Crop' | Type_2 == 'Row_Crop' ~ 'Row Crop',
        # From visual inspection/ag lease maps
        grts %in% c('3844', '68', '1555', '4428') ~ 'Meadow',
        # Notes on points to eliminate
        grts %in% c('1146', '1254', '2932', '2994', '3359') ~ 'Woods',
        grts == '684' ~ 'Hayfield',
        # All other mismatches are hay
        TRUE ~ 'Hayfield')) %>% 
  select(grts, field_type)


# Get annual harvest limit ------------------------------------------------

read_rds('data/processed/birds.rds') %>% 
  list2env(.GlobalEnv)

rm(counts, points)


# Get first year surveyed
first_survey <- 
  visits %>% 
    select(grts, year) %>% 
    group_by(grts) %>% 
    summarize(first_survey = min(year))


# Check if limits placed before or after surveys started
# 15 after (hmm...), 40 before (good)
harvest %>% 
  left_join(first_survey, by = 'grts') %>% 
  filter(!is.na(harvest_date),
         year(mdy(harvest_date)) > first_survey)


# Get annual harvest date. This should be a for loop or map...
harv1 <-
  harvest %>% 
  arrange(grts) %>% 
    mutate(
      first_limit = year(mdy(harvest_date)),
      ever_limit = harvest_limit,
      harvest_date = yday(mdy(harvest_date))) 
  
crossing(grts = as.double(nps_points$grts), year = c(2014:2021)) %>% 
  left_join(harv1) %>% 
  mutate(
    harvest_limit = 
      case_when(leased == 1 & first_limit <= year ~ 1,
                ever_limit == 0 & leased == 1 ~ 0,
                leased == 0 ~ NA_real_),
    harvest_day = if_else(harvest_limit == 0, NA_real_, harvest_date)) %>% 
  select(grts, year, harvest_limit, harvest_day) %>% 
  write_rds('data/processed/annual_harvest_covs.rds')

# STORE MODIFIED STATIC COVS, completed!!!
nps_point_covs %>% 
  left_join(
    field_type, by = 'grts') %>% 
  left_join(
    harvest %>% 
      select(-harvest_date) %>% 
      mutate(grts = as.character(grts)),
    by = 'grts') %>% 
  write_rds('data/processed/static_point_covs2.rds')


