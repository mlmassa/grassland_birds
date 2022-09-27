# This script imports all available static point data and maps it.

# setup -------------------------------------------------------------------

library(sf)
library(tmap)
library(tidyverse)

# Import points with covs
point_covs <-
  read_rds('data/processed/static_point_covs2.rds') %>% 
  st_as_sf()


# Map points TM -----------------------------------------------------------

tmap_mode('view')

# FIELD TYPE
tm_basemap(
  c('OpenStreetMap',
    'Esri.WorldImagery')) +
  
  tm_shape(point_covs) +
  tm_dots(
    size = 0.1,
    col = 'field_type',
    palette = 
      c('#c2b200',
        '#578012',
        '#cf7602',
        '#000000'),
    popup.vars = 
      c('Park', 
        'grts', 
        'for_1km', 
        'grs_1km',
        'dvp_1km',
        'trees',
        'shrub_mean',
        'clin_max',
        'field_type',
        'ever_burned'),
    clustering = FALSE)


# HARVEST LEASING?
tm_basemap(
  c('OpenStreetMap',
    'Esri.WorldImagery')) +
  
  tm_shape(point_covs) +
  tm_dots(
    size = 0.1,
    col = as.factor('leased'),
    popup.vars = 
      c('leased',
        'harvest_limit'),
    clustering = FALSE)

