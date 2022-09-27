
# setup -------------------------------------------------------------------

library(sf)
library(tidyverse)

# Load NPS park boundaries
st_write(
  st_read('data/raw/nps_boundary.shp') %>% 
    filter(UNIT_CODE %in% c('MANA', 'HAFE', 'ANTI', 'MONO')),
  'data/processed/focal_parks.shp')

