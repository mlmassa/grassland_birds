# This script maps sensory pollution around parks.

# setup -------------------------------------------------------------------

library(sf)
library(tidyverse)
library(tmap)



# raster ------------------------------------------------------------------

alan_raw <- 
  raster::raster('data/raw/alan.tif')

# parks -------------------------------------------------------------------
parks <- 
  st_read('data/processed/focal_parks.shp') 
  

# plot --------------------------------------------------------------------

tmap_options(check.and.fix = TRUE)
tmap_mode('plot')

tm_shape(alan_raw) +
  tm_rgb() +
tm_shape(parks) +
  tm_borders(col = '#5ba803') +
  tm_fill(col = '#97d64f',
          alpha = 0.8) +
  tm_legend(show = FALSE) +
tm_scale_bar()

