# This script maps the approximate (blurred) location of points,
# along with the average abundance of birds at that point.
# Much of this code is copied from scripts/analysis/basic_summaries.R


# Setup -------------------------------------------------------------------

library(sf)
library(tidyverse)
# library(raster)
library(terra)
library(tmap)
library(grid)


# Data import -------------------------------------------------------------


# Import bird data
read_rds('data/processed/birds.rds') %>% 
  list2env(.GlobalEnv)


# Build untidy bird data
birds <-
  counts %>% 
  left_join(visits) %>% 
  left_join(points)


# Import park shapes
parks <-
  st_read('data/processed/focal_parks.shp') %>% 
  select(
    park = UNIT_CODE,
    park_name = UNIT_NAME) %>% 
  # Convert to NAD83 / UTM zone 18N
  st_transform(crs = 26918)

# Import NAIP imagery
naip <-
  raster::raster(
    'data/raw/MD_NAIP_imagery.png',
    # proj4string
    crs = '+proj=merc +lon_0=0 
          +k=1 +x_0=0 +y_0=0 
          +a=6378137 +b=6378137 
          +towgs84=0,0,0,0,0,0,0 
          +units=m +no_defs') %>% 
      # Convert to NAD83 / UTM zone 18N (EPSG 26918)
    raster::projectRaster(
      crs = '+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')


# Set focal species
focal_species <-
  c('GRSP', 'FISP', 'RWBL', 'EAME')


# Calculate annual totals -------------------------------------------------

# Annual totals for all species in all parks:
point_abund <-
  birds %>% 
    # No flyover detections: birds in habitat only
    filter(flyover == 0) %>% 
    group_by(grts, species) %>% 
    # Get total abundance of species seen at each point
    summarize(
      abundance = sum(bird_count),
      .groups = 'drop') %>% 
    # Add species never seen at point with abundance = 0
    complete(
      species, 
      nesting(grts), 
      fill = list(abundance = 0)) %>%
    # Add number of visits:
    left_join(
      visits %>% 
        group_by(grts) %>% 
        summarize(n_visits = length(visit_id)),
      by = 'grts') %>% 
    # Calculate mean abundance/survey:
    mutate(mean_abund = abundance / n_visits) %>% 
    select(grts, species, mean_abund) %>% 
  # I only care about my focal species
  filter(species %in% focal_species) %>% 
  # Add spatial information
  left_join(points, by = 'grts') %>% 
  st_as_sf(
    coords = c('long', 'lat'),
    crs = 4269) %>% 
  # Convert to NAD83 / UTM zone 18N
  st_transform(crs = 26918)

# Rasterize ---------------------------------------------------------------

raster_ANTI_GRSP <-
  point_abund %>% 
    filter(species == 'GRSP', park == 'ANTI') %>% 
    vect() %>% 
    rasterize(
      rast(nlyrs = 1,
         ncols = 12, 
         nrows = 22,
         xmin = 262999.8 - 125,
         xmax = 265749.8 + 125,
         ymin = 4369751  - 125,
         ymax = 4375001  + 125,
         crs = 'EPSG:26918'),
      field = 'mean_abund')

raster_MANA_GRSP <-
  point_abund %>% 
    filter(species == 'GRSP', park == 'MANA') %>% 
    vect() %>% 
    rasterize(
      rast(nlyrs = 1,
         ncols = ((282749.8 - 276749.8)/250) + 1, 
         nrows = ((4302000.9 - 4297750.9)/250) + 1,
         xmin = 276749.8 - 125,
         xmax = 282749.8 + 125,
         ymin = 4297750.9  - 125,
         ymax = 4302000.9   + 125,
         crs = 'EPSG:26918'),
      field = 'mean_abund')

raster_HAFE_GRSP <-
  point_abund %>% 
    filter(species == 'GRSP', park == 'HAFE') %>% 
    vect() %>% 
    rasterize(
      rast(nlyrs = 1,
         ncols = ((261999.8 - 259749.8)/250) + 1, 
         nrows = ((4357000.9 - 4353500.9)/250) + 1,
         xmin = 259749.8 - 125,
         xmax = 261999.8 + 125,
         ymin = 4353500.9 - 125,
         ymax = 4357000.9 + 125,
         crs = 'EPSG:26918'),
      field = 'mean_abund')

raster_MONO_GRSP <-
  point_abund %>% 
    filter(species == 'GRSP', park == 'MONO') %>% 
    vect() %>% 
    rasterize(
      rast(nlyrs = 1,
         ncols = ((294749.8 - 292249.8)/250) + 1, 
         nrows = ((4361250.9  - 4358000.9)/250) + 1,
         xmin = 292249.8 - 125,
         xmax = 294749.8 + 125,
         ymin = 4358000.9 - 125,
         ymax = 4361250.9  + 125,
         crs = 'EPSG:26918'),
      field = 'mean_abund')

   
# State shapefile ---------------------------------------------------------

md <-
  st_read('data/raw/tl_2016_24_cousub.shp') %>% 
    select(COUNTYFP, geometry) #%>% 
    #st_transform(crs = 26918)

# Map ---------------------------------------------------------------------

tmap_options(check.and.fix = TRUE)
tmap_mode('plot')

monomap <-

#tm_basemap('Esri.WorldImagery') +
  
# Park background
tm_shape(filter(parks, park == 'MONO')) +
  tm_fill(col = 'grey80', alpha = 0) +


# Point raster
tm_shape(raster_MONO_GRSP) +
tm_raster(
  title = '',
  palette = 'BuGn',
  breaks = c(0, 0.5, 1, 1.5, 2, 2.5)) +

# Point dots (remove)
# tm_shape(filter(point_abund, species == 'GRSP', park == 'MANA')) +
#   tm_dots() +
  
# Park border
tm_shape(
  filter(
    parks, park == 'MONO'),
    unit = 'm') +  
  tm_borders(col = 'black', alpha = 0.5)


# Mod save ----------------------------------------------------------------


# Get bounding box/aspect ratio
bbox_large <-
  st_bbox(filter(parks, park == 'MONO')) 

asp_large <- 
  (bbox_large$xmax - bbox_large$xmin)/(bbox_large$ymax - bbox_large$ymin)

bbox_inset <-
  st_bbox(md)

asp_inset <- 
  (bbox_inset$xmax - bbox_inset$xmin)/(bbox_inset$ymax - bbox_inset$ymin)

insetmap <-
  tm_shape(md) + 
    tm_fill(col = 'lightgrey') +
  tm_shape(filter(parks, park == 'MONO')) + 
    tm_borders(lw = 4, col = 'forestgreen')

tmap_save(
 monomap, 
 filename = 'output/monoabundgrsp.pdf', 
 dpi = 100, 
 insets_tm = insetmap,
 insets_vp = viewport(x = 0.95, y = 0.098, 
                      width = 0.2, 
                      height = asp_inset*0.2, 
                      just = c('right', 'top')),
 height = asp_large*100, width = 100, units = 'mm'
)
