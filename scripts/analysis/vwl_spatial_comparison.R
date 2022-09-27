
# Results -----------------------------------------------------------------

# ANTI: 27, 217, 134
# HAFE: 169, 134, 37
# MANA: 68, 8, 33
# MONO: 169, 101, 30

# setup -------------------------------------------------------------------

library(sf)
library(sp)
library(tmap)
library(tidyverse)


# Import NPS point covariates
nps_points <-
  read_rds('data/processed/point_covs.rds')

nps_landscape <-
  nps_points %>% 
    group_by(Park) %>% 
    summarize(for_1km_mean = mean(for_1km),
              grs_1km_mean = mean(grs_1km),
              for_5km_mean = mean(for_5km),
              grs_5km_mean = mean(grs_5km))

# Display data

nps_landscape %>% 
  pivot_longer(2:5, names_to = 'landscape') %>% 
ggplot(aes(x=landscape, y=value, fill=Park)) +
  geom_col(position = 'dodge') +
    labs(
    x = 'Landscape metric',
    y = 'Proportion') +
  # rename x elements
  scale_x_discrete(labels = c('Forest (1km)', 'Forest (5km)', 'Grassland (1km)', 'Grassland (5km)')) +
  # Theme elements
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.background = element_rect(fill = 'white'),
        panel.grid = element_line(color = "#e9e9e9"),
        panel.grid.minor = element_line(linetype = 'dashed'),
        axis.line = element_line(color = 'black'),
        strip.background = element_rect(color = 'black', fill = 'black'),
        strip.text = element_text(size = 12, color = 'white')) 



# Read in points and shapes -----------------------------------------------

# NAD 83 / UTM 18N: EPSG 26918 (shapefiles)
# CONVERT ALL TO: WGS 84 / AEA: EPSG 9822 (raster) 
  # '+proj=aea +datum=WGS84 +units=m'

# Read in NLCD (WGS84 / AEA)

nlcd_raw <-
  raster::raster('data/raw/NLCD_VWL.tiff')


# Read in VWL points

vwl_points <-
  read_csv('data/raw/vwl_sites.csv') %>% 
  select(property_id, long, lat) %>% 
  filter(!is.na(long) & !is.na(lat)) %>% 
  st_as_sf(coords = 2:3, crs = 4326) %>% 
  st_transform(crs = raster::projection(nlcd_raw))


# Look at the data --------------------------------------------------------

parks <- 
    st_read('data/raw/nps_boundary.shp') %>% 
    st_transform(
      crs = st_crs(vwl_points)) %>% 
    filter(
      UNIT_CODE %in% c('ANTI', 'HAFE', 'MONO', 'MANA')) %>% 
    select(
      park = UNIT_CODE, geometry)

tmap_mode('view')
tmap_options(check.and.fix = TRUE)

tm_basemap(
  alpha = 0.5,
  c('OpenStreetMap',
    'Esri.WorldImagery', 
    'Esri.WorldTopoMap')) +
  
  # Proxy points
  tm_shape(
    vwl_points %>% 
      mutate(
        proxy_park =
          case_when(
            property_id %in% c(27, 217, 134) ~ 'ANTI',
            property_id %in% c(169, 134, 37) ~ 'HAFE',
            property_id %in% c(68, 8, 33) ~ 'MANA',
            property_id %in% c(169, 101, 30) ~ 'MONO')) %>% 
      filter(
        property_id %in% c(27, 217, 134, 169, 134, 37, 68, 8, 33, 101, 30))) +
    tm_dots(size = 0.2, clustering = FALSE, col = 'black') +
  
  # Parks
  tm_shape(parks) +
    tm_polygons(col = 'firebrick', border.col = NA) +
    tm_text(
      text = 'park',
      col = 'black',
      size = 1.5,
      auto.placement = FALSE,
      just = c('left', 'top'))
  


# Buffer and reclass NLCD ------------------------------------------------


# Create point buffer

point_buff <-
  vwl_points %>% 
  st_buffer(dist = 50001)


# Crop and mask raster to buffer

nlcd_mask <-
  nlcd_raw %>% 
  raster::crop(., point_buff) %>% 
  raster::mask(., point_buff)


# Clean up

rm(nlcd_raw)
rm(point_buff)
gc()


# Check that it looks right

raster::plot(nlcd_mask)


# Check raster values are right

nlcd_mask %>% 
raster::values() %>% 
unique()


# Reclassify to 1 forest/0 not forest
    
forest <-
  raster::reclassify(
    nlcd_mask,
    # Reclass matrix
    tibble(
      from = 
          nlcd_mask %>% 
          raster::values() %>% 
          unique()) %>% 
      # From NLCD legend, forest is 41, 42, 43
      mutate(
        to = if_else(from %in% 41:43, 1, 0)) %>% 
      as.matrix())


# Reclassify to 1 grassland/0 not grassland
    
grassland <-
  raster::reclassify(
    nlcd_mask,
    # Reclass matrix
    tibble(
      from = 
          nlcd_mask %>% 
          raster::values() %>% 
          unique()) %>% 
      # From NLCD legend, Johnson classed grass, shrub, pasture (52, 71, 81)
      mutate(
        to = if_else(from %in% c(52, 71, 81), 1, 0)) %>% 
    as.matrix())

# Calculate forest covariates ---------------------------------------------

# Proportion forest within 1km

for_1km <-
  raster::extract(
    forest,
    vwl_points,
    buffer = 1000,
    fun = mean,
    na.rm = TRUE,
    sp = TRUE) %>% 
  as_tibble() %>% 
  select(property_id, for_1km = NLCD_VWL)

# Proportion grassland within 1km

grs_1km <-
  raster::extract(
    grassland,
    vwl_points,
    buffer = 1000,
    fun = mean,
    na.rm = TRUE,
    sp = TRUE) %>% 
  as_tibble() %>% 
  select(property_id, grs_1km = NLCD_VWL)


# Proportion forest within 5km

for_5km <-
  raster::extract(
    forest,
    vwl_points,
    buffer = 5000,
    fun = mean,
    na.rm = TRUE,
    sp = TRUE) %>% 
  as_tibble() %>% 
  select(property_id, for_5km = NLCD_VWL)


# Proportion grassland within 5km

grs_5km <-
  raster::extract(
    grassland,
    vwl_points,
    buffer = 5000,
    fun = mean,
    na.rm = TRUE,
    sp = TRUE) %>% 
  as_tibble() %>% 
  select(property_id, grs_5km = NLCD_VWL)

# Join forest covariates with points

vwl_landscape <-
  vwl_points %>% 
    left_join(
      for_1km,
      by = 'property_id') %>% 
    left_join(
      grs_1km,
      by = 'property_id') %>% 
    left_join(
      for_5km,
      by = 'property_id') %>% 
    left_join(
      grs_5km,
      by = 'property_id') %>% 
  unique()

# Remove extra stuff

rm(forest, grassland, nlcd_mask, for_1km, for_5km, grs_5km, grs_1km)



# Compile variables into distance table -----------------------------------

# Need table of: Pair, for_nps, for_vwl, grs_nps, grs_vwl, 3d distance

# Get distance

# Get centroid of each park
centroids <-
  st_read('data/raw/nps_boundary.shp') %>% 
  st_transform(
    crs = st_crs(vwl_points)) %>% 
  filter(
    UNIT_CODE %in% c('ANTI', 'HAFE', 'MONO', 'MANA')) %>% 
  select(
    park = UNIT_CODE, geometry) %>% 
  st_centroid()

# Create distance from each point to each centroid
distances <-
  st_distance(
    vwl_points,
    centroids,
    by_element = FALSE)

# Set column names
colnames(distances) <- centroids$park

# Create table of optimization distance variables

optimize_tbl <-
  distances %>% 
    as_tibble() %>% 
    mutate(
      property_id = vwl_points$property_id) %>% 
    select(5, 1:4) %>% 
    pivot_longer(
      cols = 2:5, 
      names_to = 'park',
      values_to = 'distance') %>% 
    left_join(
      vwl_landscape %>% 
        select(
          property_id, 
          for_vwl = for_1km, 
          grs_vwl = grs_1km),
      by = 'property_id') %>% 
    left_join(
      nps_landscape %>% 
        select(
          park = Park,
          for_nps = for_1km_mean,
          grs_nps = grs_1km_mean),
      by = 'park') %>% 
    select(
      property_id, park, 
      for_vwl, for_nps, 
      grs_vwl, grs_nps, 
      distance) %>% 
  mutate(
    dist_norm =
      ((distance - min(distance)) / 
      (max(distance) - min(distance))) %>% 
      as.double(),
    formula = 
      sqrt(
        (for_nps - for_vwl)^2 +
        (grs_nps - grs_vwl)^2 +
        (dist_norm)^2)
  )

optimize_tbl %>% 
  group_by(park) %>%
  filter(formula %>% min_rank <= 3) %>% 
  arrange(park, formula) 
  

# Graph comparison of landscape vars --------------------------------------

ggplot(
  nps_points,
  aes(for_1km)) +
  geom_boxplot() +
  labs(
    x = 'Proportion forest cover within 1km',
    y = 'Frequency') +
  # Theme elements
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.background = element_rect(fill = 'white'),
        panel.grid = element_line(color = "#e9e9e9"),
        panel.grid.minor = element_line(linetype = 'dashed'),
        axis.line = element_line(color = 'black'),
        strip.background = element_rect(color = 'black', fill = 'black'),
        strip.text = element_text(size = 12, color = 'white')) 

ggplot(
  vwl_landscape,
  aes(for_1km)) +
  geom_boxplot() +
  labs(
    x = 'Proportion forest cover within 1km',
    y = 'Frequency') +
  # Theme elements
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.background = element_rect(fill = 'white'),
        panel.grid = element_line(color = "#e9e9e9"),
        panel.grid.minor = element_line(linetype = 'dashed'),
        axis.line = element_line(color = 'black'),
        strip.background = element_rect(color = 'black', fill = 'black'),
        strip.text = element_text(size = 12, color = 'white')) 
  
