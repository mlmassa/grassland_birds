# Before you run: 
# this file results in 'data/processed/static_point_covs'
# and 'data/processed/annual_point_covs'
# and 'data/processed/annual_burn_covs'
# Use that before rerunning all this code!

# setup -------------------------------------------------------------------

library(sf)
library(sp)
library(tmap)
library(tidyverse)

# read in shapefile data --------------------------------------------------

# NAD 83 / UTM 18N: EPSG 26918 (shapefiles)
# CONVERT ALL TO: WGS 84 / AEA: EPSG 9822 (raster) 
  # '+proj=aea +datum=WGS84 +units=m'

# Read in NLCD (WGS84 / AEA)

nlcd_raw <-
  raster::raster('data/raw/NLCD_2019_Land_Cover_crop.tiff')


# Read in grassland points

points <-
  st_read('data/raw/all_GRTS_pts_hab.shp') %>% 
  filter(Srvy_Ty == 'Grassland') %>% 
  select(Park, ident) %>% 
  # grts is my point ID
  rename(grts = ident) %>% 
  # Change proj. of points (small) to match raster (large)
  st_transform(crs = raster::projection(nlcd_raw))

# Create point buffer

point_buff_6km <-
  st_read('data/raw/all_GRTS_pts_hab.shp') %>% 
  filter(Srvy_Ty == 'Grassland') %>% 
  select(ident) %>% 
  rename(
    grts = ident) %>% 
  st_buffer(dist = 6000) %>% 
  st_transform(crs = raster::projection(nlcd_raw))


# Crop and mask raster to buffer

nlcd_mask <-
  nlcd_raw %>% 
  raster::crop(., point_buff_6km) %>% 
  raster::mask(., point_buff_6km)


# Clean up

rm(nlcd_raw)
rm(point_buff_6km)
gc()


# Check that it looks right

raster::plot(nlcd_mask)


# Reclass landcover -------------------------------------------------------

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
      # From NLCD legend, Johnson (2016) combined classes:
      # "grassland" = grass, shrub, pasture (52, 71, 81)
      mutate(
        to = if_else(from %in% c(52, 71, 81), 1, 0)) %>% 
  as.matrix())


# Reclassify to 1 developed/0 not developed

developed <-
  raster::reclassify(
    nlcd_mask,
    # Reclass matrix
    tibble(
      from = 
          nlcd_mask %>% 
          raster::values() %>% 
          unique()) %>% 
      # From NLCD legend, low-med-high intensity development 
      # (NOT devp. open space)
      mutate(
        to = if_else(from %in% c(22, 23, 24), 1, 0)) %>% 
  as.matrix())

# Reclassify to 1 wetland/0 not wetland
    
wetland <-
  raster::reclassify(
    nlcd_mask,
    # Reclass matrix
    tibble(
      from = 
          nlcd_mask %>% 
          raster::values() %>% 
          unique()) %>% 
      # From NLCD legend, woody and emergent herb. wetlands (90, 95)
      mutate(
        to = if_else(from %in% c(90, 95), 1, 0)) %>% 
  as.matrix())


# 500m scales -------------------------------------------------------------

# Proportion forest within 500m

for_500m <-
  raster::extract(
    forest,
    points,
    buffer = 500,
    fun = mean,
    na.rm = TRUE,
    sp = TRUE) %>% 
  as_tibble() %>% 
  select(grts, for_500m = NLCD_2019_Land_Cover_crop)


# Proportion grassland within 500m

grs_500m <-
  raster::extract(
    grassland,
    points,
    buffer = 500,
    fun = mean,
    na.rm = TRUE,
    sp = TRUE) %>% 
  as_tibble() %>% 
  select(grts, grs_500m = NLCD_2019_Land_Cover_crop)


# Proportion developed within 500m

dvp_500m <-
  raster::extract(
    developed,
    points,
    buffer = 500,
    fun = mean,
    na.rm = TRUE,
    sp = TRUE) %>% 
  as_tibble() %>% 
  select(grts, dvp_500m = NLCD_2019_Land_Cover_crop)


# Proportion wetland within 500m

wet_500m <-
  raster::extract(
    wetland,
    points,
    buffer = 500,
    fun = mean,
    na.rm = TRUE,
    sp = TRUE) %>% 
  as_tibble() %>% 
  select(grts, wet_500m = NLCD_2019_Land_Cover_crop)

# 1km scales --------------------------------------------------------------

# Proportion forest within 1km

for_1km <-
  raster::extract(
    forest,
    points,
    buffer = 1000,
    fun = mean,
    na.rm = TRUE,
    sp = TRUE) %>% 
  as_tibble() %>% 
  select(grts, for_1km = NLCD_2019_Land_Cover_crop)


# Proportion grassland within 1km

grs_1km <-
  raster::extract(
    grassland,
    points,
    buffer = 1000,
    fun = mean,
    na.rm = TRUE,
    sp = TRUE) %>% 
  as_tibble() %>% 
  select(grts, grs_1km = NLCD_2019_Land_Cover_crop)


# Proportion developed within 1km

dvp_1km <-
  raster::extract(
    developed,
    points,
    buffer = 1000,
    fun = mean,
    na.rm = TRUE,
    sp = TRUE) %>% 
  as_tibble() %>% 
  select(grts, dvp_1km = NLCD_2019_Land_Cover_crop)


# Proportion wetland within 1km

wet_1km <-
  raster::extract(
    wetland,
    points,
    buffer = 1000,
    fun = mean,
    na.rm = TRUE,
    sp = TRUE) %>% 
  as_tibble() %>% 
  select(grts, wet_1km = NLCD_2019_Land_Cover_crop)

# 5km scales --------------------------------------------------------------

# Proportion forest within 5km

for_5km <-
  raster::extract(
    forest,
    points,
    buffer = 5000,
    fun = mean,
    na.rm = TRUE,
    sp = TRUE) %>% 
  as_tibble() %>% 
  select(grts, for_5km = NLCD_2019_Land_Cover_crop)


# Proportion grassland within 5km

grs_5km <-
  raster::extract(
    grassland,
    points,
    buffer = 5000,
    fun = mean,
    na.rm = TRUE,
    sp = TRUE) %>% 
  as_tibble() %>% 
  select(grts, grs_5km = NLCD_2019_Land_Cover_crop)


# Proportion developed within 5km

dvp_5km <-
  raster::extract(
    developed,
    points,
    buffer = 5000,
    fun = mean,
    na.rm = TRUE,
    sp = TRUE) %>% 
  as_tibble() %>% 
  select(grts, dvp_5km = NLCD_2019_Land_Cover_crop)


# Proportion wetland within 5km

wet_5km <-
  raster::extract(
    wetland,
    points,
    buffer = 5000,
    fun = mean,
    na.rm = TRUE,
    sp = TRUE) %>% 
  as_tibble() %>% 
  select(grts, wet_5km = NLCD_2019_Land_Cover_crop)

# Merge data --------------------------------------------------------------

# Join landscape covariates with points

static_point_covs <-
  points %>% 
  
  # 500m scale
    left_join(grs_500m) %>% 
    left_join(dvp_500m) %>% 
    left_join(for_500m) %>% 
    left_join(wet_500m) %>% 
  
  # 1 km scale
    left_join(grs_1km) %>% 
    left_join(dvp_1km) %>% 
    left_join(for_1km) %>% 
    left_join(wet_1km) %>% 
  
  # 5 km scale
    left_join(grs_5km) %>% 
    left_join(dvp_5km) %>% 
    left_join(for_5km) %>% 
    left_join(wet_5km) 

  
# Remove extra stuff

rm(forest, grassland, developed, wetland,
   nlcd_mask, 
   for_500m, for_1km, for_5km, 
   grs_500m, grs_5km, grs_1km, 
   dvp_500m, dvp_1km, dvp_5km,
   wet_500m, wet_1km, wet_5km)


# Burn data ---------------------------------------------------------------

# Read in all burn data
burns <-
  map_dfr(
    c('ANTI', 'MANA', 'MONO', 'HAFE'),
    ~st_read(
      paste0('data/raw/', ., '_WF.shp')) %>% 
      
      # Set CRS to that of points
      st_transform(
        crs = st_crs(points)) %>% 
      
      # Simplify dataset
      select(
        park = UnitCode,
        fire_id = GeometryID,
        fire_year = FireCalend,
        fire_date = FireOutDat,
        geometry))
 


# map burns ---------------------------------------------------------------

tmap_mode('view')

tm_basemap('OpenStreetMap') +
  
  tm_shape(burns) +
  tm_polygons(col = 'fire_year',
              alpha = 0.5,
              popup.vars = 'fire_date') +
  
  tm_shape(points) +
  tm_dots(clustering = FALSE)


# merge -------------------------------------------------------------------

# Burn Y/N
points %>% 
  # Buffer survey area 100m around point
  st_buffer(dist = 100) %>% 
  st_intersection(burns) %>% 
  as_tibble() %>% 
  select(grts) %>% 
  unique() %>% 
  mutate(ever_burned = 1) %>% 
  right_join(
    static_point_covs,
    by = 'grts') %>% 
  mutate(
    ever_burned = replace_na(ever_burned, 0)) %>% 
  # Write point covariates RDS file
  write_rds('data/processed/static_point_covs.rds')


# Annual fire covariates --------------------------------------------------

# Get only points that burned
burned_points <-
  points %>% 
    # Buffer survey area 100m around point, get burns that intersect
    st_buffer(dist = 100) %>% 
    st_intersection(burns) %>% 
    as_tibble() %>% 
  cbind(
    points %>% 
      # Buffer survey area 100m around point, get burns that intersect
      st_buffer(dist = 100) %>% 
      st_intersection(burns) %>% 
      st_area() %>% 
      as_tibble() %>% 
        mutate(prop_burned = as.numeric(value)/(pi*100*100))) %>% 
    select(park, grts, fire_year, fire_date, prop_burned) %>% 
    arrange(grts, fire_date)
 

# Get annual fire interval (time since last burn)

big_map <-
  map(
    # For each GRTS point ID...
    unique(burned_points$grts),
    function(i) {
      # Split: get only this point
      grts_split <-
        burned_points %>% 
        filter(grts == i)
      
      # Apply: Perform another function (over years)
      inner_map <-
        map(
          # For each year...
          2014:2021,
          function(y) {
            # Split: get only fires during or before this year
            burn_split <-
              grts_split %>% 
              filter(fire_year <= y)
            
            # Apply: get most recent burn
            tibble(
              year = y,
              last_burn = max(burn_split$fire_year))
          })
      
        # Combine: Get last burn in all years for this point
        bind_rows(inner_map) %>% 
          mutate(grts = i)      
          }
        )

# Combine: put all GRTS point IDs together
bind_rows(big_map) %>%
  select(grts, year, last_burn) %>% 
  mutate(
    grts = as.numeric(grts),
    # Fix Inf to NA
    last_burn = na_if(last_burn, -Inf),
    last_burn = na_if(last_burn, Inf),
    # Create annual time since burn
    t_since_burn = year - last_burn) %>% 
  
  # Add NA points
  right_join(
    points %>% 
      mutate(grts = as.numeric(grts)) %>% 
      select(grts),
    by = 'grts') %>% 
  arrange(grts) %>% 
  # Add years for unburned points
  complete(grts, year) %>% 
  select(grts, year, t_since_burn) %>% 
  filter(!is.na(year)) %>% 
  
  # Create classes
  mutate(
    burn_class =
      case_when(
                #is.na(t_since_burn) ~ 'Never burned',
                t_since_burn == 0 ~ 'Burned this year',
                t_since_burn == 1 ~ '1-2 years ago',
                t_since_burn == 2 ~ '1-2 years ago',
                t_since_burn > 2 ~ '3+ years ago')) %>% 
  
write_rds('data/processed/annual_burn_covs.rds')



