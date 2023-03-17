# Chapter 2.1: Processing protected areas and landcover
# Produces: 
#   data/processed/ch2_pointcovs.rds (land/prot cover covariates over radii)
#   data/processed/ch2_combined_birds.rds (bird data combined)
#   data/processed/prot_areas.shp (simplified shapefile)

# Setup -------------------------------------------------------------------

library(raster) # load 1st, overwrite raster::select with dplyr::select
library(tidyverse)
library(sf)

radii <- seq(from = 250, to = 8000, by = 250)

# Import data -------------------------------------------------------------

# Protected areas database
pl <-
  # Import file
  st_read("data/raw/PADUS3_0Combined_Region1.shp") %>%
  # Filter to only relevant states to make file smaller
  filter(State_Nm %in% c("VA", "MD", "WV")) %>%
  # Fix polygon errors
  st_make_valid() %>%
  # Make latlong to match points
  st_transform(crs = 4326) %>%
  # Simplify ownership
  mutate(
    owner = case_when(
      d_Own_Type %in% c(
        "Local Government", "State",
        "Non-Governmental Organization", "Federal") ~ "Public",
      d_Own_Type == "Private" ~ "Private easement",
      d_Own_Type == "Unknown" ~ "Unknown/Other",
      TRUE ~ "Public")) %>% 
  # Rename and cut down on variables
  select(
    loc_name = Unit_Nm,
    owner, 
    manager = d_Mang_Typ,
    owner_name = d_Own_Name,
    description = d_Des_Tp,
    access = d_Pub_Acce,
    gap = d_GAP_Sts,
    iucn = d_IUCN_Cat,
    ease_holder = EsmtHldr,
    area = SHAPE_Area
    )

st_write(pl, "data/processed/prot_areas.shp")

# Data points surveyed
points <-
  bind_rows(
    # Virginia Working Landscapes survey points
    VWL = 
      read_rds("data/processed/vwl_birds.rds")$points %>% 
      mutate(property_id = as.character(property_id)),
    # National Park Service survey points
    NPS = 
      read_rds("data/processed/birds.rds")$points %>% 
      mutate(property_id = park, point_id = as.character(grts)) %>% 
      select(-park, -grts, -point_name),
    .id = "source") %>% 
  # Remove points missing location data
  filter(!is.na(long), !is.na(lat)) %>% 
  # Make sf spatial object (latlong)
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  # Add protected area polygon info at each point
  st_join(pl) %>%
  # Add category for unprotected ownership
  replace_na(list(owner = "Private unprotected", manager = "Private")) %>% 
  # Remove dupes caused by overlapping polygons
  filter(!duplicated(point_id)) %>% 
  # Change VWL property ID in NPS to match NPS data
  mutate(
    property_id = case_when(
      loc_name == "Manassas National Battlefield Park" ~ "MANA",
      loc_name == "George Washington National Forest" ~ "GWNF",
      loc_name == "Shenandoah National Park" ~ "SHEN",
      loc_name == "Sky Meadows State Park" ~ "SMSP",
      loc_name == "Shenandoah Valley Battlefield Foundation Holding" ~ "SVBF Holding",
      TRUE ~ property_id))

# Import and combine bird survey visit data
visits <-
  bind_rows(
    read_rds("data/processed/birds.rds")$visits %>% 
      mutate(
        point_id = as.character(grts),
        n_observers = 1,
        sky = case_when(
          sky == 0 ~ "clear",
          sky == 1 ~ "p. cloudy",
          sky == 2 ~ "overcast",
          sky == 3 ~ "fog",
          sky %in% c(4, 5, 6) ~ "rain")), 
    read_rds("data/processed/vwl_birds.rds")$visits %>% 
      mutate(
        sky = case_when(
          sky <2 ~ "clear",
          sky == 2 ~ "p. cloudy",
          sky == 3 ~ "overcast",
          sky == 4 ~ "rain",
          sky == 5 ~ "fog"))) %>% 
    select(-dttm, -grts, -humidity, -disturbance) %>% 
  # Remove visits at locations not in my dataset
  filter(point_id %in% points$point_id)

# Import and combine bird survey count data
# Distance and time intervals could be made congruent
# But I am not doing that right now
birds <-
  bind_rows(
    read_rds("data/processed/birds.rds")$counts %>% 
      select(visit_id, species, incidental = flyover),
    read_rds("data/processed/vwl_birds.rds")$birds %>% 
      mutate(incidental = if_else(is.na(incidental), 0, 1)) %>% 
      select(visit_id, species, incidental)) %>% 
  filter(visit_id %in% visits$visit_id)

# Load taxonomy of observed spp
sp_all <-
  distinct(birds, species) %>% 
  left_join(read_rds("data/processed/taxonomy.rds")) %>% 
  arrange(tax_order)

# List of OBLIGATE grassland species (based on Vickery 1999)
sp_ob <-
  c(
    "NOHA", "LEOW", "SEOW", "HOLA", "SEWR", "AMPI", "VESP", "SAVS", "GRSP", 
    "HESP", "LALO", "DICK", "BOBO", "EAME")

# List of FACULTATIVE grassland species (based on Vickery 1999)
sp_fac <-
  c(
    "AMBI", "CAEG", "TUVU", "CANG", "MALL", "AMKE", "MERL", "PEFA", "WTKI",
    "NOBO", "SACR", "KILL", "LEYE", "MODO", "BANO", "CONI", "EAKI", "LOSH", 
    "EABL", 
    "BLVU", # Flagged as fac in south america by vickery?
    "COYE", "RWBL", "BHCO")

# Make all-grass species list
sp_grs <-
  sp_all %>% 
  filter(species %in% c(sp_ob, sp_fac)) %>% 
  mutate(status = if_else(species %in% sp_ob, "obligate", "facultative"))

# Save processed data
write_rds(sp_grs, "data/processed/sp_grs_ch2.rds")
write_rds(
  mget(c("points", "visits", "birds", "sp_fac", "sp_ob", "sp_grs")),
  "data/processed/ch2_combined_birds.rds")

# Landcover ---------------------------------------------------------------

nlcd <-
  raster("data/raw/studyarea_NLCD_Land_Cover_2016.tif")

points_aea <-
  st_transform(points, crs = st_crs(nlcd))

## Reclassify raster ----

# Developed reclass matrix
rcl_dvp <-
  tibble(
    nlcd_value = unique(nlcd)) %>%
    mutate(developed = if_else(nlcd_value %in% c(22, 23, 24), 1, 0)) %>%
    as.matrix()

# Grassland reclass matrix (shrub|herbaceous|hay)
rcl_grs <-
  tibble(
    nlcd_value = unique(nlcd)) %>%
    mutate(grassland = if_else(nlcd_value %in% c(52, 71, 81), 1, 0)) %>%
    as.matrix()

# Forest reclass matrix (decid|evergreen|mixed|woody wetlands)
rcl_for <-
  tibble(
    nlcd_value = unique(nlcd)) %>%
    mutate(forest = if_else(nlcd_value %in% c(41:43, 90), 1, 0)) %>%
    as.matrix()

# Cropland reclass matrix
rcl_crp <-
  tibble(
    nlcd_value = unique(nlcd)) %>%
    mutate(cropland = if_else(nlcd_value == 82, 1, 0)) %>%
    as.matrix()

# Reclassify and stack results
# Runtime: 10 seconds
reclass <-
  raster::stack(
    reclassify(nlcd, rcl = rcl_crp),
    reclassify(nlcd, rcl = rcl_dvp),
    reclassify(nlcd, rcl = rcl_for),
    reclassify(nlcd, rcl = rcl_grs)
    )

names(reclass) <-
  c("cropland", "developed", "forest", "grassland")

## Extract raster ----
# Runtime: 27 minutes
landcover <-
  map_dfr(
    radii,
    ~raster::extract(
      reclass,
      points_aea,
      df = TRUE,
      buffer = .x, fun = mean) %>%
    mutate(radius = .x) %>%
    as_tibble())

landcover$point_id <-
  rep(points_aea$point_id, times = length(radii))

write_rds(
  landcover %>%
    select(point_id, radius, cropland, developed, forest, grassland),
  "data/processed/ch2_landcover.rds")

# Protected area cover ----------------------------------------------------

points_buffer <-
  map_dfr(
    radii,
    ~st_buffer(points, .x) %>%
  mutate(radius = .x))

points_buffer$area <- st_area(points_buffer)

prot_area <-
  points_buffer %>%
  group_by(point_id, radius) %>%
  # Intersect buffers with flattened protected area multipolygon
  st_intersection(
    pl %>%
    # Crop to study area to speed calculations and plotting
    st_crop(
      st_buffer(points, dist = max(radii) + 100)) %>%
    # Combine all polygons into one "protected area"
    st_union() %>%
    # Some errors result. Autofix them
    st_make_valid())

prot_area$overlap_area <- st_area(prot_area)

prot_area <-
  mutate(
    prot_area,
    protected = (overlap_area/area) %>% units::drop_units()) %>%
  replace_na(replace = list(protected = 0)) %>% 
  select(-area, -overlap_area) %>%
  st_drop_geometry()

points_full <-
  left_join(
    landcover, 
    select(prot_area, point_id, radius, protected)) %>% 
    relocate(point_id) %>% 
    arrange(point_id) %>% 
    select(-ID) %>% 
    replace_na(list(protected = 0)) %>% 
    left_join(points)

write_rds(points_full, "data/processed/ch2_pointcovs.rds")

