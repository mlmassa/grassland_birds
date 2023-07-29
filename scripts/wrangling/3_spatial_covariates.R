# Before you run: 
# this file results in 'data/processed/static_point_covs'
# and 'data/processed/annual_point_covs'
# and 'data/processed/annual_burn_covs'
# Use that before rerunning all this code!

# Setup -------------------------------------------------------------------

library(sf)
library(tmap)
library(FedData)
library(raster)
library(tidyverse)

# Read in data ------------------------------------------------------------

# CONVERT ALL TO: WGS 84 / AEA: EPSG 9822 (raster) 
  # '+proj=aea +datum=WGS84 +units=m'

# If I used the Chesapeake Bay Program Land Cover/Land Use 1m data...
# MANA: 
# https://cicwebresources.blob.core.windows.net/lulc1718/prin_51153/prin_51153_lulc_2018.zip
# https://cicwebresources.blob.core.windows.net/lulc1718/fair_51059/fair_51059_lulc_2018.zip
# https://cicwebresources.blob.core.windows.net/lulc1718/loud_51107/loud_51107_lulc_2018.zip
# https://cicwebresources.blob.core.windows.net/lulc1718/mana_51683/mana_51683_lulc_2018.zip
# https://cicwebresources.blob.core.windows.net/lulc1718/mana_51685/mana_51685_lulc_2018.zip
# ANTI:
# https://cicwebresources.blob.core.windows.net/lulc1718/wash_24043/wash_24043_lulc_2018.zip
# https://cicwebresources.blob.core.windows.net/lulc1718/jeff_54037/jeff_54037_lulc_2018.zip
# HAFE:
# https://cicwebresources.blob.core.windows.net/lulc1718/fred_24021/fred_24021_lulc_2018.zip
# MONO:
# No additional needs

# Read in NPS points
points <-
  read_rds("data/processed/birds.rds")$points |> 
  st_as_sf(coords = c("long", "lat"), remove = FALSE, crs = "EPSG:4326")

# Read in NPS visits
visits <-
  read_rds("data/processed/birds.rds")$visits

# Get boundary for NLCD import
nlcd_bbox <- 
  st_as_sfc(st_bbox(points)) |> 
  st_buffer(dist = 5100)

# Download NLCD
get_nlcd(
  template = nlcd_bbox,
  label = "nps",
  year = 2019,
  dataset = "landcover",
  extraction.dir = "data/raw/",
  raster.options = c(overwrite = TRUE))

# Clean up
rm(nlcd_bbox)

# Import NLCD
nlcd <- raster("data/raw/nps_NLCD_Land_Cover_2019.tif")

# Project points (to use in extraction)
points_aea <- st_transform(points, crs = st_crs(nlcd))

# Get park shapefiles
parks <-
  st_read("data/processed/focal_parks.shp") |> 
  st_transform(crs = st_crs(nlcd))

# Read in burn data
burns <-
  map_dfr(
    c("ANTI", "MANA", "MONO", "HAFE"),
    ~st_read(
      paste0("data/raw/", ., "_WF.shp")) |> 
    # Set CRS to that of points
    st_transform(
      crs = st_crs(points)) |> 
    # Simplify dataset
    dplyr::select(
      park = UnitCode,
      fire_id = GeometryID,
      fire_year = FireCalend,
      fire_date = FireOutDat,
      geometry))

# Read in veg data
veg <- 
  read_csv('data/raw/FinalVegData.csv') |> 
  mutate(
    grts = as.numeric(GRTS_Order),
    # Fix clinometer readings: topo slope -> degrees
    across(starts_with('Clin'), ~ atan(. / 66) * 180 / pi))

# Read in harvest data
harvest <-
  read_csv('data/raw/nps_harvest.csv') |> 
  left_join(
    read_csv('data/raw/nps_harvest.csv') |> 
    filter(leased == 1) |> 
    select(grts, newlimit = harvest_limit),
    by = 'grts') |> 
  dplyr::select(grts, leased, harvest_date, harvest_limit = newlimit)


# Map parks ---------------------------------------------------------------

#par(mfrow = c(2, 2), mai = c(0.2, 0.1, 0.2, 0.1))
for(p in c("ANTI", "HAFE", "MANA", "MONO")) {
  # Get park outline
  extent <- 
    st_buffer(parks[parks$UNIT_CODE == p, ], dist = 300) |> st_bbox()
  # Plot NLCD with no resample in park area
  plot(nlcd, ext = extent, maxpixels = ncell(nlcd))
  # Add park boundary
  plot(
    parks[parks$UNIT_CODE == p, ]$geometry, 
    lwd = 2, border = "black", add = T)
  # Add burns (SP can do line density fills)
  plot(
    st_union(burns) |> 
      st_transform(crs = crs(nlcd)) |> 
      as_Spatial(), 
    angle = 10, density = 30, col = "grey40", border = "grey40", add = T)
  # Add 100m points
  plot(
    st_buffer(points_aea[points_aea$park == p, ], dist = 100)$geometry, 
    add = T)
  # Add scale bar
  scalebar(
    d = 1000, type = "bar", divs = 4, label = c(0, NA, 1), below = "km",
    xy = c(min(c(extent[["xmin"]], extent[["xmax"]])) + 150, 
           min(c(extent[["ymin"]], extent[["ymax"]])) + 180))
  # Add park name
  text(
    x = mean(c(extent[["xmin"]], extent[["xmax"]])),
    y = extent[["ymax"]] - 250,
    adj = c(0.5, 0.5),
    cex = 1.8,
    font = 2,
    labels = parks[parks$UNIT_CODE == p, ]$PARKNAME
  )
}
dev.off()


# Calculate landcover -----------------------------------------------------

# Check raster values are right
unique(raster::values(nlcd))
nlcd_colors() |> filter(ID %in% unique(raster::values(nlcd)))

## Create reclass matrices ----

# Developed reclass matrix
rcl_dvp <-
  tibble(
    nlcd_value = unique(nlcd)) |> 
    mutate(developed = if_else(nlcd_value %in% c(22, 23, 24), 1, 0)) |> 
    as.matrix()
  
# Grassland reclass matrix
rcl_grs <-
  tibble(
    nlcd_value = unique(nlcd)) |> 
    mutate(grassland = if_else(nlcd_value %in% c(52, 71, 81), 1, 0)) |> 
    as.matrix()

# Forest reclass matrix
# Now including Woody wetlands
rcl_for <-
  tibble(
    nlcd_value = unique(nlcd)) |> 
    mutate(forest = if_else(nlcd_value %in% c(41:43, 90), 1, 0)) |> 
    as.matrix()

# Cropland reclass matrix
rcl_crp <-
  tibble(
    nlcd_value = unique(nlcd)) |> 
    mutate(cropland = if_else(nlcd_value == 82, 1, 0)) |> 
    as.matrix()

# Reclassify and stack results
reclass <-
  raster::stack(
    list(
    crp = reclassify(nlcd, rcl = rcl_crp),
    dvp = reclassify(nlcd, rcl = rcl_dvp),
    `for` = reclassify(nlcd, rcl = rcl_for),
    grs = reclassify(nlcd, rcl = rcl_grs)))

## Run extraction ----

# Set radii to extract landcover (originally 500m, 1km, 5km; I've altered it)
radii <- c(250, 500, 1000, 2500, 5000)

# Extract over radii (~12 seconds)
landcover <-
  map_dfr(
    radii,
    ~raster::extract(
      reclass, 
      points_aea,
      df = TRUE,
      buffer = .x, fun = mean) |> 
      mutate(radius = .x) |> 
      as_tibble()) |> 
  # Add GRTS number back
  mutate(grts = rep(points_aea$grts, length(radii))) |> 
  # Lengthen and then widen to get all landcover-radii by point (for_500 etc.)
  pivot_longer(
    cols = c(crp, dvp, for., grs), 
    names_to = "landcover", values_to = "proportion") |> 
  mutate(landcover = str_remove(landcover, "\\.")) |> 
  pivot_wider(
    id_cols = grts, names_from = c(landcover, radius),
    names_glue = "{landcover}_{radius}", values_from = proportion)

# Clean up
rm(rcl_crp, rcl_dvp, rcl_for, rcl_grs)


# Burn data ---------------------------------------------------------------
  
# Get only points that burned (~22 seconds)
burned_points <-
  points |> 
    # Buffer survey area 100m around point, get burns that intersect
    st_buffer(dist = 100) |> 
    st_intersection(burns) |> 
    as_tibble() |> 
  cbind(
    points |> 
      # Buffer survey area 100m around point, get burns that intersect
      st_buffer(dist = 100) |> 
      st_intersection(burns) |> 
      st_area() |> 
      as_tibble() |> 
        mutate(prop_burned = as.numeric(value)/(pi*100*100))) |> 
    dplyr::select(park, grts, fire_year, fire_date, prop_burned) |> 
    arrange(grts, fire_date) |> 
  # ONLY INCLUDE 10% BURNED
  filter(prop_burned >= 0.1)
 

## Annual burn ----
# Get annual fire interval (time since last burn)
annual_burns <-
  map(
    # For each GRTS point ID...
    unique(burned_points$grts),
    function(i) {
      # Split: get only this point
      grts_split <-
        burned_points |> 
        filter(grts == i)
      # Apply: Perform another function (over years)
      inner_map <-
        map(
          # For each year...
          2014:2021,
          function(y) {
            # Split: get only fires during or before this year
            burn_split <-
              grts_split |> 
              filter(fire_year <= y)
            # Apply: get most recent burn
            tibble(
              year = y,
              last_burn = max(burn_split$fire_year, na.rm = T))
          })
        # Combine: Get last burn in all years for this point
        bind_rows(inner_map) |> 
          mutate(grts = i)      
          }) |> 
  bind_rows() |> 
  dplyr::select(grts, year, last_burn) |> 
  mutate(
    # Fix Inf to NA
    last_burn = na_if(last_burn, -Inf),
    last_burn = na_if(last_burn, Inf),
    # Create annual time since burn
    t_since_burn = year - last_burn) |> 
    # Add NA points
    right_join(
      points |> 
        st_drop_geometry() |> dplyr::select(grts),
      by = 'grts') |> 
    arrange(grts) |> 
    # Add years for unburned points
    complete(grts, year) |> 
    dplyr::select(grts, year, t_since_burn) |> 
    filter(!is.na(year)) |> 
    # Add area burned
    left_join(
      burned_points |> dplyr::select(grts, year = fire_year, prop_burned), 
      by = c("grts", "year"))

# Veg data ----------------------------------------------------------------

veg_static <-
  veg |> 
    mutate(
      shrub = 
        rowMeans(
          dplyr::select(veg, c(Shrub_N:Shrub_W)), 
          na.rm = TRUE),
      clin_mean = 
        rowMeans(
          dplyr::select(veg, c(Clin_N:Clin_W)), 
          na.rm = TRUE)) |> 
    dplyr::select(
      grts = GRTS_Order,
      trees = Trees,
      shrub,
      angle_mean = clin_mean,
      angle_max = Clin_Max)


# Field type --------------------------------------------------------------

# Get only field type; reassign mismatch
field_type <-
  veg |> 
  dplyr::select(grts, Type_1, Type_2) |> 
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
        TRUE ~ 'Hayfield')) |> 
  dplyr::select(grts, field_type)

# Harvest data ------------------------------------------------------------

lease <-
  read_csv('data/raw/nps_harvest_update.csv') |> 
  mutate(
    grass_type = if_else(
      is.na(grass_type) & habitat_type == "hayfield", 
      "CSG", 
      grass_type))

## Annual harvest ----

# Get first year surveyed
first_survey <- 
  visits |> 
    dplyr::select(grts, year) |> 
    group_by(grts) |> 
    summarize(first_survey = min(year))

# Get annual harvest date. This should be a for loop or map...
harv1 <-
  lease |> 
  arrange(grts) |> 
    mutate(
      first_limit = year(mdy(harvest_date)),
      ever_limit = harvest_limit,
      harvest_date = yday(mdy(harvest_date))) 
  
annual_harvest <-
  crossing(grts = points$grts, year = c(2014:2021)) |> 
    left_join(harv1) |> 
    mutate(
      harvest_limit = 
        case_when(
          leased == 1 & first_limit <= year ~ 1,
          ever_limit == 0 & leased == 1 ~ 0,
          leased == 0 ~ NA_real_),
      harvest_day = if_else(harvest_limit == 0, NA_real_, harvest_date),
      years_limited = 1 + year - first_limit, 
      years_limited = if_else(years_limited < 1, 0, years_limited))  |> 
    dplyr::select(grts, year, harvest_limit, years_limited, harvest_day)

# Static covariates -------------------------------------------------------

static <-
  points |> 
    left_join(field_type) |> 
    left_join(veg_static) |> 
    left_join(lease) |> 
    left_join(landcover) |> 
    mutate(
      ever_burned = if_else(grts %in% burned_points$grts, 1, 0),
      habitat = paste(grass_type, habitat_type) |> str_remove_all("NA ")) |> 
  dplyr::select(-note, -harvest_date, -field_type)

static |> 
  st_drop_geometry() |> 
  dplyr::select(-point_name, -long, -lat) |> 
  write_rds("data/processed/static_covs.rds")

# Annual covariates -------------------------------------------------------

left_join(annual_harvest, annual_burns) |> 
  write_rds("data/processed/annual_covs.rds")


# Map static covs ---------------------------------------------------------

## Map all static covariates

tmap_mode("view")
tmap_options(check.and.fix = TRUE)

tm_basemap(c("OpenStreetMap", "Esri.WorldImagery")) +
# Parks
  tm_shape(parks) +
  tm_polygons(
    border.col = "black",
    alpha = 0) +
# Burns
  tm_shape(burns) +
  tm_polygons(
    col = 'fire_year',
    alpha = 0.25,
    popup.vars = 'fire_date') +
# Point radius
  tm_shape(st_buffer(static, dist = 100)) +
  tm_polygons(
    border.col = "black", 
    alpha = 0)
