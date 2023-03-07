# Chapter 2: Protected areas and grassland bird diversity

# Setup -------------------------------------------------------------------

#library(raster)
library(tidyverse)
library(sf)

theme_set(theme_bw())

radii <- c(250, 500, 1000, 2500)
y <- 2016

# Import data -------------------------------------------------------------

# Protected areas database
pl <-
  st_read("data/raw/PADUS3_0Combined_Region1.shp") %>%
  filter(State_Nm %in% c("VA", "MD", "WV")) %>%
  # Fix polygon errors
  st_make_valid() %>%
  st_transform(crs = 4326) %>%
  # Simplify ownership
  mutate(
    owner = case_when(
      d_Own_Type %in% c(
        "Local Government", "State",
        "Non-Governmental Organization", "Federal") ~ "Public",
      d_Own_Type == "Private" ~ "Private easement",
      TRUE ~ d_Own_Type))

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
  # Make sf spatial object
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  # Add protected area polygon info at each point
  st_join(
    select(
      pl, owner, manager = d_Mang_Nam, pa_name = Unit_Nm, geometry)) %>%
  # Add category for unprotected ownership
  replace_na(list(owner = "Private unprotected", manager = "Private")) %>% 
  # Remove dupes caused by overlapping polygons
  filter(!duplicated(point_id))

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
    select(-dttm, -grts, -humidity, -disturbance)

# Import and combine bird survey count data
# Distance and time intervals could be made congruent
# But I am not doing that right now
birds <-
  bind_rows(
    read_rds("data/processed/birds.rds")$counts %>% 
      select(visit_id, species, incidental = flyover),
    read_rds("data/processed/vwl_birds.rds")$birds %>% 
      mutate(incidental = if_else(is.na(incidental), 0, 1)) %>% 
      select(visit_id, species, incidental))

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

# Make all-grass species list and save it
sp_grs <-
  sp_all %>% 
  filter(species %in% c(sp_ob, sp_fac)) %>% 
  mutate(status = if_else(species %in% sp_ob, "obligate", "facultative"))

write_rds(sp_grs, "data/processed/sp_grs_ch2.rds")

# Landcover ---------------------------------------------------------------
# 
# nlcd <-
#   raster("data/raw/studyarea_NLCD_Land_Cover_2016.tif")
# 
# points_aea <- 
#   st_transform(points, crs = st_crs(nlcd))
# 
# # Reclassify raster
# 
# # Developed reclass matrix
# rcl_dvp <-
#   tibble(
#     nlcd_value = unique(nlcd)) %>% 
#     mutate(developed = if_else(nlcd_value %in% c(22, 23, 24), 1, 0)) %>% 
#     as.matrix()
#   
# # Grassland reclass matrix (shrub|herbaceous|hay)
# rcl_grs <-
#   tibble(
#     nlcd_value = unique(nlcd)) %>% 
#     mutate(grassland = if_else(nlcd_value %in% c(52, 71, 81), 1, 0)) %>% 
#     as.matrix()
# 
# # Forest reclass matrix (decid|evergreen|mixed|woody wetlands)
# rcl_for <-
#   tibble(
#     nlcd_value = unique(nlcd)) %>% 
#     mutate(forest = if_else(nlcd_value %in% c(41:43, 90), 1, 0)) %>% 
#     as.matrix()
# 
# # Cropland reclass matrix
# rcl_crp <-
#   tibble(
#     nlcd_value = unique(nlcd)) %>% 
#     mutate(cropland = if_else(nlcd_value == 82, 1, 0)) %>% 
#     as.matrix()
# 
# # Reclassify and stack results
# # Runtime: 10 seconds
# reclass <-
#   raster::stack(
#     reclassify(nlcd, rcl = rcl_crp),
#     reclassify(nlcd, rcl = rcl_dvp),
#     reclassify(nlcd, rcl = rcl_for),
#     reclassify(nlcd, rcl = rcl_grs)
#     )
# 
# names(reclass) <- 
#   c("cropland", "developed", "forest", "grassland")
# 
# # Extract raster 
# landcover <-
#   map_dfr(
#     radii,
#     ~raster::extract(
#       reclass, 
#       points_aea,
#       df = TRUE,
#       buffer = .x, fun = mean) %>% 
#     mutate(radius = .x) %>% 
#     as_tibble())
# 
# landcover$point_id <-
#   rep(points$point_id, times = length(radii))
# 
# write_rds(
#   landcover %>% 
#     select(point_id, radius, cropland, developed, forest, grassland),
#   "data/processed/ch2_landcover.rds")

# Protected area cover ----------------------------------------------------

# points_buffer <-
#   map_dfr(
#     radii,
#     ~st_buffer(points, .x) %>% 
#   mutate(radius = .x))
# 
# points_buffer$area <- st_area(points_buffer)
# 
# prot_area <-
#   points_buffer %>% 
#   group_by(point_id, radius) %>%
#   # Intersect buffers with flattened protected area multipolygon
#   st_intersection(
#     pl %>% 
#     # Crop to study area to speed calculations and plotting
#     st_crop(
#       st_buffer(points, dist = max(radii) + 100)) %>% 
#     # Combine all polygons into one "protected area"
#     st_union() %>% 
#     # Some errors result. Autofix them
#     st_make_valid())
#   
# prot_area$overlap_area <- st_area(prot_area)
# 
# prot_area <-
#   mutate(
#     prot_area,
#     protected = (overlap_area/area) %>% units::drop_units()) %>% 
#   select(-area, -overlap_area) %>% 
#   st_drop_geometry()
# 
# write_rds(
#   landcover %>% 
#     left_join(prot_area %>% select(point_id, radius, protected)),
#   "data/processed/ch2_landcover.rds")

landcover <- 
  read_rds("data/processed/ch2_landcover.rds") %>% 
  distinct() %>% 
  replace_na(list(protected = 0))

# Summary and exploration -------------------------------------------------

# How many grassland species?
nrow(sp_grs)

# Maximum of 27 grassland species possible. 26 if exclude WTKI
birds %>% 
  filter(incidental == 0) %>% 
  left_join(visits) %>% 
  filter(species %in% c(sp_ob, sp_fac)) %>% 
  group_by(point_id) %>% 
  summarize(sp = length(unique(species))) %>% 
  left_join(points) %>% 
  group_by(owner) %>% 
  summarize(mean_sp = mean(sp))

# What was the distribution of ownership over years?
visits %>% 
  left_join(points) %>% 
  distinct(point_id, year, owner) %>% 
  group_by(year, owner) %>% 
  count() %>% 
  filter(!is.na(owner)) %>% 
  ggplot(aes(x = year, y = n, fill = owner)) + 
    geom_col() + 
    scale_fill_brewer(palette = "Paired") + 
    scale_x_continuous(breaks = c(2012:2022)) +
  labs(fill = "Landowner", y = "Points surveyed", x = "") +
  theme(legend.position = c(0.15, 0.85))

# What were the points for each year?
surveyed_points <-
  vector("list", length(unique(visits$year)))

for (i in 1:11){
  surveyed_points[[i]] <-
    visits %>% 
    filter(year == 2011+i) %>% 
    pull(point_id) %>% unique()
}

# What is the distribution of area protected by radius and owner?
landcover %>% 
  left_join(points) %>% 
  ggplot(
    aes(x = factor(radius), y = protected, fill = owner)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Radius", y = "Proportion protected areas", fill = "Landowner")

# How many new points were added each year?
# When was the most recent survey?
first_year <-
  visits %>% 
  left_join(points) %>% 
  group_by(point_id, owner) %>% 
  filter(!is.na(owner)) %>% 
  summarize(first = min(year), last = max(year))

first_year %>% 
  group_by(first, owner) %>% 
  count() %>% 
  ggplot(aes(x = factor(first), y = n, fill = owner)) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "", y = "New points added", fill = "Landowner") +
  theme(legend.position = c(0.75, 0.85))

first_year %>% 
  group_by(last, owner) %>% 
  count() %>% 
  ggplot(aes(x = factor(last), y = n, fill = owner)) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "", y = "Last surveyed", fill = "Landowner") +
  theme(legend.position = c(0.15, 0.85))

# Format data for vegan ---------------------------------------------------

# Take only first year of survey
focal_points <-
  visits %>% 
  left_join(first_year) %>% 
  filter(year == first) %>% 
  filter(visit == 3) %>% 
  select(point_id, first)

# New focal points
points_f <-
  points %>% 
  filter(point_id %in% focal_points$point_id) %>% 
  distinct(point_id, .keep_all = T)

# New focal visits
visits_f <-
  visits %>% 
  right_join(focal_points) %>% 
  filter(year == first, visit <=3)

# New birds
birds_f <-
  birds %>% 
  filter(
    visit_id %in% visits_f$visit_id, 
    incidental == 0) %>% 
  select(-incidental)

# Wide species abundance per site table
abund_a <-
  left_join(birds_f, visits_f) %>% 
  group_by(visit_id, species) %>% 
  count() %>% 
  # Get zeroes
  right_join(
    birds_f %>% expand(visit_id, species)) %>% 
  replace_na(list(n = 0)) %>% 
  left_join(visits_f) %>% 
  filter(species %in% sp_grs$species) %>% 
  group_by(point_id, species) %>% 
  summarize(
    #max = max(n),
    #sum = sum(n),
    mean = mean(n)
    ) %>% 
  pivot_wider(names_from = "species", values_from = "mean")

# But what if I want the relative frequency of the species across ALL visits
# standardized to number of visits?
abund_b <-
  left_join(birds, visits) %>% 
  group_by(visit_id, species) %>% 
  count() %>% 
  # Get zeroes
  right_join(
    birds %>% expand(visit_id, species)) %>% 
  replace_na(list(n = 0)) %>% 
  left_join(visits) %>% 
  filter(species %in% sp_grs$species) %>% 
  group_by(point_id, species) %>% 
  summarize(
    #max = max(n),
    #sum = sum(n),
    mean = mean(n)
    #visits = length(n)
    ) %>% 
  pivot_wider(names_from = "species", values_from = "mean")


# Calculate metrics and visualize -----------------------------------------


library(vegan)


data <-
  full_join(
    specnumber(
      column_to_rownames(abund_a, var = "point_id"))%>% 
      enframe(name = "point_id", value = "richness"),
    diversity(
      column_to_rownames(abund_a, var = "point_id")) %>% 
      enframe(name = "point_id", value = "diversity")) %>% 
    left_join(landcover) %>%
    left_join(points)
  
hist(data$richness)
hist(data$diversity)

data %>% 
  filter(!is.na(radius)) %>% 
  pivot_longer(
    cols = cropland:protected, 
    values_to = "proportion", 
    names_to = "metric") %>% 
  ggplot(aes(x = proportion, y = diversity)) +
    geom_jitter(alpha = 0.15, size = 0.25) +
    facet_grid(rows = vars(metric), cols = vars(radius)) +
    geom_smooth(method = "lm")

data %>% 
  filter(!is.na(owner)) %>% 
  ggplot(aes(x = owner, y = diversity, fill = owner)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Paired")

  
