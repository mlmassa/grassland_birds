# Chapter 2: Protected areas and grassland bird diversity

# Setup -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(tmap)
theme_set(theme_bw())

# Import data -------------------------------------------------------------

# Protected areas database
pl <-
  st_read("data/raw/PADUS3_0Combined_Region1.shp") %>% 
  filter(State_Nm %in% c("VA", "MD", "WV")) %>% 
  st_make_valid() %>% 
  st_transform(crs = 4326)

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
  filter(!is.na(long), !is.na(lat)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  # Add protected area polygon info at each point
  st_join(
    select(
      pl, 
      owner = d_Own_Type, manager = d_Mang_Nam, pa_name = Unit_Nm, geometry)) %>%
  # Reclassify ownership to public | easement | unprotected
  mutate(
    owner = case_when(
      owner %in% c("Local Government", "State", "Non-Governmental Organization", "Federal") ~ "Public",
      owner == "Private" ~ "Private easement",
      is.na(owner) ~ "Private unprotected",
      TRUE ~ owner))

# Join bird survey visit data
visits <-
  bind_rows(
    read_rds("data/processed/birds.rds")$visits %>% 
      mutate(
        point_id = as.character(grts),
        n_observers = 1), 
    read_rds("data/processed/vwl_birds.rds")$visits) %>% 
    select(-dttm, -grts)

# Join bird count data
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
    "NOHA", "LEOW", "SEOW",
    "HOLA", "SEWR", "AMPI", 
    "VESP", "SAVS", "GRSP", "HESP", "LALO",
    "DICK", 
    "BOBO", "EAME")

sp_fac <-
  c(
    "AMBI", "CAEG", "TUVU", "CANG", "MALL",
    "AMKE", "MERL", "PEFA", "WTKI",
    "NOBO", "SACR", "KILL", "LEYE",
    "MODO", "BANO", "CONI",
    "EAKI", "LOSH", "EABL", 
    "BLVU", # Flagged as fac in south america by vickery?
    "COYE", "RWBL", "BHCO")

# Maximum of 27 grassland species possible. 26 if exclude WTKI
birds %>% 
  filter(incidental == 0) %>% 
  left_join(visits) %>% 
  filter(species %in% c(sp_ob, sp_fac)) %>% 
  group_by(point_id) %>% summarize(sp = length(unique(species))) %>% 
  left_join(points) %>% group_by(owner) %>% summarize(mean_sp = mean(sp))




