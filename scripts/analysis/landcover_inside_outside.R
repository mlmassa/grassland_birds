# Landcover within and outside parks


# setup -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)

nlcd_raw <-
  rast('data/raw/NLCD_2019_Land_Cover_crop.tiff')

parks <-
  vect('data/processed/focal_parks.shp') %>% 
  # Projection to match NLCD
  project(nlcd_raw)


# terra methods -----------------------------------------------------------

# Create reclass tibble

reclass_key <-
  tibble(
    nlcd_value = terra::unique(nlcd_raw) %>% pull(1)) %>% 
    mutate(
      landcover = 
        case_when(
          nlcd_value %in% c(90, 95) ~ 'wetland',
          nlcd_value %in% c(22, 23, 24) ~ 'developed',
          nlcd_value %in% c(52, 71, 81) ~ 'grassland',
          nlcd_value %in% 41:43 ~ 'forest',
          nlcd_value == 82 ~ 'cropland',
          TRUE ~ 'other'),
      new_value = 
        case_when(
          landcover == 'wetland' ~ 1,
          landcover == 'developed' ~ 2,
          landcover == 'grassland' ~ 3,
          landcover == 'forest' ~ 4,
          landcover == 'cropland' ~ 5,
          landcover == 'other' ~ 6))

# Reclassify raster

nlcd_reclass <-
  classify(nlcd_raw,
           rcl = 
             reclass_key %>%
             select(nlcd_value, new_value) %>% 
             as.matrix())


# inside ------------------------------------------------------------------


# Extract the park contents

extraction <-
  terra::extract(nlcd_reclass, parks) %>% 
  as_tibble()

# Summarize results

landcover_by_park <-
  extraction %>% 
  group_by(ID, Layer_1) %>% 
  summarize(n = n()) %>% 
  # Add park names
  mutate(park = factor(ID, levels = 1:4, labels = parks$UNIT_CODE)) %>% 
  select(park,
         new_value = Layer_1,
         n) %>% 
  left_join(
    reclass_key %>% 
      select(new_value, landcover) %>% 
      distinct(),
    by = 'new_value') %>% 
  select(park, landcover, n) %>% 
  mutate(total = sum(n),
         prop = n/total)

# Graph it

landcover_by_park %>% 
  ggplot(
    aes(
      x = park,
      y = prop,
      fill = factor(landcover, 
                    levels = c('other', 'wetland', 'developed', 'forest', 'cropland', 'grassland')))) +
  geom_col() +
  scale_fill_manual(
    values = c(
              '#fac0e3',  # other
              '#6eadc2', # wetland
              '#242424', # developed
              '#4d8552', # forest
               '#ad9839', # cropland
               '#b3cf59'  # grassland
               )) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = 'park', y = 'proportion within park', fill = 'landcover') +
  theme_classic()





# outside -----------------------------------------------------------------

parks_buffer <-
  # 2km upper end of dispersal recorded in GRSP 
  # (see Miller 2005 in FL field naturalist, anthony et al 2013 in WJO)
  # 5km arbitrary to enhance differences
  buffer(parks, width = 5000) %>% 
  erase(parks)

plot(parks_buffer)

# Extract the park contents

extraction_outside <-
  terra::extract(nlcd_reclass, parks_buffer) %>% 
  as_tibble()

# Summarize results

landcover_outside_park <-
  extraction_outside %>% 
  group_by(ID, Layer_1) %>% 
  summarize(n = n()) %>% 
  # Add park names
  mutate(park = factor(ID, levels = 1:4, labels = parks$UNIT_CODE)) %>% 
  select(park,
         new_value = Layer_1,
         n) %>% 
  left_join(
    reclass_key %>% 
      select(new_value, landcover) %>% 
      distinct(),
    by = 'new_value') %>% 
  select(park, landcover, n) %>% 
  mutate(total = sum(n),
         prop = n/total)

# Graph it

landcover_outside_park %>% 
  ggplot(
    aes(
      x = park,
      y = prop,
      fill = factor(landcover, 
                    levels = c('other', 'wetland', 'developed', 'forest', 'cropland', 'grassland')))) +
  geom_col() +
  scale_fill_manual(
    values = c(
              '#fac0e3',  # other
              '#6eadc2', # wetland
              '#242424', # developed
              '#4d8552', # forest
               '#ad9839', # cropland
               '#b3cf59'  # grassland
               )) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = 'park', y = 'proportion within 5km buffer of park', fill = 'landcover') +
  theme_classic()


# join --------------------------------------------------------------------

landcover <-
  landcover_by_park %>% 
    select(-n) %>% 
    left_join(
      landcover_outside_park %>% 
        select(-n), 
      by = c('park', 'ID', 'landcover'),
             suffix = c('_inside', '_outside'))

# No results from t test. There are only 4 parks

landcover %>% 
  pivot_longer(cols = 4:7, names_to = 'measure', values_to = 'value') %>% 
  filter(measure %in% c('prop_inside', 'prop_outside')) %>% 
  ggplot(aes(x = measure, y = value, fill = landcover)) +
    geom_col() +
  facet_wrap(~park)
