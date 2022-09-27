# This script makes maps of the parks/NPS regions.
# Setup -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(tigris)
library(cowplot)

# Download of shapefile: Keep download and cache to speed things up.
options(tigris_use_cache = TRUE, keep_zipped_shapefile = TRUE)

# Set projections to use
crs_far <- 5070 # NAD83/ Conus Albers
crs_close <- 26985 #NAD83/ Maryland

# Import parks of interest
focal_parks <- # NAD83
  st_read('data/processed/focal_parks.shp')

# Get park centroids (close view only)
park_centroids <- 
  focal_parks %>% 
    group_by(PARKNAME) %>% 
    st_transform(crs = crs_close) %>% 
    st_centroid()

# Transform park to close crs
park_boundaries <- # NAD83
  focal_parks %>% 
    st_transform(crs = crs_close)

# Import US states
us <- # NAD83
  states(cb = TRUE, resolution = '5m') %>% 
  filter(!NAME %in% c('Alaska', 'Puerto Rico', 'Hawaii', 
                      'American Samoa', 
                      'Commonwealth of the Northern Mariana Islands', 
                      'United States Virgin Islands', 'Guam'))
# Outline of USA
us_outline <-
  us %>% st_union()

# Import urban areas
urban <-
  urban_areas() %>% 
    filter(str_detect(string = NAME10, pattern = 'VA|WV|MD|DE|DC|PA')) %>% 
    st_transform(crs = crs_close)

# Import NPS regions
nps_regions <- # WGS-84 / Pseudo-Mercator
  st_read('data/raw/National_Park_Service___Regional_Boundaries.shp') %>% 
  filter(!Region %in% c('Alaska')) %>% 
  mutate(Region = 
           factor(
             Region, 
             levels = c('Alaska', 'Pacific West', 'Intermountain', 
                        'Midwest','Southeast', 'Northeast', 
                         'National Capital'))) %>%
  st_transform(crs = st_crs(us)) %>% 
  st_intersection(us)

# Set theme
theme_set(theme_void())

# Make basic maps ---------------------------------------------------------


# Nationwide scale map
national_map <-
  ggplot() +
    # NPS regions
      geom_sf(data = nps_regions %>% 
                st_transform(crs = crs_far), 
              aes(fill = Region), 
              color = NA) +
      scale_fill_brewer(palette = 'Set3', direction = 1) +
    # US state borders
      geom_sf(data = us %>% 
                st_transform(crs = crs_far), 
              size = 0.25, 
              fill = NA,
              color = alpha('gray20', 0.3)) +
    # US outline
      geom_sf(data = us_outline,
              fill = NA)

# Region scale map
regional_map <-
  ggplot() +
    # Base color for states
      geom_sf(data = us %>%
                st_transform(crs = crs_close),
              fill = 'gray90',
              color = NA) +
    # NCR
      geom_sf(data = nps_regions %>% 
                filter(Region == 'National Capital') %>% 
                st_transform(crs = crs_close),
              fill = '#fdb462', color = '#fdb462') +
    # States for context
      geom_sf(data = us %>%
                st_transform(crs = crs_close),
              fill = NA,
              size = 0.25) +
    # Focal parks
      geom_sf(data = park_centroids, size = 3) +
    # Set scale
      coord_sf(
        xlim = st_bbox(nps_regions %>% 
                filter(Region == 'National Capital') %>% 
                st_transform(crs = crs_close))[c(1,3)],
        ylim = st_bbox(nps_regions %>% 
                filter(Region == 'National Capital') %>% 
                st_transform(crs = crs_close))[c(2,4)],
        expand = TRUE) +
    # Park labels
      ggrepel::geom_text_repel(
        data = park_centroids,
        aes(
            label = PARKNAME,
            ## Alternative: Harpers Ferry (HAFE). Takes up more room.
            # label = paste0(PARKNAME, '\n(', UNIT_CODE, ')'), 
            geometry = geometry),
        stat = 'sf_coordinates',
        size = 4,
        segment.color = NA)
  

# Get bbox of national map (will be expanding it later)
national_bbox <-
  nps_regions %>% 
    st_transform(crs = crs_far) %>% 
    st_bbox()

# Create focused national map (bbox, larger)
national_map_focus <-
  national_map +
  theme(legend.position = c(0, 0),
        legend.direction = 'horizontal',
        legend.justification = 'left',
        legend.key.size = unit(0.3, 'cm')) +
  # Add bbox for inset
    geom_sf(
      data = 
        st_bbox(
          nps_regions %>% 
            filter(Region == 'National Capital') %>% 
            st_transform(crs = crs_far) %>% 
            st_buffer(dist = 30000)) %>% 
        st_as_sfc(),
      fill = NA,
      color = 'black',
      size = 0.9) +
  # Expand map to right to make room for inset
    coord_sf(xlim = c(national_bbox[1], national_bbox[3] * 1.3),
             ylim = c(national_bbox[2], national_bbox[4]))

inset_map <-
  national_map_focus %>% 
    ggdraw() +
    draw_plot(
      {regional_map +
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 0.9))},
      x = 0.75, y = 0.17, 
      width = 0.25, height = 0.25)

# Reversed inset map ------------------------------------------------------

region_palette <-
  c(
    '#ebb1a4', # pacwest
    '#C8B08D', # mtn
    '#DBE6C1', # midwest
    '#B3C495', # se
    '#8297c2', # ne
    '#fdb462' #cap
    )

# Nationwide scale map
national_map <-
  ggplot() +
    # NPS regions (outline)
      geom_sf(data = nps_regions %>% 
                st_transform(crs = crs_far), 
              aes(fill = Region),
              color = NA) +
      scale_fill_discrete(type = region_palette) +
    # NPS regions (NCRN fill)
      geom_sf(data = nps_regions %>% 
                filter(Region == 'National Capital') %>% 
                st_transform(crs = crs_far), 
              color = '#fdb462', fill = '#fdb462') +
    # US state borders
      geom_sf(data = us %>% 
                st_transform(crs = crs_far), 
              size = 0.2, 
              fill = NA,
              color = alpha('black', 0.2)) +
    # US outline
      geom_sf(data = us_outline,
              color = 'black',
              fill = NA)

# Get bbox of national map (will be expanding it later)
national_bbox <-
  nps_regions %>% 
    st_transform(crs = crs_far) %>% 
    st_bbox()

# Region scale map

region_xlims <- 
  st_bbox(
    nps_regions %>% 
    filter(Region == 'National Capital') %>% 
    st_transform(crs = crs_close))[c(1,3)] * c(0.9, 1.1)

region_ylims <-
  st_bbox(
    nps_regions %>% 
    filter(Region == 'National Capital') %>% 
    st_transform(crs = crs_close))[c(2,4)] * c(0.9, 1.03)
  

regional_map <-
  ggplot() +
    # Base color for states
      geom_sf(data = us %>%
                st_transform(crs = crs_close),
              fill = 'gray90',
              color = NA) +
    # NCR
      geom_sf(data = nps_regions %>% 
                filter(Region == 'National Capital') %>% 
                st_transform(crs = crs_close),
              fill = '#fdb462', color = '#fdb462') +
    # Urban areas
      geom_sf(data = urban,
              fill = 'gray20', alpha = 0.2, color = NA) +
    # States for context
      geom_sf(data = us %>%
                st_transform(crs = crs_close),
              fill = NA,
              size = 0.3) +
    # Focal parks
      #geom_sf(data = park_centroids, size = 3) +
    # ALT: Park outlines
      geom_sf(data = park_boundaries, fill = 'black', color = NA) +
    # Set scale
      coord_sf(
        xlim = region_xlims,
        ylim = region_ylims,
        expand = TRUE) +
    # Park labels
      ggrepel::geom_text_repel(
        data = park_boundaries,
        aes(
            label = PARKNAME,
            ## Alternative: "Harpers Ferry (HAFE)". Takes up more room.
            # label = paste0(PARKNAME, '\n(', UNIT_CODE, ')'), 
            geometry = geometry),
        stat = 'sf_coordinates',
        size = 4,
        point.padding = 0.25,
        segment.color = NA)


national_map_focus <-
  national_map +
  labs(fill = 'NPS Region') +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.position = 'top',
        legend.direction = 'vertical',
        legend.justification = 'center',
        legend.title.align = 0.5,
        legend.spacing.y = unit(0.2, 'cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.key.size = unit(0.4, 'cm')) +
  # Add bbox for inset
    geom_sf(
      data = 
        st_bbox(
          nps_regions %>% 
            filter(Region == 'National Capital') %>% 
            st_transform(crs = crs_far) %>% 
            st_buffer(dist = 70000)) %>% 
        st_as_sfc(),
      fill = NA,
      color = 'firebrick',
      size = 0.9)

inset_map_reverse <-
  regional_map %>% 
    ggdraw() +
    draw_plot(
      {national_map_focus +
          theme(
            # panel.border = element_rect(color = "black", fill = NA, size = 0.75),
            panel.background = element_rect(color = 'white'),
            legend.background = element_rect(color = 'white'))},
      x = 0.01, y = 0.05, 
      width = 0.5, height = 0.5)

inset_map_reverse


ggsave('output/plots/inset_map.pdf',
       plot = inset_map,
       width = 6.5,
       height = 4.5,
       units = 'in')

