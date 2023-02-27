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
  filter(
    !NAME %in% c('Alaska', 'Puerto Rico', 'Hawaii', 
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

# Reversed inset map ------------------------------------------------------

region_palette <-
  c(
    'gray50', # pacwest
    'gray80', # mtn
    'gray35', # midwest
    'gray70', # se
    'gray95', # ne
    '#f0b25b' #cap
    )

# Nationwide scale map
national_map <-
  ggplot() +
    # NPS regions (outline)
      geom_sf(
        data = nps_regions %>% 
          group_by(Region) %>% summarize() %>% 
          st_transform(crs = crs_far), 
        aes(fill = Region), 
        color = "black",
        size = 0.25) +
      scale_fill_discrete(type = region_palette) +
    # US state borders
      geom_sf(
        data = us %>% st_transform(crs = crs_far), 
        size = 0.25, 
        fill = NA,
        color = alpha('black', 0.2)) +
    # US outline
      geom_sf(
        data = us_outline,
        fill = NA,
        color = "black",
        size = 0.3) +
  # Edit legend
  labs(fill = "NPS Region") +
  theme(
    legend.position = "top", 
    legend.direction = "horizontal") +
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      title.theme = element_text(face = "bold")))

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
    st_transform(crs = crs_close))[c(1,3)] * c(0.95, 1.05)

region_ylims <-
  st_bbox(
    nps_regions %>% 
    filter(Region == 'National Capital') %>% 
    st_transform(crs = crs_close))[c(2,4)] * c(0.9, 1)
  

regional_map <-
  ggplot() +
    # Base color for states
      geom_sf(
        data = us %>% st_transform(crs = crs_close),
        fill = 'gray95',
        color = NA) +
    # NCR base
      geom_sf(
        data = nps_regions %>% 
          group_by(Region) %>% 
          summarize() %>% 
          filter(Region == 'National Capital') %>% 
        st_transform(crs = crs_close),
        fill = region_palette[6], 
        alpha = 0.25, 
        color = NA) +
    # NCR highlight
      geom_sf(
        data = nps_regions %>% 
          group_by(Region) %>% 
          summarize() %>% 
          filter(Region == 'National Capital') %>% 
        st_transform(crs = crs_close),
        fill = NA, 
        color = region_palette[6], size = 2) +
    # Urban areas
      geom_sf(
        data = urban,
        fill = "gray30", 
        alpha = 0.15, 
        color = NA) +
    # States for context
      geom_sf(
        data = us %>% st_transform(crs = crs_close),
        fill = NA,
        color = "gray30",
        size = 0.25,
        alpha = 0.8) +
    # Focal parks
      #geom_sf(data = park_centroids, size = 3) +
    # ALT: Park outlines
      geom_sf(
        data = park_boundaries, 
        fill = 'black', 
        color = "black",
        size = 0.1) +
    # Set scale
      coord_sf(
        xlim = region_xlims,
        ylim = region_ylims,
        expand = TRUE) +
    # Park labels
      ggrepel::geom_text_repel(
        data = park_boundaries,
        aes(
            # label = PARKNAME,
            ## Alternative:
            label = paste0(PARKNAME, '\n', UNIT_TYPE),
            geometry = geometry),
        stat = 'sf_coordinates',
        size = 4,
        point.padding = 0.5,
        segment.color = NA) +
  # Scale bar
  ggsn::scalebar(
    data = 
      nps_regions %>% 
      filter(Region == 'National Capital') %>% 
      st_buffer(dist = 9000) %>% 
      st_transform(crs = crs_close),
    dist = 25, dist_unit = "km",
    transform = F,
    border.size = 0.5)


national_map_focus <-
  national_map +
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
      color = 'red',
      size = 0.7)

inset_map_reverse <-
  regional_map %>% 
    ggdraw() +
    draw_plot(
      {
        national_map_focus +
          theme(
            plot.title = element_text(size = 10),
            plot.background = element_rect(fill = "white", color = "black", size = 1),
            plot.margin = margin(t = 5, l = 4, r = 4),
            legend.text = element_text(size = 7),
            legend.margin = margin(),
            legend.key.size = unit(7, "points"))
      },
      x = 0.01, y = 0.05, 
      width = 0.5, height = 0.45)

inset_map_reverse


ggsave('output/plots/inset_map.pdf',
       plot = inset_map_reverse,
       width = 6.5,
       height = 4,
       units = 'in')

