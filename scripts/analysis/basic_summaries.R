
# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
#library(rebird)
library(sf)
library(tmap)

# Import bird data

read_rds('data/processed/birds.rds') %>% 
  list2env(.GlobalEnv)

taxonomy <-
  read_rds('data/processed/taxonomy.rds')

pif_stats <-
  read_csv('data/raw/ACAD Global 2021.02.05-filtered.csv') %>%
  select(
    common_name = `Common Name`,
    global_pop = `Global Pop Size#`,
    pop_change = `Pop Change`,
    pop_change_90_lcl = `Pop Change 90% lcl`,
    pop_change_90_ucl = `Population Change 90% ucl`,
    hab_breeding = `Primary Breeding Habitat`,
    watchlist = CI)


# Import covariate data

static_covs <-
  read_rds('data/processed/static_covs.rds')

annual_covs <- 
  read_rds('data/processed/annual_covs.rds')


# Build untidy bird data

birds <-
  counts %>% 
  left_join(visits) %>% 
  left_join(points) %>% 
  filter(visit == 1 | visit == 2, flyover == 0)


# Import park shapes

parks <-
  st_read('data/processed/focal_parks.shp') %>% 
  select(
    park = UNIT_CODE,
    park_name = PARKNAME)

# Set focal species
focal_spp <- 
  c('GRSP', 'EAME')


# Calculate annual totals -------------------------------------------------

# Annual totals for all species in all parks:

annual_props <-
  birds %>% 
    group_by(year, park, species) %>% 
    # Total count and points detected:
    summarize(
      abundance = n(),
      points_detected = length(unique(point_name))) %>% 
    # Add total points for park-year
    left_join(
      birds %>% 
        group_by(park, year) %>% 
        summarize(total_points = length(unique(point_name)))) %>% 
    # calculate annual prop. points detected
    mutate(prop_points = points_detected / total_points) %>% 
    select(park, species, prop_points) %>% 
  ungroup()

# Plot mean prop points
annual_props %>% 
  filter(species %in% focal_spp) %>% 
  group_by(park, species) %>% 
  summarize(mean_prop_points = mean(prop_points)) %>% 
  ggplot(aes(x = park, 
             y = mean_prop_points, 
             fill = species)) +
  geom_col(position = 'dodge',
           color = 'black') +
  #scale_fill_manual(values = c('goldenrod', 'pink', 'green', 'black')) +
  scale_fill_brewer(palette = "Greys") +
  labs(y = 'Mean proportion\nof sites occupied',
       x = 'Park',
       fill = 'Species') +
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  theme_classic() +
  theme(legend.position = c(0.85, 0.8))

ggsave('output/plots/mean_prop_points_parkxspecies.png', 
       width = 5, height = 4, units = 'in')


# All grass point species (to note ones of concern)

counts %>% 
  select(species) %>% 
  unique() %>% 
  left_join(
    taxonomy,
    by = 'species') %>% 
  arrange(tax_order) %>% 
  write_csv('data/processed/all_grassland_species.csv')

counts %>% 
  left_join(visits) %>% 
  left_join(points) %>% 
  left_join(parks) %>% 
  select(species, park) %>% 
  distinct() %>% 
  left_join(
    taxonomy,
    by = 'species') %>% 
  arrange(tax_order) %>% 
  filter(tax_level == 'species') %>% 
  mutate(X = 'X') %>% 
  pivot_wider(names_from = park, values_from = X) %>% 
  select(common_name, sci_name, ANTI, MANA, MONO, HAFE) %>% 
  write_csv('data/processed/grassland_species_by_park.csv')

# Show annual variables (ex. disurb) --------------------------------------

counts %>% 
  group_by(visit_id) %>% 
  summarize(
    species = length(unique(species))) %>% 
  left_join(visits %>% 
              select(visit_id, grts, year, 
                     visit, temperature, disturbance)) %>% 
  left_join(points %>% select(grts, park)) %>% 
  filter(visit == 1 | visit == 2) %>% 
  group_by(park, year) %>% 
  summarize(
    points = length(unique(grts)),
    mean_species = mean(species, na.rm = T),
    mean_disturb = mean(disturbance, na.rm = T)) %>% 
  ggplot(aes(x=year, y = mean_disturb, color = park)) +
  geom_point() + geom_line()

# Get species over the season ---------------------------------------------

counts %>% 
  group_by(visit_id) %>% 
  summarize(
    species = length(unique(species))) %>% 
  left_join(visits %>% select(visit_id, visit, date)) %>% 
  mutate(week = week(date)) %>% 
  ggplot(
    aes(x = week,
        y = species,
        color = visit)) +
  geom_jitter() +
  geom_smooth(method = lm)
  
# Plot mean abundance by park ---------------------------------------------

#abundance_park_plot <-

# Summary table of total effort and sp. by park:

  birds %>% 
    group_by(park) %>% 
    summarize(
      survey_points = length(unique(point_name)),
      total_species = length(unique(species)),
      total_surveys = length(unique(visit_id)),
      years_surveyed = length(unique(year))) %>% 
    # Add number of focal species per park
    left_join(
      birds %>% 
        group_by(park, species) %>% 
        summarize(
          abund = sum(bird_count)) %>% 
        filter(species %in% focal_species),
      by = 'park') %>% 
    mutate(
      mean_abund = abund / total_surveys) %>%
  
    #Plot it
    ggplot(aes(x = species, y = mean_abund, fill = park)) +
    geom_col(position = 'dodge') +
    labs(
      y = 'Mean annual abundance\n(# birds/survey)', 
      x = '', 
      title = 'Most grassland bird species are least abundant at Monocacy',
      fill = '') +
    scale_x_discrete(
      labels = c(
        'Eastern Meadowlark', 
        'Field Sparrow', 
        'Grasshopper Sparrow', 
        'Red-winged Blackbird')) +
    scale_fill_manual(
      labels = c('Antietam National Battlefield',
                 'Harpers Ferry National Historical Park',
                 'Manassas National Battlefield Park',
                 'Monocacy National Battlefield'),
      values = c('#f2c75a', '#d49b3f', '#b3652e', '#782f0d')) +
    scale_y_continuous(
      breaks = seq(0, 1.75, 0.5),
      expand = c(0,0)) +
  coord_flip() +
    theme_minimal() +
    theme(
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 14, hjust = 0.5),
      legend.position = c(0.7, 0.5))

# export plot

ggsave(filename = 'plot_park_abundance_horizontal.png',
       path = 'output/plots',
       width = 6,
       height = 7,
       units = 'in')

write_rds(abundance_park_plot, 'output/plots/plot_park_abundance.rds')



# Plot focal detections by year -------------------------------------------

# Plot of focal sp. detections by park/year:

plot_annual_detections <-
  
  # Conduct summaries for graph:
  annual_props %>% 
    filter(species %in% c('GRSP', 'EAME', 'FISP', 'RWBL')) %>%
    mutate(
      park = factor(park,
                    levels = c('ANTI', 'HAFE', 'MANA', 'MONO'),
                    labels = c('Antietam National Battlefield', 
                               'Harpers Ferry National Historical Park', 
                               'Manassas National Battlefield Park', 
                               'Monocacy National Battlefield')),
      species = factor(species,
                       levels = c('GRSP', 'FISP', 'EAME', 'RWBL'),
                       labels = c('Grasshopper Sparrow', 
                                  'Field Sparrow',
                                  'Eastern Meadowlark',
                                  'Red-winged Blackbird'))) %>% 
      
    # Plot how often species was detected each year
    ggplot(
      aes(x = year,
          y = prop_points,
          color = park,
          shape = park)) +
    geom_line() +
    geom_point(size = 3) +
    scale_shape_manual(values = c(15, 17, 19, 4)) +
    facet_wrap(
      ~ species, 
      nrow = 2) +
    
    # Labels
    labs(
      title = 'Naïve occupancy of focal species varied across years',
      x = 'Year',
      y = 'Proportion points with 1+ detections',
      color = '',
      shape = '') +
    scale_y_continuous(
      breaks = seq(0, 1, 0.2),
      expand = c(0.04, 0.04)) +
    
    # Theme elements
    theme(
          axis.ticks.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 14, hjust = 0.5),
          axis.text = element_text(size = 10),
          legend.text = element_text(size = 11),
          legend.key = element_rect(color = NA, fill = NA),
          legend.position = 'bottom',
          panel.grid = element_line(color = "#e9e9e9"),
          panel.grid.minor = element_line(linetype = 'dashed'),
          axis.line = element_line(color = 'black'),
          panel.background = element_rect(fill = 'white', color = 'black'),
          strip.background = element_rect(color = 'black', fill = 'black'),
          strip.text = element_text(size = 12, color = 'white')) +
    guides(shape = guide_legend(nrow = 2, byrow = TRUE))
  

# export plot

ggsave(filename = 'plot_annual_detections.png',
       path = 'output/plots',
       width = 6,
       height = 7,
       units = 'in')

# export plot for SciViz class

ggsave(filename = 'plot_annual_detections.pdf',
       path = 'output/plots',
       width = 7,
       height = 7,
       units = 'in',
       useDingbats = FALSE)

write_rds(plot_annual_detections, 'output/plots/plot_annual_detections.rds')



# Landscape covariates by park --------------------------------------------
land_covs <-
  static_covs %>% 
  pivot_longer(
    cols = crp_250:grs_5000, 
    names_to = "variable", 
    values_to = "value") %>% 
  select(grts, park, variable, value) %>% 
  mutate(
    value = value * 100,
    scale = readr::parse_number(variable),
    landcover = 
      str_extract(
        string = variable, 
        pattern = '[a-z][a-z][a-z]') %>% 
      factor(
        labels = c("Cropland", "Developed", "Forest", "Grassland")))

# Anova of park landcovers ------------------------------------------------

land_covs %>% 
  left_join(st_drop_geometry(parks)) %>% 
  ggplot(aes(x = landcover, y = value)) +
  geom_jitter(aes(color = landcover), alpha = 0.8, width = 0.3) +
  geom_boxplot(fill = NA, outlier.shape = NA) +
  facet_grid(rows = vars(scale), cols = vars(park_name)) +
  labs(y = 'Percent landcover') +
  theme_bw() +
  theme(
    legend.position = 'none',
    axis.text.x = element_text(size = 7))

ggsave('output/plots/plot_landcoverxscalexpark.png', width = 7, height = 8, units = 'in')

# Plot field type by park -------------------------------------------------

field_type_plot <-
  point_covs %>% 
    filter(!is.na(field_type), 
           field_type != 'Woods') %>% 
    ggplot(
      aes(x = Park, 
          fill = field_type)) +
    scale_fill_discrete(
      type = c('#BFE3B6', '#63AB60', '#334D2E')) +
    geom_bar(position = 'stack') +
    labs(
      y = 'Number of points', 
      x = '', 
      title = 'Distribution of points across field types',
      fill = '') +
    scale_x_discrete(
      labels = c(
        'Antietam', 
        'Harpers\nFerry', 
        'Manassas', 
        'Monocacy')) +
    scale_y_continuous(
      breaks = seq(0, 100, 20)) +
    theme_bw() +
    theme(
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.title.align = 0.5,
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = c(0.85, 0.85))

# export plot

ggsave(field_type_plot,
       filename = 'plot_park_fieldtype.png',
       path = 'output/plots',
       width = 7.5,
       height = 5,
       units = 'in')


# Table of field type totals: pretty equal overall
point_covs %>% 
  filter(!is.na(field_type), 
         field_type != 'Woods') %>% 
  group_by(field_type) %>% 
  summarize(count = n())


# Table for thesis of covariates ------------------------------------------

# Detection covariates: doy, dist, wind, temp, humidity, tsun, observer
# Range, mean(se)
det_tabler <-
  visits %>% 
    filter(visit == 1 | visit == 2) %>% 
    mutate(sky = as.factor(sky), 
           wind = as.factor(wind), 
           disturbance= as.factor(disturbance), 
           observer = as.factor(observer), 
           doy = lubridate::yday(date)) %>% 
    select(
      `Day of year (Jan 1 = Day 1)` = doy, 
      `Disturbance; categorical` = disturbance, 
      `Wind (Beaufort scale); categorical` = wind, 
      `Temperature (C)` = temperature, 
      `Percent humidity` = humidity, 
      `Time since sunrise (minutes)` = start_sun, 
      `Observer; categorical` = observer) 

summary_table_detections <- 
  det_tabler %>% 
    mutate(`Temperature (C)` = na_if(`Temperature (C)`, 0),
           `Percent humidity` = na_if(`Percent humidity`, 0)) %>% 
    summarize(
      across(where(is.numeric), 
             list(
               mean = ~mean(.x, na.rm = TRUE),
               min = ~min(.x, na.rm = TRUE),
               max = ~max(.x, na.rm = TRUE),
               se = ~sd(.x, na.rm = TRUE)/sqrt(length(.x))))
      ) %>% 
    pivot_longer(everything(), 
                 names_to = c('var', 'metric'),
                 names_sep = '_') %>% 
    pivot_wider(names_from = metric, values_from = value) %>% 
    mutate(
      Range = paste0(signif(min, 3), ' - ', signif(max, 3)),
      `Mean (SE)` = paste0(signif(mean, 3), ' (', signif(se, 3), ')')) %>% 
    select(`Detection variables` = var, Range, `Mean (SE)`)

point_covs %>% 
  mutate(
    across(4:15, ~ .x*100)) %>% 
  group_by(Park) %>%
  summarize(
    across(where(is.numeric),
      list(mean = ~signif(mean(.x, na.rm = TRUE),3),
           se = ~signif(sd(.x, na.rm = TRUE)/sqrt(length(.x)), 3)),
           .names = '{.col}ww{.fn}')) %>% 
  pivot_longer(!Park,
               names_to = c('var', 'metric'),
               names_sep = 'ww') %>% 
  pivot_wider(names_from = metric, values_from = value) %>% 
  mutate(concat = paste0(mean, ' (', se, ')')) %>% 
  filter(!var %in% c('ever_burned', 'trees', 'clin_mean', 'leased', 'harvest_limit')) %>% 
  select(Park, var, concat) %>% 
  pivot_wider(names_from = Park, values_from = concat)


point_covs %>% group_by(Park, field_type) %>% summarize(n = n())
  
point_covs %>% group_by(Park, ever_burned) %>% summarize(n = n())

point_covs %>% group_by(Park, leased) %>% summarize(n = n()) %>% filter(leased == 1)

point_covs %>% group_by(Park, harvest_limit) %>% summarize(n = n()) %>% filter(harvest_limit == 1)


# Occupancy: 
# hab (field, shrub, angle)
# land (g f d w, 500 1 5)
# mgmt (yr burn lease limit harvestday park)
# Range, mean(se) BY PARK
