# Chapter 2.2: Analysis of protected areas and grassland diversity

# Setup -------------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(glue)
library(sf)
library(vegan)

# Read in data: points, visits, birds, species
read_rds("data/processed/ch2_combined_birds.rds") %>% 
  list2env(.GlobalEnv)

# Read in covariates
landcover <- read_rds("data/processed/ch2_pointcovs.rds")

# Remove all incidentals
birds <- 
  filter(birds, incidental == 0) %>% select(-incidental)

# Set theme for plotting
theme_set(theme_bw(base_size = 12))

# Set palette for fac/ob
pal <- c(facultative = "gray", obligate = "black")


# Explore sampling data ---------------------------------------------------

# How many grassland species?
nrow(sp_grs)

# What was the distribution of ownership over years?
visits %>% 
  left_join(points) %>% 
  distinct(point_id, year, owner) %>% 
  count(year, owner) %>% 
  ggplot(
    aes(x = year, y = n, fill = owner)) + 
    geom_col() + 
    scale_fill_brewer(palette = "Paired") + 
    scale_x_continuous(breaks = c(2012:2022)) +
  labs(fill = "Landowner", y = "Points surveyed", x = "") +
  theme(legend.position = "top")

# All species x visits
spxvisit <- expand(birds, visit_id, species)

# How many detections?
birds %>% 
  filter(species %in% sp_grs$species) %>% 
  count(species) %>% 
  left_join(
    sp_grs %>% 
    select(species, common_name, status)) %>% 
  # Plot
  mutate(
    styled_name = if_else(
      status == "obligate", 
      glue("<b>{common_name}</b>"),
      common_name)) %>% 
  ggplot(
    aes(
      x = n, 
      y = reorder(styled_name, n), 
      fill = status)) + 
    geom_col() + 
    labs(
      x = "Individuals detected", 
      y = "", 
      fill = "Grassland species:") + 
    scale_fill_manual(values = pal) +
  scale_x_continuous(
    limits = c(0, 5500), 
    expand = c(0,0)) +
  theme(
    axis.text.y = ggtext::element_markdown(),
    legend.position = "top")

## Rarefaction ----
# Make a species accumulation curve
visits %>% 
  group_by(point_id) %>% 
  arrange(date) %>% 
  mutate(total_visits = row_number()) %>%
  right_join(birds) %>% 
  group_by(point_id, total_visits) %>% 
  distinct(species) %>% 
  group_by(point_id) %>% 
  distinct(species, .keep_all = T) %>% 
  group_by(total_visits, point_id) %>% 
  filter(species %in% sp_grs$species) %>% 
  summarize(new_sp = length(unique(species))) %>% 
  arrange(point_id, total_visits) %>% 
  group_by(point_id) %>% 
  mutate(total_sp = cumsum(new_sp)) %>% 
  bind_rows(
    crossing(
      point_id = unique(visits$point_id),
      total_sp = 0, total_visits = 0)) %>% 
  left_join(points) %>% 
  ggplot() + 
    geom_line(
      aes(
      x = total_visits, 
      y = total_sp, 
      color = owner, 
      group = point_id),
      alpha = 0.5) +
    geom_smooth(
      aes(x = total_visits, y = total_sp, group = "owner"),
      method = NULL,
      formula = y ~ log(x+1),
      color = "black",
      se = F,
      fullrange = T) +
    scale_color_brewer(palette = "Paired") +
    theme(legend.position = "none") +
  facet_wrap(vars(owner)) +
    labs(
      x = "Total visits to point", 
      y = "Total grassland species", 
      title = "Species accumulation")

## Ob/fac abundance ----

# Abundance by survey (by status)
abund_bysurv <-
  birds %>% 
  left_join(sp_grs) %>% 
  left_join(visits) %>% 
  group_by(visit_id, status, point_id) %>% 
  summarize(n = n()) %>% 
  replace_na(list(status = "non-grassland")) %>% 
  left_join(points) %>% 
  group_by(owner, status) %>% 
  summarize(
    mean_birds_surv = mean(n), 
    se = sd(n)/sqrt(length(n)))

# Plot
ggplot(
  data = abund_bysurv,
  aes(
    x = factor(
      status, levels = c("obligate", "facultative", "non-grassland")), 
    y = mean_birds_surv, 
    fill = owner)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(
      ymin = mean_birds_surv - se, 
      ymax = mean_birds_surv + se),
    position = position_dodge()) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    x = "", 
    y = "Mean individuals per survey (SE)", 
    fill = "Landowner") +
  theme(legend.position = "top")

## Species abundance ----

# Most obligate spp. are more abundant on private lands than public except 
#   for GRSP, HOLA, VESP
# For BOBO, DICK, SAVS, LOSH the superiority of private lands could be
#   because those species' breeding ranges are more western than MANA/MONO/ANTI

abund_sp_bysurv <-
  birds %>% 
  left_join(visits) %>% 
  group_by(visit_id, species) %>% 
  summarize(n = n()) %>% 
  right_join(spxvisit) %>% 
  replace_na(list(n = 0)) %>% 
  left_join(sp_grs) %>% 
  left_join(visits) %>% 
  left_join(points) %>% 
  filter(species %in% c(sp_fac, sp_ob)) %>% 
  group_by(owner, common_name) %>% 
  summarize(
    mean_birds_surv = mean(n, na.rm = T), 
    se = sd(n, na.rm = T)/sqrt(length(n)),
    .groups = "drop") %>% 
  filter(!is.na(common_name)) %>% 
  left_join(sp_grs)

# Plot
abund_sp_bysurv %>% 
  mutate(
    styled_name = if_else(
      status == "obligate", 
      glue("<b>{common_name}</b>"),
      common_name)) %>% 
ggplot(
  aes(
    x = styled_name,
    y = mean_birds_surv, 
    fill = owner)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(
      color = owner,
      ymin = mean_birds_surv - se, 
      ymax = mean_birds_surv + se),
    position = position_dodge()) +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  labs(
    y = "Mean individuals per survey (SE)", 
    x = "", 
    fill = "Landowner",
    color = "Landowner") +
  facet_wrap(vars(styled_name), ncol = 4, scales = "free") +
  theme(
    strip.text = ggtext::element_markdown(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = c(1, 0),
    legend.direction = "horizontal",
    legend.justification = c(1,0))

## Property total richness ----

# Public properties had a higher richness altogether...
birds %>% 
  left_join(visits) %>% 
  left_join(points) %>% 
  group_by(owner) %>% 
  filter(species %in% c(sp_ob, sp_fac)) %>% 
  summarize(sp = length(unique(species)))

## Property standard richness ----

# And when standardized for effort, each public property had more grass spp...

oversampled <- 
  count(st_drop_geometry(points), property_id) %>% 
  filter(n > 3)

undersampled <-
  count(st_drop_geometry(points), property_id) %>% 
  filter(n < 3)

first_year <-
  visits %>% 
  left_join(points) %>% 
  group_by(point_id, owner) %>% 
  filter(!is.na(owner)) %>% 
  summarize(first = min(year), last = max(year))

pts_sample <-
  points %>% 
  filter(property_id %in% oversampled$property_id) %>% 
  group_by(property_id) %>% 
  slice_sample(n = 3, replace = F) %>% 
  bind_rows(
    filter(
      points,
      !property_id %in% oversampled$property_id,
      !property_id %in% undersampled$property_id)) %>% 
  pull(point_id)

survey_sample <-
  visits %>% 
  left_join(first_year) %>% 
  filter(
    year == first,
    point_id %in% pts_sample,
    visit <= 3)

rich_prop_1yr <-
  birds %>% 
  filter(
    visit_id %in% survey_sample$visit_id,
    species %in% c(sp_ob, sp_fac)) %>% 
  left_join(visits) %>% 
  left_join(points) %>% 
  group_by(property_id) %>% 
  summarize(sp = length(unique(species))) %>% 
  left_join(points) %>% 
  group_by(owner) %>% 
  summarize(
    mean_sp = mean(sp), 
    se = sd(sp)/sqrt(length(sp)))

# Plot
ggplot(
  data = rich_prop_1yr,
  aes(
    x = owner,
    y = mean_sp, 
    fill = owner)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(
      ymin = mean_sp - se, 
      ymax = mean_sp + se),
    position = position_dodge()) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    x = "", 
    y = "Mean richness by property (SE)", 
    fill = "Landowner")

## Survey richness ----

# Richness of obligates per survey
rich_bysurv <-
  bind_rows(
    obligate =
      birds %>% 
      filter(species %in% c(sp_ob)) %>% 
      group_by(visit_id) %>%  
      summarize(sp = length(unique(species))) %>% 
      left_join(visits) %>%
      left_join(points) %>% 
      group_by(owner) %>% 
      summarize(
        mean_sp = mean(sp), 
        se = sd(sp)/sqrt(length(sp))),
    facultative =
      birds %>% 
      filter(species %in% c(sp_fac)) %>% 
      group_by(visit_id) %>%  
      summarize(sp = length(unique(species))) %>% 
      left_join(visits) %>%
      left_join(points) %>% 
      group_by(owner) %>% 
      summarize(
        mean_sp = mean(sp), 
        se = sd(sp)/sqrt(length(sp))),
    all_grass =
      birds %>% 
      filter(species %in% c(sp_ob, sp_fac)) %>% 
      group_by(visit_id) %>%  
      summarize(sp = length(unique(species))) %>% 
      left_join(visits) %>%
      left_join(points) %>% 
      group_by(owner) %>% 
      summarize(
        mean_sp = mean(sp), 
        se = sd(sp)/sqrt(length(sp))),
    .id = "status")

# Plot
ggplot(
  data = rich_bysurv,
  aes(
    x = factor(
      status, 
      levels = c("obligate", "facultative", "all_grass"),
      labels = c("obligate", "facultative", "obligate +\nfacultative")), 
    y = mean_sp, 
    fill = owner)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(
      ymin = mean_sp - se, 
      ymax = mean_sp + se),
    position = position_dodge()) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    x = "", 
    y = "Mean species per survey (SE)", 
    fill = "Landowner")

## Point-year standard richness ----

# Richness per point-year (standard effort)
rich_ptyr <-
  bind_rows(
    obligate =
      birds %>% 
      left_join(visits) %>% 
      # Standard effort: 3 visits
      filter(
        visit <= 3, 
        species %in% c(sp_ob)) %>% 
      group_by(point_id, year) %>%  
      summarize(sp = length(unique(species))) %>% 
      left_join(points) %>% 
      group_by(owner) %>% 
      summarize(
        mean_sp = mean(sp), 
        se = sd(sp)/sqrt(length(sp))),
    facultative =
       birds %>% 
      left_join(visits) %>% 
      # Standard effort: 3 visits
      filter(
        visit <= 3, 
        species %in% c(sp_fac)) %>% 
      group_by(point_id, year) %>%  
      summarize(sp = length(unique(species))) %>% 
      left_join(points) %>% 
      group_by(owner) %>% 
      summarize(
        mean_sp = mean(sp), 
        se = sd(sp)/sqrt(length(sp))),
    all_grass = 
       birds %>% 
      left_join(visits) %>% 
      # Standard effort: 3 visits
      filter(
        visit <= 3, 
        species %in% c(sp_ob, sp_fac)) %>% 
      group_by(point_id, year) %>%  
      summarize(sp = length(unique(species))) %>% 
      left_join(points) %>% 
      group_by(owner) %>% 
      summarize(
        mean_sp = mean(sp), 
        se = sd(sp)/sqrt(length(sp))),
    .id = "status")

# Plot
ggplot(
  data = rich_ptyr,
  aes(
    x = factor(
      status, 
      levels = c("obligate", "facultative", "all_grass"),
      labels = c("obligate", "facultative", "obligate +\nfacultative")), 
    y = mean_sp, 
    fill = owner)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(
      ymin = mean_sp - se, 
      ymax = mean_sp + se),
    position = position_dodge()) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    x = "", 
    y = "Mean species per point per year (SE)", 
    fill = "Landowner")


# Explore covariate data --------------------------------------------------

# What is the distribution of area protected by radius and owner?
landcover %>% 
  left_join(points) %>% 
  mutate(radius_km = radius/1000) %>% 
  ggplot(
    aes(
      x = radius_km, y = protected, 
      group = interaction(point_id, owner),
      color = owner)) + 
  geom_line(alpha = 0.3) +
  scale_color_brewer(palette = "Paired") +
  facet_wrap(vars(owner)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    x = "Radius (km)", 
    y = "Prop. protected area", 
    fill = "Landowner") +
  theme(
    legend.position = "none",
    panel.grid = element_blank())

# Format data for vegan ---------------------------------------------------

abund <-
  left_join(birds, visits) %>% 
  count(visit_id, species) %>% 
  # Get zeroes
  right_join(spxvisit) %>% 
  replace_na(list(n = 0)) %>% 
  left_join(visits) %>% 
  filter(species %in% sp_grs$species) %>% 
  group_by(point_id, species) %>% 
  summarize(
    #max = max(n),
    sum = sum(n),
    #mean = mean(n)
    #visits = length(n)
    ) %>% 
  pivot_wider(names_from = "species", values_from = "sum")

# Another method: use rarefaction


# Calculate metrics and visualize -----------------------------------------

abund <- abund_b

data <-
  full_join(
    specnumber(
      column_to_rownames(abund, var = "point_id"))%>% 
      enframe(name = "point_id", value = "richness"),
    diversity(
      column_to_rownames(abund, var = "point_id")) %>% 
      enframe(name = "point_id", value = "shannon")) %>% 
  mutate(hill = exp(shannon)) %>% 
  #left_join(landcover) %>%
  left_join(points)
  
hist(data$richness)
data %>% distinct(point_id, richness, owner) %>% 
  filter(!is.na(owner)) %>% 
  ggplot(aes(x = richness, color = owner)) + 
  geom_density(size = 1) + 
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous(breaks = 0:max(data$richness)) +
  theme(panel.grid.minor = element_blank())
hist(data$diversity)
data %>% distinct(point_id, diversity, owner) %>% 
  filter(!is.na(owner)) %>% 
  ggplot(aes(x = diversity, color = owner)) + 
  geom_density(size = 1) + 
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous(breaks = 0:max(data$diversity)) +
  theme(panel.grid.minor = element_blank())

data %>% 
  filter(!is.na(radius)) %>% 
  # group_by(property_id) %>% 
  # sample_n(size = 100, replace = T) %>% 
  pivot_longer(
    cols = cropland:protected, 
    values_to = "proportion", 
    names_to = "metric") %>% 
  ggplot(
    aes(
      x = proportion, 
      y = diversity, 
      group = owner, 
      color = owner)) +
    geom_point(alpha = 0.15, size = 0.2) +
    facet_grid(rows = vars(metric), cols = vars(radius)) +
    scale_color_brewer(palette = "Paired") +
    geom_smooth(method = "lm", se = FALSE) +
  theme_bw(base_size = 15) +
  theme(panel.grid = element_blank())

data %>% 
  filter(!is.na(owner), radius == 1000) %>% 
  ggplot(aes(x = owner, y = richness, fill = owner)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Paired") +
  theme_bw(base_size = 15)

# Do owner types have different landcover

aov(
  formula = grassland ~ owner, 
  data = 
    filter(data, radius == 1000) %>% 
    group_by(property_id) %>% 
    sample_n(5, replace = T)) %>% 
  TukeyHSD()

# How many points
points %>% 
  count(owner) %>% st_drop_geometry() %>% 
  left_join(
  # How many visits per point
  visits %>% 
    count(point_id) %>% 
    left_join(points) %>% 
    group_by(owner) %>% 
    summarize(visits_per_point = mean(n), se = sd(n)/sqrt(length(n))))

