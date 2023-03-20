# Chapter 2.2: Analysis of protected areas and grassland diversity

# Setup -------------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(glue)
library(sf)
library(vegan)
library(AICcmodavg)

# Read in data: points, visits, birds, species
read_rds("data/processed/ch2_combined_birds.rds") %>% 
  list2env(.GlobalEnv)

# Read in covariates
landcover <- read_rds("data/processed/ch2_pointcovs.rds")

# Remove all incidentals
birds <- 
  filter(birds, incidental == 0) %>% select(-incidental)

# Set theme for plotting
theme_set(
  theme_bw(base_size = 12) +
  theme(strip.background = element_blank()))

# Set palette for fac/ob
pal <- c(facultative = "gray", obligate = "black")

radii <- unique(landcover$radius)

# Explore sampling data ---------------------------------------------------

# How many points
points %>% 
  count(owner) %>% st_drop_geometry() %>% 
  left_join(
  # How many visits per point
  visits %>% 
    count(point_id) %>% 
    left_join(points) %>% 
    group_by(owner) %>% 
    summarize(
      visits_per_point = mean(n), 
      se = sd(n)/sqrt(length(n))))

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

# All species x ptyr
spxptyr <- 
  birds %>% 
  left_join(visits %>% select(visit_id, point_id, year)) %>% 
  mutate(ptyr = str_c(point_id, year)) %>% 
  expand(ptyr, species)

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
      alpha = 0.6) +
    geom_smooth(
      aes(x = total_visits, y = total_sp, group = "owner"),
      method = NULL,
      formula = y ~ log(x+1),
      color = "black",
      size = 0.5,
      linetype = "dashed",
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
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  facet_wrap(vars(styled_name), ncol = 4, scales = "free") +
  theme(
    strip.text = ggtext::element_markdown(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = c(0.73, 0),
    #legend.direction = "horizontal",
    legend.justification = c(1,0),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()) 

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

# Get properties with more than 3 points
oversampled <- 
  count(st_drop_geometry(points), property_id) %>% 
  filter(n > 3)

# Get properties with less than 3 points
undersampled <-
  count(st_drop_geometry(points), property_id) %>% 
  filter(n < 3)

# Get first year property was surveyed
first_year <-
  visits %>% 
  left_join(points) %>% 
  group_by(point_id, owner) %>% 
  filter(!is.na(owner)) %>% 
  summarize(first = min(year), last = max(year))

# Take 3 points per property (exclude properties with <3 points)
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

# Only take surveys 1-3 in first year surveyed
# For a total of 3 surveys per property
survey_sample <-
  visits %>% 
  left_join(first_year) %>% 
  filter(
    year == first,
    point_id %in% pts_sample,
    visit <= 3)

rich_prop_1yr <-
  bind_rows(
    all_grass =
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
        se = sd(sp)/sqrt(length(sp))),
    obligate = 
      birds %>% 
      filter(
        visit_id %in% survey_sample$visit_id,
        species %in% c(sp_ob)) %>% 
      left_join(visits) %>% 
      left_join(points) %>% 
      group_by(property_id) %>% 
      summarize(sp = length(unique(species))) %>% 
      left_join(points) %>% 
      group_by(owner) %>% 
      summarize(
        mean_sp = mean(sp), 
        se = sd(sp)/sqrt(length(sp))),
    facultative = 
      birds %>% 
      filter(
        visit_id %in% survey_sample$visit_id,
        species %in% c(sp_fac)) %>% 
      left_join(visits) %>% 
      left_join(points) %>% 
      group_by(property_id) %>% 
      summarize(sp = length(unique(species))) %>% 
      left_join(points) %>% 
      group_by(owner) %>% 
      summarize(
        mean_sp = mean(sp), 
        se = sd(sp)/sqrt(length(sp))),
    .id = "status")

# Plot
ggplot(
  data = rich_prop_1yr,
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
    y = "Mean richness by property (SE)", 
    fill = "Landowner",
    title = "Grassland species richness",
    subtitle = "Sampled at 3 points per property in first year surveyed")

# Possibly because they were on larger properties with more diverse habitats
# (which is an effect that would persist even when standardized for effort)
points %>% 
  group_by(owner) %>% 
  summarize(
    min = min(area, na.rm = T), 
    mean = mean(area, na.rm = T), 
    max = max(area, na.rm = T))

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
    fill = "Landowner",
    title = "Grassland species richness",
    subtitle = "Sampled across all surveys")

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
    fill = "Landowner",
    title = "Grassland species richness",
    subtitle = "Sampled per point per year")


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

# Get total abundance (will be converted to relative automatically)
method_a <-
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

# Sample cutdown method: point-year, 3 visits/year
method_b <-
  left_join(birds, visits) %>% 
  filter(visit <= 3) %>% 
  mutate(ptyr = str_c(point_id, year)) %>% 
  count(ptyr, species) %>% 
  # Get zeroes
  right_join(spxptyr) %>% 
  replace_na(list(n = 0)) %>% 
  pivot_wider(names_from = "species", values_from = "n")

#method_c <-


# Calculate metrics and visualize -----------------------------------------

abund <- method_a

method <-
  case_when(
    identical(abund, method_a) ~ "Total abundance across all surveys",
    identical(abund, method_b) ~ "Abundance separated per point per year, max. 3 visits",
    #identical(abund, method_c) ~ "Undefined",
    TRUE ~ "Undefined")

landcover_wide <-
  pivot_wider(
    landcover,
    names_from = radius, 
    values_from = c(cropland:grassland, protected))

# Method A
data <-
  full_join(
    specnumber(
      column_to_rownames(abund, var = "point_id"))%>% 
      enframe(name = "point_id", value = "richness"),
    diversity(
      column_to_rownames(abund, var = "point_id")) %>% 
      enframe(name = "point_id", value = "shannon")) %>% 
  mutate(hill = exp(shannon)) %>% 
  left_join(landcover_wide)

# Method B
data <- 
  full_join(
    specnumber(
      column_to_rownames(abund, var = "ptyr"))%>% 
      enframe(name = "ptyr", value = "richness"),
    diversity(
      column_to_rownames(abund, var = "ptyr")) %>% 
      enframe(name = "ptyr", value = "shannon")) %>% 
  mutate(hill = exp(shannon)) %>% 
  left_join(
    visits %>% 
      mutate(ptyr = str_c(point_id, year)) %>% 
      select(ptyr, point_id, year)) %>% 
  left_join(landcover_wide) %>% 
  left_join(first_year) %>% 
  filter(first == year)
  
# Histograms of Shannon
data %>% 
  distinct(point_id, hill, owner) %>% 
  filter(!is.na(owner)) %>% 
  ggplot(aes(x = hill, color = owner)) + 
  geom_density(size = 1) + 
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous(breaks = 0:max(data$hill)) +
  theme(panel.grid.minor = element_blank()) +
  labs(
    x = "Hill number (effective # of species)",
    title = paste("Method:", method))

data %>% 
ggplot(aes(x = owner, y = hill, fill = owner)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Paired") +
  labs(title = paste("Method:", method))

n_distinct(points$point_id)
n_distinct(points$property_id)

# Explore landscape data --------------------------------------------------

prot_own <-
  lm(protected_8000 ~ owner, data = landcover_wide)

broom::tidy(anova(prot_own))

TukeyHSD(aov(prot_own)) %>% broom::tidy()

# Points on public lands had more developed context.
# For example, within 2 km points on public land were significantly
# more developed 

# Did all-richness differ
rich <-
  birds %>% 
    left_join(visits) %>% 
    # Standard effort: 3 visits
    filter(
      visit <= 3, 
      species %in% c(sp_ob)) %>% 
    group_by(point_id, year) %>%  
    summarize(sp = length(unique(species))) %>% 
    left_join(points)

TukeyHSD(aov(lm(sp ~ owner, data = rich)))
anova(lm(sp ~ owner, data = rich))

AIC(
  lm(hill ~ 1, data = data),
  lm(hill ~ owner * developed_250, data = data),
  lm(hill ~ owner * developed_500, data = data),
  lm(hill ~ owner * developed_750, data = data),
  lm(hill ~ owner * developed_1000, data = data),
  lm(hill ~ owner * developed_1250, data = data),
  lm(hill ~ owner * developed_1500, data = data),
  lm(hill ~ owner * developed_1750, data = data),
  lm(hill ~ owner * developed_2000, data = data),
  lm(hill ~ owner * developed_2250, data = data),
  lm(hill ~ owner * developed_2500, data = data),
  lm(hill ~ owner * developed_2750, data = data),
  lm(hill ~ owner * developed_3000, data = data),
  lm(hill ~ owner * developed_3250, data = data),
  lm(hill ~ owner * developed_3500, data = data),
  lm(hill ~ owner * developed_3750, data = data),
  lm(hill ~ owner * developed_4000, data = data),
  lm(hill ~ owner * developed_4250, data = data),
  lm(hill ~ owner * developed_4500, data = data),
  lm(hill ~ owner * developed_4750, data = data),
  lm(hill ~ owner * developed_5000, data = data),
  lm(hill ~ owner * developed_5250, data = data),
  lm(hill ~ owner * developed_5500, data = data),
  lm(hill ~ owner * developed_5750, data = data),
  lm(hill ~ owner * developed_6000, data = data),
  lm(hill ~ owner * developed_6250, data = data),
  lm(hill ~ owner * developed_6500, data = data),
  lm(hill ~ owner * developed_6750, data = data),
  lm(hill ~ owner * developed_7000, data = data),
  lm(hill ~ owner * developed_7250, data = data),
  lm(hill ~ owner * developed_7500, data = data),
  lm(hill ~ owner * developed_7750, data = data),
  lm(hill ~ owner * developed_8000, data = data)
) %>% 
  rownames_to_column("model") %>% 
  mutate(
    delta = round(AIC - min(AIC), 2),
    radius = parse_number(model)) %>% 
  arrange(AIC) %>% 
  filter(radius!=1) %>% 
  ggplot(aes(x = radius, y = -AIC)) + geom_line()


focal_abund <-
  abund %>% 
  filter(species %in% sp_grs$species)

library(multcompView)

sp_anovas <-
  focal_abund %>% 
    split(focal_abund$species) %>% 
    purrr::map_dfr(
      ~ lm(n ~ owner, data = .x) %>% 
        anova() %>% 
        broom::tidy(),
      .id = "species") %>% 
    filter(term == "owner") %>% 
    mutate(signif = case_when(
      p.value < 0.001 ~ "P<0.001",
      p.value < 0.05 ~ "P<0.05",
      TRUE ~ "n.s.")) 


