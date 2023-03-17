# Chapter 2.2: Analysis of protected areas and grassland diversity

# Setup -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(vegan)

# Read in data: points, visits, birds, species
read_rds("data/processed/ch2_combined_birds.rds") %>% 
  list2env(.GlobalEnv)

# Set theme for plotting
theme_set(theme_bw(base_size = 12))

# Set palette for fac/ob
pal <- c(facultative = "gray", obligate = "darkgreen")

# Explore data ------------------------------------------------------------

# How many grassland species?
nrow(sp_grs)

# How many detections?
birds %>% 
  filter(species %in% sp_grs$species, incidental == 0) %>% 
  group_by(species) %>% 
  count() %>% 
  left_join(
    sp_grs %>% 
    select(species, common_name, status)) %>% 
  arrange(n) %>% 
  ggplot(aes(x = n, y = reorder(common_name, n), fill = status)) + 
    geom_col() + 
    labs(x = "Individuals detected", y = "", fill = "") + 
    scale_fill_manual(values = pal) +
  theme(
    legend.position = c(0.8, 0.2))

# Average abundance of all obligates
birds %>% 
  filter(incidental != 0) %>% 
  left_join(sp_grs) %>% 
  left_join(visits) %>% 
  group_by(visit_id, status, point_id) %>% 
  summarize(n = n()) %>% 
  replace_na(list(status = "non-grassland")) %>% 
  left_join(points) %>% 
  group_by(owner, status) %>% 
  summarize(mean_birds_surv = mean(n), se = sd(n)/sqrt(length(n))) %>% 
  filter(!is.na(owner)) %>% 
  ggplot(
    aes(x = status, y = mean_birds_surv, fill = owner)) +
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
      fill = "Landowner")

# Maximum of 27 grassland species possible. 26 if exclude WTKI
# Per point?
birds %>% 
  filter(incidental == 0) %>% 
  left_join(visits) %>% 
  filter(species %in% c(sp_ob, sp_fac)) %>% 
  group_by(point_id) %>% 
  summarize(sp = length(unique(species))) %>% 
  left_join(points) %>% 
  group_by(owner) %>% 
  summarize(mean_sp = mean(sp), se = sd(sp)/sqrt(length(sp)))

# Per property?
birds %>% 
  filter(incidental == 0) %>% 
  left_join(visits) %>% 
  left_join(points) %>% 
  filter(species %in% c(sp_ob, sp_fac)) %>% 
  group_by(property_id) %>% 
  summarize(sp = length(unique(species))) %>% 
  #left_join(visits) %>% 
  left_join(points %>% distinct(property_id, owner)) %>% 
  group_by(owner) %>% 
  summarize(mean_sp = mean(sp), se = sd(sp)/sqrt(length(sp)))

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

# Another method: use rarefaction
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
  bind_rows(crossing(point_id = unique(visits$point_id), total_sp = 0, total_visits = 0)) %>% 
  left_join(points) %>% 
  ggplot(
    aes(
      x = total_visits, 
      y = total_sp, 
      color = owner, 
      group = point_id)) + 
    #geom_jitter(size = 0.5, height = 0.3, width = 0.3) +
    geom_line(alpha = 0.5) +
    scale_color_brewer(palette = "Paired") +
    theme(legend.position = "none") +
    labs(
      x = "Total visits to point", 
      y = "Total grassland species", 
      title = "Species accumulation")

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

