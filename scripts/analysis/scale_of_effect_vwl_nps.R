# Spatial Ecology final project
# Megan Massa
# Fall 2022

# Setup -------------------------------------------------------------------

# Load required packages
library(sf)
library(raster)
library(tidyverse)
library(tigris)
library(FedData)
library(spThin)

# Set major parameters
sp <- c("EAME", "GRSP")
radii <- c(seq(from = 200, to = 6000, by = 200))
y <- 2016

# Set options for graphing and file management
theme_set(
    theme_bw() +
    theme(
      strip.background = element_blank(), 
      panel.grid = element_blank()))
options(tigris_use_cache = TRUE)

# Import bird data --------------------------------------------------------

# Condense NPS data
nps <-
  read_rds("data/processed/birds.rds")$counts %>% 
  left_join(
    read_rds("data/processed/birds.rds")$visits, by = "visit_id") %>% 
  left_join(
    read_rds("data/processed/birds.rds")$points, by ="grts") %>% 
  select(visit_id, date, species, flyover, 
         point_id = point_name, visit, year, 
         property_id = park, long, lat)

# Condense VWL data
vwl <-
  read_rds("data/processed/vwl_birds.rds")$birds %>% 
    left_join(
      read_rds("data/processed/vwl_birds.rds")$visits, by = "visit_id") %>% 
    left_join(
      read_rds("data/processed/vwl_birds.rds")$points, by = "point_id") %>% 
    select(visit_id, date, species, flyover = incidental, 
           point_id, visit, year, 
           property_id, long, lat) %>% 
    mutate(
      flyover = if_else(is.na(flyover), 0, 1),
      point_id = str_c("VWL_", point_id),
      property_id = str_c("VWL_", property_id)) %>% 
  filter(!is.na(point_id), !is.na(lat), !is.na(long))

## Combine datasets ----

# Which year had the most VWL points?
vwl %>% 
  group_by(year) %>% 
  summarize(points = n_distinct(point_id),
            properties = n_distinct(property_id),
            mean_pts_per_prop = points/properties) %>% 
  filter(year >=2016) %>% 
  arrange(-points)

# What did the spatial distribution of points look like?
par(mfrow = c(3, 2))

for(i in 2016:2021) {
  yearpts <-
    vwl %>%
      filter(year == i) %>%
      distinct(property_id, .keep_all =  T)
    plot(yearpts$long, yearpts$lat, axes = T, main = paste("VWL", i))
}

par(mfrow = c(1, 1))

# 2016 has the most VWL data, so I will use that (see header; y <- 2016)

# Clean up
rm(yearpts, i)

# Combine datasets and filter
birds <-
  bind_rows(NPS = nps, VWL = vwl, .id = "source")

birds <-
  birds %>% 
    filter(
      year == y,
      # Only points visited at least 3x
      point_id %in% unique(filter(birds, visit == 3)$point_id),
      # Only visits 1-3
      visit <= 3)

# Calculate statistics for each point
points <-
  birds %>%
  # Get full species list for each survey with an NA if absent
  expand(species, visit_id) %>% 
  # Attach abundance of each species at each visit, if present
  left_join(
    filter(birds, flyover == 0) %>% 
      group_by(visit_id, species) %>% 
      summarize(n = n())) %>% 
  # Turn NAs to abundance of 0
  replace_na(list(n = 0)) %>% 
  right_join(birds %>% select(visit_id, point_id) %>% distinct()) %>% 
  # Keep only focal species
  filter(species %in% sp) %>% 
  group_by(species, point_id) %>% 
  # Calculate abundance and presence
  summarize(
    mean_abund = mean(n),
    max_abund = max(n)) %>% 
  mutate(
    presence = if_else(mean_abund > 0, 1, 0)) %>% 
  # Widen data
  pivot_wider(
    id_cols = point_id, 
    names_from = species, 
    values_from = presence) %>% 
  # Add property info back in
  left_join(
    birds %>% 
      select(point_id, property_id, source, long, lat) %>% 
      distinct()) %>% 
  # simplify IDs
  mutate(point_id = row_number()) %>% 
  # Convert to a spatial object
  st_as_sf(coords = c("long", "lat"), crs = "EPSG:4326", remove = F)

summary(points)
points

# Make plots --------------------------------------------------------------

# Import state lines
states <- 
  filter(
    states(year = y), 
    STUSPS %in% c("VA", "WV", "MD", "DC")) %>% 
  st_transform(crs = st_crs(points))

# Get occupancy combos
points <-
  points %>% 
    mutate(
      presence = 
        case_when(
          EAME == 0 & GRSP == 0 ~ "Both absent",
          EAME == 0 & GRSP == 1 ~ "GRSP only",
          EAME == 1 & GRSP == 0 ~ "EAME only",
          TRUE ~ "Both present") %>% 
        as.factor())

## Map whole region ----

# Summarize occupancy by property
properties <-
  points %>% 
    group_by(property_id) %>% 
    summarize(
      EAME = sum(EAME), 
      GRSP = sum(GRSP),
      points = n()) %>% 
  mutate(
    presence =
      case_when(
        EAME == 0 & GRSP == 0 ~ "Both absent",
        EAME == 0 & GRSP > 0 ~ "GRSP only",
        EAME > 0 & GRSP == 0 ~ "EAME only",
        TRUE ~ "Both present") %>% 
      as.factor()) %>% 
  st_centroid()

# Plot only properties
ggplot() + 
  geom_sf(
    data = states, 
    color = "gray80", 
    fill = "white") +
  geom_sf(
    data = properties, 
    aes(color = presence, shape = presence),
    size = 3) + 
  scale_color_manual(
    values = c("red", "black", "gray40", "gray40"), drop = F) +
  scale_shape_manual(
    values = c(1, 20, 3, 4), drop = F) +
  coord_sf(
    xlim = c(min(points$long), max(points$long)), 
    ylim = c(min(points$lat), max(points$lat))) +
  labs(
    title = "Survey sites",
    color = "", shape = "") +
  guides(
    colour = guide_legend(override.aes = list(size = 3)))

# ggsave("output/spatecolplots/property_occupancy.png", 
#        width = 6, height = 3.5, units = "in", scale = 1.5)

## Map major parks ----

# Initialize list of plots
plots <- list()

# Loop for a map of every major park
for(p in c("ANTI", "MANA", "HAFE", "MONO")) {
  plots[[p]] <-
    ggplot() + 
      geom_sf(
        data = states, 
        color = "gray", fill = "white") +
      geom_sf(
        data = st_read("data/processed/focal_parks.shp") %>% st_transform(crs = crs(points)),
        color = "gray80", fill = "gray99") +
      geom_sf(
        data = points, 
        aes(color = source, shape = presence),
        size = 3) + 
      scale_shape_manual(
        values = c(1, 20, 3, 4), drop = F) +
      coord_sf(
        xlim = c(min(filter(points, property_id == p)$long), 
                 max(filter(points, property_id == p)$long)), 
        ylim = c(min(filter(points, property_id == p)$lat), 
                 max(filter(points, property_id == p)$lat))) +
      labs(
        title = 
          if(p == "ANTI") {"Antietam"} else 
            if(p == "MANA"){"Manassas"} else 
              if(p == "HAFE"){"Harpers Ferry"} else "Monocacy",
        color = "", shape = "") +
      guides(
        colour = guide_legend(override.aes = list(size = 3))) +
      theme_void() +
      theme(
        panel.border = element_rect(fill = NA),
        plot.title = element_text(
          hjust = 0.5,
          margin = margin(5, 5, 5, 5, "pt")),
        #aspect.ratio = 1.3
        )
}

rm(p)

# Arrange plots together
# This kind of sucks
ggpubr::ggarrange(
  plotlist = plots,
  nrow = 1,
  ncol = 4,
  legend = "bottom",
  common.legend = T)

# ggsave("output/spatecolplots/park_occupancy.png", 
#        width = 7, height = 7/3, units = "in", scale = 1.5)

## Plot prop. occupancy ----
points %>% 
  pivot_longer(
    cols = c("EAME", "GRSP"),
    names_to = "species",
    values_to = "occupancy") %>% 
  group_by(species, source) %>% 
  summarize(
    prop_occupied = mean(occupancy)) %>% 
  st_drop_geometry() %>% 
  ungroup() %>% 
  ggplot(aes(x = species, y = prop_occupied, fill = source))+
    geom_col(position = "dodge") +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "", y = "Prop. points occupied") 

# ggsave("output/spatecolplots/point_occupancy.png", 
#        width = 3, height = 3, units = "in", scale = 1.5)

# Or just present as a table

# write_rds(
#   points %>% 
#     group_by(presence) %>% 
#     summarize(n = n()) %>% 
#     mutate(prop = n/sum(n)) %>% 
#     st_drop_geometry(),
#   "output/spatecolplots/presence_table.rds")

# EAME vs GRSP
cross_table <-
  table(
    factor(points$EAME, levels = c(0, 1), 
           labels = c("EAME absent", "EAME present")),
    factor(points$GRSP, levels = c(0, 1), 
           labels = c("GRSP absent", "GRSP present"))) %>%
    prop.table()

# Run some tests
mosaicplot(cross_table, main = "")
# Yes, their occupancy is correlated
cor.test(points$EAME, points$GRSP, method = "pearson")

# Import NLCD -------------------------------------------------------------

# Get boundary for NLCD import
nlcd_bbox <- 
  st_bbox(points) %>% 
  st_as_sfc() %>% 
  st_buffer(dist = max(radii)) %>% 
  st_bbox() %>% 
  st_as_sfc()

# Download NLCD
get_nlcd(
  template = nlcd_bbox,
  label = "studyarea",
  year = y,
  dataset = "landcover",
  extraction.dir = "data/raw/",
  raster.options = c(overwrite = TRUE))

# Import NLCD and reproject others
nlcd <- raster(paste0("data/raw/studyarea_NLCD_Land_Cover_", y, ".tif"))
points_aea <- st_transform(points, crs = st_crs(nlcd))
states_aea <- st_transform(states, crs = st_crs(nlcd))
buff_large <- st_buffer(points_aea, dist = max(radii))

# Plot NLCD, points, largest buffer
plot(nlcd, axes = F)
  plot(st_crop(states_aea, st_bbox(nlcd))[1], col = NA, border = 1, add = T)
  plot(st_union(buff_large), col = NA, border = "white", lwd = 2, add = T)
  plot(points_aea[1], col = "black", pch = 19, cex = 0.4, add = T)

# Extract NLCD values -----------------------------------------------------

## Reclassify raster ----

# Developed reclass matrix
rcl_dvp <-
  tibble(
    nlcd_value = unique(nlcd)) %>% 
    mutate(developed = if_else(nlcd_value %in% c(22, 23, 24), 1, 0)) %>% 
    as.matrix()
  
# Grassland reclass matrix
rcl_grs <-
  tibble(
    nlcd_value = unique(nlcd)) %>% 
    mutate(grassland = if_else(nlcd_value %in% c(52, 71, 81), 1, 0)) %>% 
    as.matrix()

# Forest reclass matrix
rcl_for <-
  tibble(
    nlcd_value = unique(nlcd)) %>% 
    mutate(forest = if_else(nlcd_value %in% c(41:43, 90), 1, 0)) %>% 
    as.matrix()

# Cropland reclass matrix
rcl_crp <-
  tibble(
    nlcd_value = unique(nlcd)) %>% 
    mutate(cropland = if_else(nlcd_value == 82, 1, 0)) %>% 
    as.matrix()

# Reclassify and stack results
# Runtime: 10 seconds
reclass <-
  raster::stack(
    reclassify(nlcd, rcl = rcl_crp),
    reclassify(nlcd, rcl = rcl_dvp),
    reclassify(nlcd, rcl = rcl_for),
    reclassify(nlcd, rcl = rcl_grs)
    )

names(reclass) <- 
  c("cropland", "developed", "forest", "grassland")


# Thin data randomly ------------------------------------------------------

runs <- list()

# Number of times to run the thinning
n_runs <- 50

for(i in 1:n_runs) {
  # Thin to max 3 points per property
  runs[[i]] <-
    bind_rows(
      # If 3+ points, sample 3
      points %>% 
        filter(property_id %in% filter(properties, points >= 3)$property_id) %>% 
        group_by(property_id) %>% 
        slice_sample(n = 3),
      # If less than 3 points, take all
      points %>% 
        filter(property_id %in% filter(properties, points < 3)$property_id)) %>% 
    st_drop_geometry() %>% 
    arrange(point_id) %>% 
    select(point_id)
}

bind_rows(runs, .id = "run")


# Landcover by run --------------------------------------------------------

# landcover <-
#   read_rds(landcover, "data/processed/scale_effect_landcover_extracted.rds")

landcover <-
  map_dfr(
    radii,
    ~raster::extract(
      reclass, 
      points_aea,
      df = TRUE,
      buffer = .x, fun = mean) %>% 
    mutate(radius = .x) %>% 
    as_tibble()) %>% 
  mutate(
    EAME = rep(points$EAME, times = length(radii)),
    GRSP = rep(points$GRSP, times = length(radii)));beepr::beep()

write_rds(landcover, "data/processed/scale_effect_landcover_extracted.rds")

# Perform multiple runs of sampling the maximum possible number of 
# spatially independent points at each buffer radius.
landcover_runs <- 
  map(
    1:n_runs,
    ~inner_join(
      landcover, 
      runs[[.x]], 
      by = c("ID" = "point_id"))) %>% 
  bind_rows(.id = "run") %>% 
  pivot_longer(
    cols = "cropland":"grassland",
    names_to = "landcover", values_to = "proportion")

# Calculate statistics about land cover at each radius

landcover_summary <-
  landcover_runs %>% 
    group_by(radius, landcover) %>% 
    summarise(
      mean = mean(proportion, na.rm = T),
      sd = sd(proportion, na.rm = T),
      # Set sample size to be # of points per run.
      n = n()/n_runs,
      se = sd/sqrt(n)) %>% 
  ungroup()

# write_rds(
#   list(
#     landcover_summary = landcover_summary,
#     landcover_runs = landcover_runs,
#     landcover = landcover),
#   "output/spatecolplots/landcover_outputs.rds"
# )

## Plot landcover ----
# Plot change in land cover over buffer distance
ggplot(data = landcover_summary, aes(x = radius)) + 
  geom_line(
    aes(y = mean, color = landcover), 
    size = 1) +
  geom_ribbon(
    aes(ymin = mean - se, 
        ymax = mean + se, 
        color = landcover, 
        fill = landcover),
        alpha = 0.2) +
  scale_x_continuous(breaks = seq(from = 0, to = 8000, by = 1000)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Buffer radius (m)", y = "Proportion land cover (± SE)", 
       color = "Land cover", fill = "Land cover")

# ggsave("output/spatecolplots/landcover_scale.png", 
#        width = 6, height = 3.75, units = "in", scale = 1.5)

# Run cor analysis --------------------------------------------------------

# Set parameters
lc <- names(reclass)

# Unload stuff to save memory
rm(birds, buff_8km, nlcd, nlcd_bbox, nps, rcl_crp, rcl_dvp, rcl_for, rcl_grs, reclass, plots, point_demo_a, points_aea, properties, states, states_aea, vwl, crop_points, rcl_hay, rcl_noshb, rcl_wsg, visits)
gc()

# Loop over species, landcover types, and, just to mix it up, 
# a purrr::map instead of a final for loop for radii
# Results are tidied and saved into a tibble
# Runtime: 34 minutes

cors <- tibble()

for(i in 1:n_runs) {
for(s in sp) {
for(c in lc) {
  cors <-
    bind_rows(
      cors, 
      map_dfr(
        radii,
        ~cor.test(
          filter(landcover_runs, 
                 radius == .x, 
                 landcover == c,
                 run == i) %>% 
            pull(proportion),
          filter(landcover_runs, 
                 radius == .x, 
                 landcover == c,
                 run == i) %>% 
            pull(s)) %>% 
          broom::tidy() %>% 
          mutate(
            radius = .x,
            species = s,
            landcover = c,
            run = i
            )))
}
}
};beepr::beep()

write_rds(cors, "output/spatecolplots/cors.rds")

# Take the mean result of a cor test across runs
# Again this does NOT feel legitimate
cor_summary <-
  cors %>% 
  group_by(radius, species, landcover) %>% 
  summarize(
    n = n(), 
    est_mean = mean(estimate), 
    est_abs_mean = mean(abs(estimate)),
    stat_mean = mean(statistic), 
    p_min = min(p.value), 
    param_mean = mean(parameter), 
    conf.low_mean = mean(conf.low), 
    conf.high_mean = mean(conf.high),
    est_sd = sd(estimate),
    est_se = est_sd/sqrt(n)) %>% 
  mutate(signif = if_else(p_min < 0.05, "p < 0.05", "n.s.")) %>% 
  ungroup()

# write_rds(cor_summary, "output/spatecolplots/cor_summary.rds")

## Plot cor strength ----
cor_summary %>% 
  ggplot(aes(x = radius)) +
  geom_line(aes(y = abs(est_mean), color = landcover), size = 1) +
  geom_ribbon(
    aes(ymin = abs(est_mean - est_se),
        ymax = abs(est_mean + est_se),
        color = landcover,
        fill = landcover),
        alpha = 0.25, linetype = "blank") +
  scale_fill_manual(
    values = c("chocolate", "black", "forestgreen", "gold")) +
  scale_color_manual(
    values = c("chocolate", "black", "forestgreen", "gold")) +
  # geom_point(
  #   aes(y = abs(est_mean), fill = signif),
  #   shape = 21, size = 1) +
  # scale_fill_manual(values = c("white", "black")) +
  scale_x_continuous(breaks = seq(from = 0, to = 8000, by = 1000)) +
  facet_grid(
    rows = vars(landcover), 
    cols = vars(species), scales = "free_y") +
  #facet_wrap(~species, scales = "free_y") +
  labs(x = "Buffer radius (m)", 
       y = "Impact on presence\n|Pearson's r|",
       color = "Land cover",
       fill = "Land cover")

# ggsave("output/spatecolplots/cor_result_color.png", 
#        width = 7, height = 5, units = "in", scale = 1.5)


ggplot() +
  geom_line(
    data = cors,
    aes(x = radius, 
        y = estimate, 
        color = as.factor(run)), 
    size = 0.25) +
  scale_color_manual(values = rep("gray80", n_runs)) +
  geom_line(
    data = cor_summary,
    aes(
      x = radius,
      y = est_mean), 
    size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(from = 0, to = 8000, by = 1000)) +
  facet_grid(
    rows = vars(landcover), 
    cols = vars(species), scales = "free_y") +
  labs(x = "Buffer radius (m)", 
       y = "Impact on presence\nPearson's r"
       ) +
  theme(legend.position = "none")

# ggsave("output/spatecolplots/cor_result_bw.png", 
#        width = 7, height = 5, units = "in", scale = 1.5)


# Run logit analysis ------------------------------------------------------

# Run time: 35 minutes
logliks <- tibble()

for(i in 1:n_runs) {
for(s in sp) {
for(c in lc) {
  logliks <-
    bind_rows(
      logliks, 
      map_dfr(
        radii,
        ~logLik(
          glm(presence ~ proportion, 
              family = "binomial", 
              data = 
                landcover_runs %>% 
                filter(radius == .x, landcover == c, run == i) %>% 
                select(presence = s, proportion))) %>% 
          as_tibble() %>% 
          mutate(species = s, landcover = c, radius = .x, run = i)
      ))
}
}
}

write_rds(logliks, "output/spatecolplots/logliks.rds")

logliks_summary <-
  logliks %>% 
  group_by(radius, species, landcover) %>% 
  summarize(
    n = n(), 
    loglik_mean = mean(`c(x)`), 
    loglik_max = max(`c(x)`),
    loglik_min = min(`c(x)`),
    loglik_sd = sd(`c(x)`),
    loglik_se = loglik_sd/sqrt(n)) %>% 
  ungroup()

## Plot log-likelihood ----
logliks_summary %>% 
  ggplot(aes(x = radius)) +
  geom_line(aes(y = loglik_mean, color = landcover), size = 1) +
  geom_ribbon(
    aes(ymax = loglik_max, ymin = loglik_min, color = landcover, fill = landcover), 
    alpha = 0.4, linetype = 0) +
  scale_color_manual(
    values = c("chocolate", "black", "forestgreen", "gold")) +
  scale_fill_manual(
    values = c("chocolate", "black", "forestgreen", "gold")) +
  scale_x_continuous(breaks = seq(from = 0, to = 8000, by = 1000)) +
  facet_grid(
    rows = vars(landcover), 
    cols = vars(species), 
    scales = "free_y") +
  labs(x = "Buffer radius (m)", 
       y = "Log-likelihood",
       color = "Land cover", fill = "Land cover")

# ggsave("output/spatecolplots/loglik_results.png", 
#        width = 5, height = 3.75, units = "in", scale = 1.5)

# Scale of effect ---------------------------------------------------------

# SCALE OF EFFECT (cor)
# Ideally this would involve comparing to see which distances 
# significantly differ from each other, but oh well!
cor_summary %>% 
  group_by(species, landcover) %>% 
  #filter(signif == "p < 0.05") %>% 
  arrange(-abs(est_mean)) %>% 
  select(species, landcover, radius, est_mean) %>% 
  slice_head(n = 1)

# SCALE OF EFFECT (log-lik)
# Ugh. This is not working like it should. Ignore this one
logliks_summary %>% 
  group_by(species, landcover) %>% 
  arrange(-loglik_mean) %>% 
  select(species, landcover, radius, loglik_mean) %>% 
  slice_head(n = 1)
