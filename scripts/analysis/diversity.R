# Calculate Shannon-Wiener Diversity


# Setup -------------------------------------------------------------------

# Load required packages
library(tidyverse)
library(sf)

# Load bird data
points <-
  bind_rows(
    VWL =
      read_rds('data/processed/vwl_birds.rds')$points %>% 
      mutate(property_id = as.character(property_id)) %>% 
      select(
        point_id,
        property_id,
        long = point_long,
        lat = point_lat),
    NPS =
      read_rds('data/processed/birds.rds')$points %>% 
      mutate(point_id = as.character(grts)) %>% 
      select(
        point_id,
        property_id = park,
        long,
        lat),
    .id = "dataset")

birds <-
  bind_rows(
    read_rds('data/processed/vwl_birds.rds')$birds %>% 
      select(visit_id, species, incidental),
    read_rds('data/processed/birds.rds')$counts %>% 
      mutate(incidental = if_else(flyover == 1, "Y", NA_character_)) %>% 
      select(visit_id, species, incidental))

visits <-
  bind_rows(
    read_rds('data/processed/vwl_birds.rds')$visits %>% 
      select(-grass_height),
    read_rds('data/processed/birds.rds')$visits %>% 
      mutate(point_id = as.character(grts)) %>% 
      select(-grts, -humidity, -disturbance, -observer))

# Load public lands information
# DOI Protected Area Database Version 3.0
# pad<-
# st_read("data/raw/PADUS3_0Combined_Region1.shp")
# Load taxonomy
tax <- 
  read_rds("data/processed/taxonomy.rds") %>% 
  filter(species %in% birds$species)

# Get grassland bird list -------------------------------------------------
bird_assignment <-
  tax %>% 
    left_join(
      readxl::read_xlsx(
        "data/raw/ACAD Global 2021.02.05.xlsx", sheet = 1) %>% 
        select(
          common_name = `Common Name`, 
          global_pop = `Global Pop Size#`,
          watchlist = CI,
          iucn = `IUCN Red List 2018`,
          agriculture = Agriculture,
          habitat_1 = `Primary Breeding Habitat`,
          habitat_2 = `Secondary Breeding Habitat`)) %>% 
    left_join(
      readxl::read_xlsx(
        "data/raw/pif_species_assessment-table-full.xlsx", sheet = 1) %>% 
        select(
          common_name = `English Name`, 
          habitat_3 = `Major Breeding Habitats`,
          habitat_4 = `Breeding Habitats`)) %>% 
    replace_na(
      list(agriculture = "-", habitat_1 = "-", habitat_2 = "-", habitat_3 = "-", habitat_4 = "-")) %>% 
    mutate(
      grassbird = if_else(
        str_detect(str_c(habitat_1, habitat_2, habitat_3, habitat_4), "[Gg]rassland|[Ss]hrub") | 
        str_detect(agriculture, "b") |
        # Amy Johnson's thesis spp
        str_detect(species, "AMKE|BLGR|BOBO|COYE|DICK|EABL|EAKI|EAME|EATO|FISP|GRSP|INBU|NOBO|OROR|PRAW|RWBL|SAVS|WEVI|WIFL|YBCH"), 
        "grassland", "non-grassland"))

bird_assignment


# Calculate density -------------------------------------------------------

birds %>% 
  left_join(visits) %>% 
  group_by(point_id, species) %>% 
  summarize(abundance = n(), .groups = "drop") %>% 
  left_join(visits %>% group_by(point_id) %>% summarize(visits = n())) %>% 
  mutate(density = abundance/visits) %>% 
  filter(species %in% bird_assignment[which(bird_assignment$grassbird == "grassland"), ]$species) %>% 
  ggplot(aes(x = density)) + geom_histogram()

# Calculate diversity -----------------------------------------------------

y <- 2018

# Get number of species S per point
diversity <-
  birds %>% 
    left_join(visits) %>% 
    filter(year == y) %>% 
    filter(!is.na(species), is.na(incidental), !is.na(point_id)) %>%
    group_by(point_id) %>% 
    summarize(S = n_distinct(species)) %>% 
  # Get number of grassland species Sg per point
  left_join(
    birds %>% 
    left_join(visits) %>% 
    filter(year == y) %>% 
    filter(species %in% bird_assignment[which(bird_assignment$grassbird == "grassland"), ]$species) %>% 
    filter(!is.na(species), is.na(incidental), !is.na(point_id)) %>%
    group_by(point_id) %>% 
    summarize(Sg = n_distinct(species))) %>% 
  # Get H' per point
  left_join(
    birds %>% 
    left_join(visits) %>% 
    filter(year == y) %>% 
    filter(!is.na(species), is.na(incidental), !is.na(point_id)) %>%
    left_join(select(visits, c("visit_id", "point_id"))) %>% 
    group_by(point_id, species) %>% 
    summarize(n = n()) %>% 
    mutate(pI = n/sum(n)) %>% 
    summarize(HI = -sum(pI * log(pI)))) %>% 
  # Get H' diversity of ONLY grassland community at each point
  left_join(
    birds %>% 
    left_join(visits) %>% 
    filter(year == y) %>% 
    filter(!is.na(species), is.na(incidental), !is.na(point_id)) %>%
    filter(species %in% bird_assignment[which(bird_assignment$grassbird == "grassland"), ]$species) %>% 
    left_join(select(visits, c("visit_id", "point_id"))) %>% 
    group_by(point_id, species) %>% 
    summarize(n = n()) %>% 
    mutate(pI = n/sum(n)) %>% 
    summarize(HIg = -sum(pI * log(pI))))  %>% 
  replace_na(list(S = 0, Sg = 0, HI = 0, HIg = 0)) %>% 
  mutate(
    prop_g_spp = Sg/S,
    E = HI/log(S),
    Eg = HIg/log(Sg))

# Plot richness vs diversity
diversity %>% 
  left_join(points) %>% 
  ggplot(aes(x = HIg, y = HI, color = dataset)) + 
    geom_jitter() + 
    theme_bw()

diversity %>% 
  left_join(points) %>% 
  ggplot(aes(x = Eg, y = HIg, color = dataset)) + 
    geom_point() + 
    theme_bw()

diversity %>% 
  left_join(points) %>% 
  ggplot(aes(x = dataset, y = HI, fill = dataset)) + 
    geom_boxplot() + 
    theme_bw()

diversity %>% 
  left_join(points) %>% 
  ggplot(aes(x = dataset, y = Sg, fill = dataset)) + 
    geom_jitter() + 
    theme_bw()

cor.test(Hg$HI, H$HI)
