# This script should be run first.
# It imports VWL bird survey data and corrects errors.
# Products: "data/processed/vwl_birds.rds"

# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(suncalc)

f <- "data/raw/VWL_BreedingBirdSurvey_Database_2011-2022_Massa.xlsx"

# Fix up bird data --------------------------------------------------------

# Import their taxonomy. Need to square with ebird
vwl_tax <- 
    readxl::read_xlsx(f, sheet = 4)

ebird_tax <-
  read_rds("data/processed/taxonomy.rds")

# Import data

data_raw <- 
  readxl::read_xlsx(f, sheet = 2) 

# Convert - to NA
data_raw[data_raw == "-"] <- NA
# Change incidental to NA (no) vs Y (yes)
data_raw$Incidental[data_raw$Incidental == "N"] <- NA
data_raw$Incidental[data_raw$Incidental == "flyover"] <- "Y"

# Inspect all unique values
data_unique <-
1:length(data_raw) %>% 
  map(~pull(data_raw, .) %>% unique())
names(data_unique) <- names(data_raw)

# Inspect species
data_raw %>% 
  group_by(Species) %>% 
  summarize(n = n()) %>% 
  left_join(ebird_tax, by = c("Species" = "species")) %>% 
  View()

# Cleaning
data <-
  data_raw %>%
  mutate(
    # Rename species
    species = case_when(
      Species == "BDOW" ~ "BADO",
      Species == "BNOW" ~ "BANO",
      Species == "EAPW" ~ "EAWP", 
      Species == "RWBB" ~ "RWBL", 
      Species == "YSFL" ~ "NOFL", # Subspecies
      Species == "SCJU" ~ "DEJU", # Subspecies
      Species == "UNID" ~ "UNBI",
      Species == "UN-ID DUCK" ~ "UNDU",
      Species == "UN-ID FLYCATCHER" ~ "UNFL",
      Species == "UN-ID FLYCATCHER\r\n" ~ "UNFL",
      Species == "UN-ID RAPTOR" ~ "UNHA",
      Species == "UN-ID SPARROW" ~ "UNSP",
      Species == "UN-ID SWALLOW" ~ "UNSW",
      Species == "UN-ID VIREO" ~ "UNBI", # No ebird taxonomy for this
      Species == "UN-ID WARBLER" ~ "UNWA",
      Species == "UN-ID WOODPECKER" ~ "UNWO",
      Species == "UN-ID Woodpecker" ~ "UNWO",
      Species == "UN-ID" ~ "UNBI",
      TRUE ~ Species),
    # Fix date
    date = str_c(Year, Month, Day, sep = "-") %>% ymd(),
    doy = yday(date),
    year = year(date),
    # Fix time
    start_time =
      if_else(
        str_detect(Time_24h, ":"),
        str_c(Time_24h, ":00"),
        hms::as_hms(
          round(as.numeric(Time_24h)*24*60*60)) %>% 
        as.character())) %>% 
  select(
    property_id = `Property ID`,
    field_id = `Site ID`,
    pole = Pole,
    date,
    doy,
    year,
    visit_raw = Visit,
    start_time,
    grass_height = Grass_Height,
    sky = Sky_Condition,
    wind = Wind_Condition,
    temperature = Temp_C,
    n_observers = Num_Surveyors,
    observer = `Surveyor Category`,
    species,
    distance_1 = Distance1,
    detection_1 = Detection1,
    distance_2 = Distance2,
    detection_2 = Detection2,
    incidental = Incidental,
    comments = Comments,
    long = Longitude,
    lat = Latitude
    ) %>% 
  mutate(
    point_id = str_c(property_id, field_id, pole, sep = "-"),
    dttm = as_datetime(str_c(date, start_time), tz = "America/New_York"))

# Visits to the properties
visits <-
  data %>% 
  # Remove duplicate date (only take first survey that day)
  distinct(point_id, year, dttm, visit_raw, grass_height, sky, wind, temperature, observer, n_observers) %>% 
  arrange(point_id, dttm, year) %>% 
  group_by(point_id, year) %>% 
  mutate(
    visit_id = 
      str_c(as.numeric(dttm), 
            n_observers, 
            str_sub(observer, start = 1, end = 1), 
            point_id), 
    interval = difftime(dttm, lag(dttm), units = "days") %>% as.numeric()) %>% 
  # Only visits at least 12h after the previous (different days). This excludes second same-day
  filter(interval > 0.5 | is.na(interval)) %>% 
  select(-interval) %>% 
  # Reassign visit number
  group_by(point_id, year) %>% 
  arrange(point_id, dttm) %>% 
  mutate(visit = row_number()) %>% 
  ungroup() %>% 
  select(visit_id, point_id, visit, dttm, year, sky, wind, temperature, observer, n_observers)

# Points on the properties
points <-
  data %>% 
  mutate(field_id = str_c(property_id, field_id, sep = "-")) %>% 
  select(point_id, field_id, property_id, long, lat) %>% 
  distinct()

# Birds on the visits
birds <-
  left_join(
    data,
    visits, 
    by = c("point_id", "dttm")) %>% 
  select(
    visit_id, species, distance_1, detection_1, distance_2, detection_2, 
    incidental, comments) %>% 
  # Eliminate birds on same-day removed visit
  filter(!is.na(visit_id))

# Add sunrise time
visits <-
  visits %>% 
    select(visit_id, point_id, dttm) %>%
    mutate(date = date(dttm)) %>% 
    # Add coordinates of point
    left_join(
      select(points, point_id, long, lat), 
      by = "point_id") %>% 
    # Add sunrise time
    left_join(
      getSunlightTimes(
        data = 
          visits %>% 
          select(visit_id, point_id, dttm) %>% 
          mutate(date = date(dttm)) %>% 
          left_join(
            points %>% select(point_id, long, lat), 
            by = "point_id") %>% 
          select(date, lat = lat, lon = long),
        keep = 'sunrise',
        tz = 'America/New_York') %>% 
        as_tibble(),
      by = c('date', 'lat', "long" = "lon")) %>% 
    # Calculate time since sunrise
    mutate(
      diff = difftime(dttm, sunrise, units = 'mins'),
      # Convert to numeric, "minutes after sunrise"
      start_sun = as.numeric(diff, units = 'mins')) %>% 
    # Select only calculation, then append to visits
    select(visit_id, start_sun) %>% 
    left_join(visits, by = 'visit_id') %>% 
  distinct() %>% 
  mutate(
    date = date(dttm), 
    start_time = str_extract(as.character(dttm), 
                             pattern = "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"))
  
# birds, points, visits, properties
data <- c('birds', 'points', 'visits')

merged <-
  birds %>% 
  left_join(visits, by = "visit_id") %>% 
  left_join(points, by = "point_id")

write_rds(
  mget(data),
  'data/processed/vwl_birds.rds')

rm(data, visits, site_info, properties, points, merged, data_raw, birds)
