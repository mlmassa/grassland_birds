# This script should be run first.
# It imports VWL bird survey data and corrects errors.
# Products: 'data/processed/vwl_birds.rds'

# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(suncalc)

# Fix up bird data --------------------------------------------------------

# Import their taxonomy. Need to square with ebird
vwl_tax <- 
    readxl::read_xlsx(
    #"data/raw/VWL_BreedingBirdSurvey_Database_Updated.xlsx", 
    "data/raw/VWL_data_updated_manualedit.xlsx", sheet = 3)

ebird_tax <-
  read_rds("data/processed/taxonomy.rds")

# Import data

data_raw <- 
  readxl::read_xlsx(
    #"data/raw/VWL_BreedingBirdSurvey_Database_Updated.xlsx", 
    "data/raw/VWL_data_updated_manualedit.xlsx",
    col_types = c(
      "numeric", #A Property_ID
      "text", #B Pole
      "skip", #C JulianDate
      "text", #D Day
      "text", #E Month ("6" "June")
      "text", #F Year
      "numeric", #G Visit (suspect wrong: max 5)
      "text", #H Grass_Height (mixed text/number)
      "numeric", #I Temp_C
      "numeric", #J Sky_Condition
      "numeric", #K Wind_Condition
      "text", #L Num_Surveyors
      "text", #M Surveyor_Category
      "text", #N Time_24h
      "text", #O Species
      "numeric", #P Distance1
      "numeric", #Q Detection1
      "numeric", #R Distance2
      "numeric", #S Detection2
      "text", #T Incidental
      "text", #U Comments
      "text" # V Dupe notes (manual)
      ),
    sheet = 2) 

# Convert - to NA
data_raw[data_raw == "-"] <- NA
# Change incidental to NA (no) vs Y (yes)
data_raw$Incidental[data_raw$Incidental == "N"] <- NA
data_raw$Incidental[data_raw$Incidental == "flyover"] <- "Y"

# Cleaning
data <-
  data_raw %>%
  mutate(
    # Rename species
    species = case_when(
      Species == "ACTA" ~ "SCTA", # Most likely typo
      Species == "CAGO" ~ "CANG", # Commonly miscoded
      Species == "CAWR" ~ "CARW", # Commonly miscoded
      Species == "CEWA" ~ "CEDW", # Multiple together must be waxwings.
      Species == "CLIS" ~ "CLSW", # Weird miscode
      Species == "COCR" ~ "COGR", # Most likely typo. NOT Common Crane!
      Species == "CORM" ~ "CORA", # Weird miscode. Not certain.
      Species == "EAPE" ~ "EAWP", # Weird miscode. This could be EAPH
      Species == "EAPW" ~ "EAWP", # Commonly miscoded
      Species == "FSIP" ~ "FISP", # Most likely typo
      Species == "MAKE" ~ "AMKE", # Most likely typo
      Species == "ORO" ~ "OROR",  # Most likely typo
      Species == "RBWL" ~ "RWBL", # Most likely typo. Maybe RBWO but unlikely
      Species == "RWBB" ~ "RWBL", # Commonly miscoded
      Species == "RWBO" ~ "RBWO", # Most likely typo. Maybe RBWL but unlikely
      Species == "RWBK" ~ "RWBL", # Most likely typo
      Species == "TRE" ~ "TRES",  # Most likely typo
      Species == "YWAR" ~ "YEWA", # Commonly miscoded (old taxonomy)
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
      Species == "UN-ID" ~ "UNBI",
      Species == "WTKI" ~ NA_character_, # White-tailed Kite. That can't be right
      Species == "RSFL" ~ NA_character_, # Red-shafted Flicker. That can't be right
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
        hms::as_hms(round(as.numeric(Time_24h)*24*60*60)) %>% as.character())) %>% 
  select(
    property_id = Property_ID,
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
    observer = Surveyor_Category,
    species,
    distance_1 = Distance1,
    detection_1 = Detection1,
    distance_2 = Distance2,
    detection_2 = Detection2,
    incidental = Incidental,
    comments = Comments,
    dupe = Dupe
    )

site_info <-
  readxl::read_xlsx("data/raw/VWL_Property_Database.xlsx", sheet = 1) %>% 
  select(
    property_id = Property_ID,
    pole = New_Pole,
    point_long = `Pole Long`,
    point_lat = `Pole Lat`,
    property_long = `Centroid Longitude`,
    property_lat = `Centroid Latitude`,
    acreage = Acres,
    survey_years = `Bird Data By Pole`) %>% 
  left_join(
    readxl::read_xlsx("data/raw/VWL_Property_Database.xlsx", sheet = 3) %>% 
    select(
      property_id = Property_ID, 
      easement = `Conservation Easement`,
      county = Property_County))

# Properties
properties <-
  site_info %>% 
  select(
    property_id, property_long, property_lat, 
    acreage, survey_years, easement, county) %>% 
  distinct() %>% 
  mutate(years_surveyed = 1 + str_count(survey_years, ","))

# Visits to the properties
# NOTE NEED FIXED DATA... THERE ARE QUESTIONS
visits <-
  data %>% 
  filter(is.na(dupe)) %>% 
  # Remove duplicate date (only take first survey that day)
    distinct(property_id, pole, date, year, start_time) %>% 
    arrange(property_id, year, date, start_time) %>% 
    group_by(property_id, pole, year, date) %>% 
    mutate(dupe_check = row_number()) %>% 
    filter(dupe_check != 2) %>% 
    select(-dupe_check) %>% 
  # Reassign visit number
  group_by(property_id, pole, year) %>% 
  arrange(date) %>% 
  mutate(visit = row_number()) %>% 
  ungroup() %>% 
  # Ugh I'm dropping observer effect
  left_join(
    distinct(data, 
             property_id, pole, date, year, start_time, 
             grass_height, sky, wind, temperature)) %>% 
  mutate(
    # Make point ID
    point_id = str_c(property_id, pole),
    # Only take the first one's info if there's dupes
    visit_id = str_c(point_id, year, visit, sep = "_")) %>% 
  group_by(visit_id) %>% 
  slice(1) %>% 
  ungroup()

# Points on the properties
points <-
  site_info %>% 
    mutate(point_id = str_c(property_id, pole)) %>% 
    select(point_id, property_id, pole, point_long, point_lat)

# Birds on the visits
birds <-
  left_join(
    data,
    visits, 
    by = c("property_id", "year", "date", "start_time")) %>% 
  select(
    visit_id, species, distance_1, detection_1, distance_2, detection_2, 
    incidental, comments) 

# Add sunrise time
visits <-
  visits %>% 
    select(visit_id, point_id, date, start_time) %>%
    # Add coordinates of point
    left_join(
      select(points, point_id, point_long, point_lat), 
      by = "point_id") %>% 
    # Add sunrise time
    left_join(
      getSunlightTimes(
        data = 
          visits %>% 
          select(point_id, visit_id, date, start_time) %>% 
          left_join(
            points %>% select(point_id, point_long, point_lat), 
            by = "point_id") %>% 
          select(date, lat = point_lat, lon = point_long),
        keep = 'sunrise',
        tz = 'America/New_York') %>% 
        as_tibble(),
      by = c('date', "point_lat" = 'lat', 'point_long' = 'lon')) %>% 
    # Calculate time since sunrise
    mutate(
      start_dttm = (ymd(date) + hms(start_time)),
      tz(start_dttm) <- 'America/New_York',
      diff = difftime(start_dttm, sunrise, units = 'mins'),
      # Convert to numeric, "minutes after sunrise"
      start_sun = as.numeric(diff, units = 'mins')) %>% 
    # Select only calculation, then append to visits
    select(visit_id, start_sun) %>% 
    left_join(visits, by = 'visit_id') %>% 
  select(-pole, -property_id) %>% 
  distinct()
  
# birds, points, visits, properties
data <- c('birds', 'points', 'visits', 'properties')

merged <-
  birds %>% 
  left_join(visits, by = "visit_id") %>% 
  left_join(points, by = "point_id") %>% 
  left_join(properties, by = "property_id")

write_rds(
  mget(data),
  'data/processed/vwl_birds.rds')

rm(data, visits, site_info, properties, points, merged, data_raw, birds)