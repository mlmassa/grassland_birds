# This script should be run first.
# It imports bird survey data and corrects errors.
# Products: 
# "data/processed/birds.rds"
# "data/processed/focal_parks.shp"

# setup -------------------------------------------------------------------

library(sf)
library(tidyverse)
library(suncalc)

taxonomy <-
  read_rds("data/processed/taxonomy.rds")

# Setup -------------------------------------------------------------------

# Load NPS park boundaries
st_write(
  st_read("data/raw/nps_boundary.shp") |> 
    filter(UNIT_CODE %in% c("MANA", "HAFE", "ANTI", "MONO")),
  "data/processed/focal_parks.shp",
  append = T)


# Fix up bird data --------------------------------------------------------

# Import
birds <-
  # Read in raw data:
  read_csv(
    "data/raw/NCRN_Grassland_Bird_Data_Formatted_2014_2021_FieldTypes.csv") |>
  # Remove non-bird and irrelevant variables:
  select(
    -c(Month, Day, Year, EndTime, Bird_Count, `Common Name`, Field_Type)) |> 
  # Rename variables to the format I prefer:
  rename(
    park = Admin_Unit_Code,
    grts = GRTS_Order,
    point_name = Point_Name,
    lat = Latitude,
    long = Longitude,
    visit = Visit,
    date = EventDate,
    species = AOU_Code,
    flyover = Flyover_Observed,
    time_interval = Interval,
    distance_band = Distance_id,
    visit_id = Event_ID,
    start_time = StartTime,
    temperature = Temperature,
    humidity = Humidity,
    sky = Sky_Condition,
    wind = Wind_Code,
    disturbance = Disturbance_Level,
    observer = Observer,
    sex = Bird_Sex_ID,
    id_method = ID_Method_Code,
    first_3_min = Initial_Three_Min_Cnt) |> 
  # Build and modify columns:
  mutate(
    # Format date:
    date = mdy(date),
    # Add year variable for ease of use:
    # I use this instead of the included "year" variable for no reason.
    year = year(date),
    # Correct code mistakes
    species = 
      str_replace_all(
        string = species,
        c("UNCH" = "UPCH",
          "ETTI" = "TUTI",
          "RODO" = "ROPI",
          "SASP" = "SAVS",
          "GRBH" = "GBHE",
          "UNAH" = "UAHA")),
    # Standardize time formats
    start_time = start_time |> str_remove(pattern = " AM"), 
    start_time =
      if_else(
        str_detect(string = start_time, pattern = ":[0-9][0-9]:00$"), 
        str_remove(string = start_time, pattern = ":00$"), 
        start_time) |> 
      str_c(":00")) |> 
    # Remove unverifiable code mistakes
    filter(!species %in% c("CHPH", "NOCR")) |>
  # Arrange by date:
  arrange(date)

# Create tidy data --------------------------------------------------------

# Create tidy table of points (only coords, park, point name)
# This looks awful but don't change it. Some latlong non-distinct duplicates
# are ruining a simlper version.
points <-
  birds |> 
  select(grts, point_name, park, long, lat) |> 
  distinct() |> 
  group_by(grts) |>
  arrange(grts) |> 
  mutate(id = row_number()) |> 
  filter(id == 1) |> 
  ungroup() |> 
  select(-id)

# Create tidy table of visits/surveys
# This contains only the visit/survey-level information (no birds)
visits <-
  birds |> 
  filter(!observer == "Tyler Chambers") |> 
  select(
    visit_id, grts,
    visit, date,
    year, start_time,
    sky, wind,
    temperature,
    humidity,
    disturbance,
    observer) |> 
  distinct() |> 
  # Fix visit number
  group_by(grts, year) |> 
  arrange(grts, date) |> 
  mutate(visit = row_number()) |> 
  ungroup()

# Create tidy table of bird counts
counts <-
  birds |> 
  filter(!observer == "Tyler Chambers") |> 
  select(
    visit_id,
    species, 
    flyover,
    distance_band,
    time_interval,
    sex,
    id_method,
    first_3_min)

# Add sunrise time --------------------------------------------------------

timefix <-
  # Subset to relevant variables
  visits |> 
    select(grts, visit_id, date, start_time) |>
    # Add coordinates of point
    left_join(points |> select(grts, lat, long), by = "grts") |> 
    # Add sunrise time
    left_join(
      getSunlightTimes(
        data = 
          visits |> select(grts, date, start_time) |> 
          left_join(
            points |> select(grts, lat, long), 
            by = "grts") |> 
          select(date, lat, lon = long),
        keep = "sunrise",
        tz = "America/New_York") |> 
        as_tibble(),
      by = c("date", "lat", "long" = "lon")) |> 
  # Calculate time since sunrise
    mutate(start_dttm = (ymd(date) + hms(start_time)))
      
tz(timefix$start_dttm) <- "America/New_York"

visits <-
  timefix |> 
    mutate(
      diff = difftime(start_dttm, sunrise, units = "mins"),
      # Convert to numeric, "minutes after sunrise"
      start_sun = as.numeric(diff, units = "mins")) |> 
    # Select only calculation, then append to visits
    select(visit_id, start_sun) |> 
    left_join(visits, by = "visit_id")

# Error detection ---------------------------------------------------------

# Import rebird taxonomy (this will take a few seconds)
# This has already been done-- see load at the top
# taxonomy <- 
#   rebird::ebirdtaxonomy() |> 
#   select(
#     species = bandingCodes,
#     common_name = comName,
#     sci_name = sciName,
#     tax_level = category,
#     order = order,
#     family = familySciName,
#     tax_order = taxonOrder) |> 
#   # Remove species with no banding code (not North American)
#   filter(
#     !is.na(species),
#     !order %in% c("Tinamiformes", "Phoenicopteriformes", "Pterocliformes",
#                   "Eurypygiformes", "Phaethontiformes", "Procellariiformes",
#                   "Trogoniformes", "Bucerotiformes", "Galbuliformes", 
#                   "Psittaciformes"))
#
# Save taxonomy
# taxonomy |> 
#   write_rds("data/processed/taxonomy.rds")

# Return rows where banding code does not match one in eBird taxonomy:
birds |> 
  filter(!species %in% taxonomy$species)

# Visually inspect this list to see if any species seem out of place:
birds |> 
  select(species) |> 
  count(species) |> 
  left_join(taxonomy, by = "species") |> 
  arrange(tax_order) |> 
  View()

# Verify that there are an equal number of unique visits as event IDs
visits |> 
  pull(visit_id) |> 
  length()
# ==
# This was fixed by auto-reassigning visit number
visits |> 
  group_by(grts, year) |> 
  arrange(grts, date) |> 
  mutate(
    # Fix visit number
    visit = row_number(),
    # Add visit identifier:
    visit_id = str_c(grts, year(date), visit, sep = "-")) |> 
  ungroup() |> 
  pull(visit_id) |> unique() |> length()


# Save data ---------------------------------------------------------------

data <- c("points", "counts", "visits")

write_rds(
  mget(data),
  "data/processed/birds.rds")

# This can be read in using read_rds then list2env(.GlobalEnv)
# I prefer saving and loading data as R objects because you can't 
# accidentally destroy or modify them with Excel.
# But you can also save it with write_csv if you prefer...
