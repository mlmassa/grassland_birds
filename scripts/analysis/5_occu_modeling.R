# This script runs and compares models using the occu package.

# Setup -------------------------------------------------------------------

# Load required packages
library(unmarked)
library(tidyverse)
library(AICcmodavg)
library(beepr)
library(xlsx)

theme_set(theme_bw())

source("scripts/wrangling/tidy_method_unmarked.R")

# Set focal species

# sp <- "EAME"
# sp_long <- "Eastern meadowlark"

sp <- "GRSP"
sp_long <- "Grasshopper sparrow"

# Import UMF covariates
read_rds("data/processed/umf.rds") %>% 
  list2env(.GlobalEnv)

# Create UMF object
umf <-
  unmarkedFrameOccu(
    y = 
      detections %>% 
      filter(species == sp) %>% 
      select(-grts, -year, -species) %>% 
      column_to_rownames(var = "grts_year"),
    siteCovs = siteyear_covs,
    obsCovs = obs_covs)

summary(umf)

# Clean up
gc()

# Null model --------------------------------------------------------------

# All formulas take the form ~ p (detection) ~ Psi (occupancy)
m.0 <- 
  occu(
    data = umf, 
    formula = ~ 1 ~ 1)

# Detection models --------------------------------------------------------

# Suite of detection models (30 seconds EAME; longer GRSP)
det_models <-
  list(
    null = m.0,
    all = 
      occu(data = umf, 
        formula = ~ observer + scale(doy) + disturbance + wind + scale(temperature) + scale(time) ~ 1),
  # Single models
    obs = 
      occu(data = umf, 
        formula = ~ observer ~ 1), 
    doy = 
      occu(data = umf, 
        formula = ~ scale(doy)~ 1), 
    wind = 
      occu(data = umf, 
        formula = ~ wind ~ 1),
    dist =
      occu(data = umf,
        formula = ~ disturbance ~ 1),
    temp = 
      occu(data = umf, 
        formula = ~ scale(temperature) ~ 1),
    time = 
      occu(data = umf, 
        formula = ~ scale(time) ~ 1), 
  # 2-models
    wind_obs = 
      occu(data = umf, 
        formula = ~ wind + observer ~ 1),
    wind_doy = 
      occu(data = umf, 
        formula = ~ wind +scale(doy)~ 1),
    wind_temp = 
      occu(data = umf, 
        formula = ~ wind + scale(temperature) ~ 1),
    wind_time = 
      occu(data = umf, 
        formula = ~ wind + scale(time) ~ 1),
    wind_dist =
      occu(data = umf,
        formula = ~ wind + disturbance ~ 1),
    obs_doy = 
      occu(data = umf, 
        formula = ~ observer +scale(doy)~ 1),
    obs_temp = 
      occu(data = umf, 
        formula = ~ observer + scale(temperature) ~ 1),
    obs_time = 
      occu(data = umf, 
        formula = ~ observer + scale(time) ~ 1),
    doy_temp = 
      occu(data = umf, 
        formula = ~scale(doy)+ scale(temperature) ~ 1),
    doy_time = 
      occu(data = umf, 
        formula = ~scale(doy)+ scale(time) ~ 1),
    temp_time = 
      occu(data = umf, 
        formula = ~ scale(temperature) + scale(time) ~ 1),
  # 3-models
    wind_obs_doy =
      occu(data = umf, 
        formula = ~ wind + observer +scale(doy)~ 1),
    wind_obs_temp = 
      occu(data = umf, 
        formula = ~ wind + observer + scale(temperature) ~ 1),
    wind_obs_time = 
      occu(data = umf, 
        formula = ~ wind + observer + scale(time) ~ 1),
    wind_doy_temp =
      occu(data = umf, 
        formula = ~ wind + scale(doy)+ scale(temperature) ~ 1),
    wind_doy_time =
      occu(data = umf, 
        formula = ~ wind + scale(doy)+ scale(time) ~ 1),
    wind_temp_time = 
      occu(data = umf, 
        formula = ~ wind + scale(temperature) + scale(time) ~ 1),
    obs_doy_temp = 
      occu(data = umf, 
        formula = ~ observer + scale(doy)+ scale(temperature) ~ 1),
    obs_doy_time = 
      occu(data = umf, 
        formula = ~ observer + scale(doy)+ scale(temperature) ~ 1),
    obs_temp_time = 
      occu(data = umf, 
        formula = ~ observer + scale(temperature) + scale(time) ~ 1),
    doy_temp_time = 
      occu(data = umf, 
        formula = ~scale(doy)+ scale(temperature) + scale(time) ~ 1)
    ); beep("ping")

aictab(det_models)

# The following creates a Psi formula using that species's best det model
best_det <-
  as.character(pluck(det_models, aictab(det_models)[1, 1])@formula)[2]

# WINNING DETECTION MODELS
# EAME: disturbance
# GRSP: wind (not by much, try comparing others at the end?)

# Annual trend models -----------------------------------------------------

m.psi0 <- 
  occu(data = umf, 
    formula = 
      as.formula(paste(best_det, 
      "1", 
      sep = " ~ ")))

# 10 seconds
trend_models <-
  list(
    null = m.psi0,
    year =
      occu(data = umf, 
        formula = 
         as.formula(paste(best_det, 
         "year", 
         sep = " ~ "))),
    yearpark =
      occu(data = umf, 
        formula = 
         as.formula(paste(best_det, 
         "year + park", 
         sep = " ~ "))),
    park =
      occu(data = umf, 
        formula = 
         as.formula(paste(best_det, 
         "park", 
         sep = " ~ "))),
    yearfct =
      occu(data = umf,
        formula = 
         as.formula(paste(best_det, 
         "year_fct", 
         sep = " ~ "))),
    yearxpark =
      occu(data = umf, 
        formula = 
         as.formula(paste(best_det, 
         "year * park", 
         sep = " ~ "))),
    yearfctxpark =
      occu(data = umf,
        formula = 
         as.formula(paste(best_det, 
         "year_fct * park", 
         sep = " ~ ")))
  ) ; beep("ping")

aictab(trend_models)

# The following creates a Psi formula using best trend model (with NO park)
best_trend <-
  as.character(
    pluck(
      trend_models, 
      pull(
        filter(
          as_tibble(aictab(trend_models)), 
          !str_detect(Modnames, "park"))[1, 1]))@formula)[3]


# GRSP: Across all, small (-0.0852/yr) but sig. (p = 0.02) trend. Num > fct year
# EAME: Worse than null: NO trend across years Fct > num year but only slightly.

trend_models$yearxpark

# Occupancy models --------------------------------------------------------

## Habitat submodel ----

# What is the impact of immediate habitat?

# Habitat variables:
  # field_type (crop/hay/pasture/unleased)
  # habitat (crop/CSGhay/WSGhay/CSGpasture/WSGunleased/unleased)
  # *grass_type (CSG/WSG) <- SUBSET
  # shrub
  # angle_mean
  # angle_max

# Habitat type (all)

hab_models <-
  list(
    null = m.psi0,
  # Single models
    habitat =
      occu(data = umf, 
        formula = 
         as.formula(paste(best_det, 
         "habitat", 
         sep = " ~ "))),
    shrub =
      occu(data = umf, 
        formula = 
         as.formula(paste(best_det, 
         "scale(shrub)", 
         sep = " ~ "))),
    anglemax =
      occu(data = umf, 
        formula = 
         as.formula(paste(best_det, 
         "scale(angle_max)", 
         sep = " ~ "))),
    anglemean =
      occu(data = umf, 
        formula = 
         as.formula(paste(best_det, 
         "scale(angle_mean)", 
         sep = " ~ "))),
  # 2-models
    hab_anglemean =
      occu(data = umf, 
        formula = 
         as.formula(paste(best_det, 
         "habitat + scale(angle_mean)", 
         sep = " ~ "))),
    hab_shrub =
      occu(data = umf, 
        formula = 
         as.formula(paste(best_det, 
         "habitat + scale(shrub)", 
         sep = " ~ "))),
    shrub_anglemean =
        occu(data = umf, 
          formula = 
           as.formula(paste(best_det, 
           "scale(shrub) + scale(angle_mean)", 
           sep = " ~ "))),
  # Full models
    hab_anglemax_shrub =
      occu(data = umf, 
        formula = 
         as.formula(paste(best_det, 
         "habitat + scale(angle_max) + scale(shrub) ", 
         sep = " ~ "))),
    hab_anglemean_shrub =
      occu(data = umf, 
        formula = 
         as.formula(paste(best_det, 
         "habitat + scale(angle_mean) + scale(shrub) ", 
         sep = " ~ ")))
    ); beep("ping")

aictab(hab_models)

# The following creates a Psi formula using best hab model
best_hab <-
  as.character(pluck(hab_models, aictab(hab_models)[1, 1])@formula[3])

# BEST HABITAT MODELS:
# GRSP: All three habitat + anglemean + shrub
# EAME: Only habitat + anglemean (shrub improves but not by much, delta 1.99)

## Management submodel ----

# What is the impact of burning and harvesting?

# Management variables: (* = subset)
#  ever_burned (no n=1610/ yes n=84)
#  *burn_class (0-2 n=32/ 3+ n=36)
#  *t_since_burn (linear)
#  *prop_burned (in year zero)
#  ~*harvest limit (no n=1134/ yes n=294) Barely a subset
#  *years_limited
#  leased (no n=147, yes n=1547)

# Management (ag)
mgmt_models <-
  list(
    null = m.psi0,
  # Singles
    burn =
      occu(data = umf, 
        formula = 
          as.formula(paste(best_det, 
          "ever_burned", 
          sep = " ~ "))),
    lease =
      occu(data = umf, 
        formula = 
          as.formula(paste(best_det, 
          "leased", 
          sep = " ~ "))),
  # Both
    burn_lease =
      occu(data = umf, 
        formula = 
          as.formula(paste(best_det, 
          "ever_burned + leased", 
          sep = " ~ ")))
  ); beep("ping")

aictab(mgmt_models)

# Get best management
best_mgmt <-
  as.character(pluck(mgmt_models, aictab(mgmt_models)[1, 1])@formula[3])

# EAME: Lease good (n.s.), burn bad (n.s.)
# GRSP: Lease good (***), burn bad (n.s.)

gc()

mgmt_subset_models <-
  list(
    null = m.psi0,
    burnclass =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "burn_class", 
         sep = " ~ "))),
    burnclass_p.null =
      occu(data = umf,
       formula = ~ 1 ~ burn_class),
    burntime =
      occu(data = umf, 
       formula = ~1 ~ scale(t_since_burn)),
    burnprop =
      occu(data = umf, 
       formula = 
         ~ 1 ~ scale(prop_burned)),
    limit =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "harvest_limit", 
         sep = " ~ "))),
    years_limited = 
      occu(data = umf,
        formula = 
          as.formula(paste(best_det,
          "scale(years_limited)",
          sep = " ~ ")))
  ); beep("ping")

aictab(mgmt_subset_models)

# Time since burn
mgmt_subset_models$burnclass
mgmt_subset_models$burnclass_p.null
mgmt_subset_models$burntime
  # EAME: No impact of burn class, # of years since burn is bad (*) barely
  # GRSP: Burn 0-2yrs good (*), # of years since burn is bad (**)

# Area burned
mgmt_subset_models$burnprop
  # EAME: Nope
  # GRSP: Nope

mgmt_subset_models$limit
mgmt_subset_models$years_limited
  # EAME: Limit is better (***). More years limited better (**)
  # GRSP: Limit is better (**) and number of years has no effect.

## Landscape submodel ----

# What are the most important landscape-level effects?

# Landscape variables:
land_models <-
  list(
    null = m.psi0,
  # Singles
    # Grass
    grs_250 =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(grs_250)", 
         sep = " ~ "))),
    grs_500 =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(grs_500)", 
         sep = " ~ "))),
    grs_1000 =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(grs_1000)", 
         sep = " ~ "))),
    grs_2500 =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(grs_2500)", 
         sep = " ~ "))),
    # grs_5000 =
    #   occu(data = umf, 
    #    formula = 
    #      as.formula(paste(best_det, 
    #      "scale(grs_5000)", 
    #      sep = " ~ "))),
    # Forest
    for_250 =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(for_250)", 
         sep = " ~ "))),
    for_500 =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(for_500)", 
         sep = " ~ "))),
    for_1000 =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(for_1000)", 
         sep = " ~ "))),
    for_2500 =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(for_2500)", 
         sep = " ~ "))),
    # for_5000 =
    #   occu(data = umf, 
    #    formula = 
    #      as.formula(paste(best_det, 
    #      "scale(for_5000)", 
    #      sep = " ~ "))),
    # Development
    dvp_250 =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(dvp_250)", 
         sep = " ~ "))),
    dvp_500 =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(dvp_500)", 
         sep = " ~ "))),
    dvp_1000 =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(dvp_1000)", 
         sep = " ~ "))),
    dvp_2500 =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(dvp_2500)", 
         sep = " ~ "))),
    # dvp_5000 =
    #   occu(data = umf, 
    #    formula = 
    #      as.formula(paste(best_det, 
    #      "scale(dvp_5000)", 
    #      sep = " ~ "))),
    # Cropland
    crp_250 =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(crp_250)", 
         sep = " ~ "))),
    crp_500 =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(crp_500)", 
         sep = " ~ "))),
    crp_1000 =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(crp_1000)", 
         sep = " ~ "))),
    crp_2500 =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(crp_2500)", 
         sep = " ~ "))),
    # crp_5000 =
    #   occu(data = umf, 
    #    formula = 
    #      as.formula(paste(best_det, 
    #      "scale(crp_5000)", 
    #      sep = " ~ "))),
  # Hypotheses
    # Close forest, far grass
    for250_grs1k =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(for_250) + scale(grs_1000)", 
         sep = " ~ "))),
    for250_grs2k =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(for_250) + scale(grs_2500)", 
         sep = " ~ "))),
    # Close grass, far forest
    grs250_for2k =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(grs_250) + scale(for_2500)", 
         sep = " ~ "))),
    grs250_for1k =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(grs_250) + scale(for_1000)", 
         sep = " ~ "))),
    # Close grass, far development
    grs250_dvp2k =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(grs_250) + scale(dvp_2500)", 
         sep = " ~ "))),
    grs250_dvp1k =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(grs_250) + scale(dvp_1000)", 
         sep = " ~ "))),
   # Close grass, far crop
    grs250_crp2k =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(grs_250) + scale(crp_2500)", 
         sep = " ~ "))),
    grs250_crp1k =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(grs_250) + scale(crp_1000)", 
         sep = " ~ "))),
   # Close grass, far crop, far development
    grs250_crp1k_dvp2k =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(grs_250) + scale(crp_1000) + scale(dvp_2500)", 
         sep = " ~ "))),
  # Close grass, far forest, far development
    grs250_for2k_dvp2k =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(grs_250) + scale(for_2500) + scale(dvp_2500)", 
         sep = " ~ "))),
    # Close grass, close development, far crop
    grs250_dvp250_crp2k =
      occu(data = umf, 
       formula = 
         as.formula(paste(best_det, 
         "scale(grs_250) + scale(crp_2500) + scale(dvp_250)", 
         sep = " ~ "))),
    # Close crop, far grs
    grs500_crp1k =
      occu(data = umf, 
         formula = 
           as.formula(paste(best_det, 
           "scale(grs_500) + scale(crp_1000)", 
           sep = " ~ "))),
    # Multi close
      close =
        occu(data = umf, 
         formula = 
           as.formula(paste(best_det, 
           "scale(grs_250) + scale(for_250) + scale(dvp_250)", 
           sep = " ~ ")))
      ) ; beep("ping")

aictab(land_models)  

# EAME: Best single is forest 5km (+) but if I eliminate 5km scale,
#       then best single is grass 250m (+). 
#       Best combo tested is close grass+, close development- broad crop- [is this just park?].
# GRSP: Best single is forest 250m (-)
#       Best combo tested is close grass (+) close forest (-) close dvp (-)

# Get best land
best_land <-
  as.character(pluck(land_models, aictab(land_models)[1, 1])@formula[3])

# Combine occupancy model -------------------------------------------------

# All models will include year to account for model structure

combo_models <-
  list(
    nullnull = m.0,
    null = m.psi0,
    # Trend
    year = trend_models$year,
    yearfct = trend_models$yearfct,
    parkxyear = trend_models$yearxpark,
    park = trend_models$park,
    # Casettes
    hab = 
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             paste(best_hab, best_trend, sep = "+"), 
             sep = " ~ "))),
    # hab_p = 
    #   occu(data = umf, 
    #        formula = 
    #          as.formula(paste(best_det, 
    #          paste(best_hab, best_trend, "park", sep = "+"), 
    #          sep = " ~ "))),
    land = 
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             paste(best_land, best_trend, sep = "+"), 
             sep = " ~ "))),
    # land_p = 
    #   occu(data = umf, 
    #        formula = 
    #          as.formula(paste(best_det, 
    #          paste(best_land, best_trend, "park", sep = "+"), 
    #          sep = " ~ "))),
    mgmt = 
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             paste(best_mgmt, best_trend, sep = "+"),
             sep = " ~ "))),
    # mgmt = 
    #   occu(data = umf, 
    #        formula = 
    #          as.formula(paste(best_det, 
    #          paste(best_mgmt, best_trend, "park", sep = "+"),
    #          sep = " ~ "))),
    # Habitat + land
    hab_land =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             paste(best_hab, best_land, best_trend, sep = "+"),
             sep = " ~ "))),
    # hab_land_p =
    #   occu(data = umf, 
    #        formula = 
    #          as.formula(paste(best_det, 
    #          paste(best_hab, best_land, best_trend, "park", sep = "+"),
    #          sep = " ~ "))),
    # Habitat + mgmt
    hab_mgmt =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             paste(best_hab, best_mgmt, best_trend, sep = "+"),
             sep = " ~ "))),
    # hab_mgmt_p =
    #   occu(data = umf, 
    #        formula = 
    #          as.formula(paste(best_det, 
    #          paste(best_hab, best_mgmt, best_trend, "park", sep = "+"),
    #          sep = " ~ "))),
    # Land + mgmt
    land_mgmt =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             paste(best_land, best_mgmt, best_trend, sep = "+"),
             sep = " ~ "))),
    # land_mgmt_p =
    #   occu(data = umf, 
    #        formula = 
    #          as.formula(paste(best_det, 
    #          paste(best_land, best_mgmt, best_trend, "park", sep = "+"),
    #          sep = " ~ "))),
    # Habitat + land + mgmt
    hab_land_mgmt =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             paste(best_hab, best_land, best_mgmt, best_trend, sep = "+"),
             sep = " ~ ")))
    # hab_land_mgmt_p =
    #   occu(data = umf, 
    #        formula = 
    #          as.formula(paste(best_det, 
    #          paste(best_hab, best_land, best_mgmt, best_trend, "park", sep = "+"),
    #          sep = " ~ ")))
    ) ; beep("ping")
  
aictab(combo_models)

best_formula <-
  pluck(combo_models, aictab(combo_models)[1, 1])@formula

best_full <-
  occu(data = umf, 
       formula = best_formula)

# Get every Psi formula
get_formula <-  
  function(x) {
      modname <- c(NULL)
      modformula <- c(NULL)
      for (n in 1:length(x)) {
        modname[n] <- names(x)[n]
        modformula[n] <- as.character(x[[n]]@formula)[3]
      }
      tibble(Modnames = modname, formula = modformula)
  }

# Save AIC tables ---------------------------------------------------------

AIC_tables <-
  list(
    detection = aictab(det_models),
    habitat = aictab(hab_models) %>% as_tibble() %>% 
      left_join(get_formula(hab_models)),
    trend = aictab(trend_models) %>% as_tibble() %>% 
      left_join(get_formula(trend_models)),
    landscape = aictab(land_models) %>% as_tibble() %>% 
      left_join(get_formula(land_models)),
    management = aictab(mgmt_models) %>% as_tibble() %>% 
      left_join(get_formula(mgmt_models)),
    management_subset = aictab(mgmt_subset_models) %>% as_tibble() %>% 
      left_join(get_formula(mgmt_subset_models)),
    combo = aictab(combo_models) %>% as_tibble() %>% 
      left_join(get_formula(combo_models))
  )

# Initialize excel document
write.xlsx(
  as.data.frame(AIC_tables$detection),
  paste0("output/aic_", sp, ".xlsx"),
  sheetName = "detection")

write.xlsx(
  as.data.frame(AIC_tables$habitat),
  paste0("output/aic_", sp, ".xlsx"),
  sheetName = "habitat",
  append = TRUE)

write.xlsx(
  as.data.frame(AIC_tables$trend),
  paste0("output/aic_", sp, ".xlsx"),
  sheetName = "trend",
  append = TRUE)

write.xlsx(
  as.data.frame(AIC_tables$management),
  paste0("output/aic_", sp, ".xlsx"),
  sheetName = "management",
  append = TRUE)

write.xlsx(
  as.data.frame(AIC_tables$management_subset),
  paste0("output/aic_", sp, ".xlsx"),
  sheetName = "management_subset",
  append = TRUE)

write.xlsx(
  as.data.frame(AIC_tables$combo),
  paste0("output/aic_", sp, ".xlsx"),
  sheetName = "combo",
  append = TRUE)

write.xlsx(
  as.data.frame(AIC_tables$landscape),
  paste0("output/aic_", sp, ".xlsx"),
  sheetName = "landscape",
  append = TRUE)


# Predict detection variables ---------------------------------------------

defaults_detection <-
  data.frame(
    doy = 
      mean(umf@obsCovs$doy, na.rm = T), 
    wind = 
      levels(umf@obsCovs$wind)[1],
    disturbance = 
      levels(umf@obsCovs$disturbance)[1],
    temperature = 
      mean(umf@obsCovs$temperature, na.rm = T), 
    time = 
      mean(umf@obsCovs$time, na.rm = T),
    observer = 
      names(which.max(table(umf@obsCovs$observer))))

# Wind 
pred.p_wind <-
  predict(
    best_full, 
    type = "det", 
    newdata = bind_cols(
      wind = levels(umf@obsCovs$wind), 
      select(defaults_detection, -wind)), 
    appendData = T)

pred.p_wind

# Disturbance 
pred.p_dist <-
  predict(
    best_full, 
    type = "det", 
    newdata = bind_cols(
      disturbance = levels(umf@obsCovs$disturbance), 
      select(defaults_detection, -disturbance)), 
    appendData = T)

pred.p_dist

# Doy 
pred.p_doy <-
  predict(
    best_full, 
    type = "det", 
    newdata = bind_cols(
      doy = seq(
        min(umf@obsCovs$doy, na.rm = T),
        max(umf@obsCovs$doy, na.rm = T),
        by = 1),
      select(defaults_detection, -doy)), 
    appendData = T)

pred.p_doy

# Temp 
pred.p_temp <-
  predict(
    best_full, 
    type = "det", 
    newdata = bind_cols(
      temperature = seq(
        min(umf@obsCovs$temperature, na.rm = T),
        max(umf@obsCovs$temperature, na.rm = T),
        by = 1),
      select(defaults_detection, -temperature)),
    appendData = T)

pred.p_temp

# Time 
pred.p_time <-
  predict(
    best_full, 
    type = "det", 
    newdata = bind_cols(
      time = seq(
        min(umf@obsCovs$time, na.rm = T),
        max(umf@obsCovs$time, na.rm = T),
        by = 1),
      select(defaults_detection, -time)),
    appendData = T)

pred.p_time

# Observer(just for fun)
predict(
  occu(data = umf, formula = ~ observer ~ 1), 
  type = "det", 
  newdata = 
    data.frame(
      observer = factor(levels(obs_covs$observer$observer_1),
             levels = levels(obs_covs$observer$observer_1))),
  appendData = T) %>% 
  as_tibble() %>% 
  ggplot(aes(x = reorder(observer, Predicted), y = Predicted)) + 
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  labs(y = paste0("Detection of ", sp_long),
       x = "") +
  coord_flip() +
  theme_bw()

  ggsave(
    filename = paste0("output/plots/observers_", sp, ".png"), 
    width = 4, height = 5, units = "in")

# Predict occupancy variables ---------------------------------------------
best_formula
best_full
names(siteyear_covs)

## Defaults ----
defaults <-
  list(
    angle_max = 
      mean(distinct(siteyear_covs, grts, .keep_all = T)$angle_max, na.rm = T), 
    angle_mean = 
      mean(distinct(siteyear_covs, grts, .keep_all = T)$angle_mean, na.rm = T),
    burn_class = 
      factor(levels(siteyear_covs$burn_class)[1], 
             levels = levels(siteyear_covs$burn_class)),
    crp_1000 =   
      mean(distinct(siteyear_covs, grts, .keep_all = T)$crp_1000, na.rm = T),
    crp_250 = 
      mean(distinct(siteyear_covs, grts, .keep_all = T)$crp_250, na.rm = T),
    crp_2500 =
      mean(distinct(siteyear_covs, grts, .keep_all = T)$crp_2500, na.rm = T),
    crp_500 =
      mean(distinct(siteyear_covs, grts, .keep_all = T)$crp_2500, na.rm = T),
    crp_5000 =
      mean(distinct(siteyear_covs, grts, .keep_all = T)$crp_5000, na.rm = T),
    dvp_1000 =    
      mean(distinct(siteyear_covs, grts, .keep_all = T)$dvp_1000, na.rm = T),
    dvp_250 =   
      mean(distinct(siteyear_covs, grts, .keep_all = T)$dvp_250, na.rm = T),
    dvp_2500 = 
      mean(distinct(siteyear_covs, grts, .keep_all = T)$dvp_2500, na.rm = T),
    dvp_500 =  
      mean(distinct(siteyear_covs, grts, .keep_all = T)$dvp_500, na.rm = T),
    dvp_5000 =  
      mean(distinct(siteyear_covs, grts, .keep_all = T)$dvp_5000, na.rm = T),
    ever_burned = 
      factor(levels(siteyear_covs$ever_burned)[1], 
             levels = levels(siteyear_covs$ever_burned)),
    field_type =  
      factor("hayfield", 
             levels = levels(siteyear_covs$field_type)),
    for_1000 = 
      mean(distinct(siteyear_covs, grts, .keep_all = T)$for_1000, na.rm = T),
    for_250 =  
      mean(distinct(siteyear_covs, grts, .keep_all = T)$for_250, na.rm = T),
    for_2500 = 
      mean(distinct(siteyear_covs, grts, .keep_all = T)$for_2500, na.rm = T),
    for_500 =      
      mean(distinct(siteyear_covs, grts, .keep_all = T)$for_500, na.rm = T),
    for_5000 =  
      mean(distinct(siteyear_covs, grts, .keep_all = T)$for_5000, na.rm = T),
    grass_type =
      factor(levels(siteyear_covs$grass_type)[1],
             levels = levels(siteyear_covs$grass_type)),
    grs_1000 = 
      mean(distinct(siteyear_covs, grts, .keep_all = T)$grs_1000, na.rm = T),
    grs_250 = 
      mean(distinct(siteyear_covs, grts, .keep_all = T)$grs_250, na.rm = T),
    grs_2500 = 
      mean(distinct(siteyear_covs, grts, .keep_all = T)$grs_2500, na.rm = T),
    grs_500 =     
      mean(distinct(siteyear_covs, grts, .keep_all = T)$grs_500, na.rm = T),
    grs_5000 =  
      mean(distinct(siteyear_covs, grts, .keep_all = T)$grs_5000, na.rm = T),
    habitat =   
      factor("CSG hayfield",
             levels = levels(siteyear_covs$habitat)),
    harvest_limit =
      factor(levels(siteyear_covs$harvest_limit)[1],
             levels = levels(siteyear_covs$harvest_limit)),
    leased =   
      factor(levels(siteyear_covs$leased)[2],
             levels = levels(siteyear_covs$leased)),
    park =  "ANTI",
    prop_burned = 
      mean(siteyear_covs$prop_burned, na.rm = T),
    shrub =
      mean(distinct(siteyear_covs, grts, .keep_all = T)$shrub, na.rm = T),
    t_since_burn = 2,
    year = 4,         
    year_fct =  
      factor(4,
             levels = levels(siteyear_covs$year_fct)),
    years_limited = 0
  )

## Habitat ----
### Habitat
pred.psi_habitat <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"habitat"),
      habitat = levels(defaults$habitat)
      )) %>% 
  select(habitat, 1:4)

pred.psi_habitat_1 <-
  predict(
    hab_models$habitat, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"habitat"),
      habitat = levels(defaults$habitat)
      )) %>% 
  select(habitat, 1:4)

### Field type
pred.psi_fieldtype <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"field_type"),
      field_type = levels(defaults$field_type)
      )) %>% 
  select(field_type, 1:4)

pred.psi_fieldtype_1 <-
  predict(
    hab_models$field, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"field_type"),
      field_type = levels(defaults$field_type)
      )) %>% 
  select(field_type, 1:4)

### Shrub
pred.psi_shrub <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"shrub"),
      shrub = 
        seq(from = min(umf@siteCovs$shrub, na.rm = T),
            to =   max(umf@siteCovs$shrub, na.rm = T),
            length.out = 30)
      )) %>% 
  select(shrub, 1:4)

pred.psi_shrub_1 <-
  predict(
    hab_models$shrub, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"shrub"),
      shrub = 
        seq(from = min(umf@siteCovs$shrub, na.rm = T),
            to =   max(umf@siteCovs$shrub, na.rm = T),
            length.out = 30)
      )) %>% 
  select(shrub, 1:4)

### Angle

pred.psi_anglemax <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"angle_max"),
      angle_max = 
        seq(from = min(umf@siteCovs$angle_max, na.rm = T),
            to =   max(umf@siteCovs$angle_max, na.rm = T),
            length.out = 30)
      )) %>% 
  select(angle_max, 1:4)

pred.psi_anglemax_1 <-
  predict(
    hab_models$anglemax, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"angle_max"),
      angle_max = 
        seq(from = min(umf@siteCovs$angle_max, na.rm = T),
            to =   max(umf@siteCovs$angle_max, na.rm = T),
            length.out = 30)
      )) %>% 
  select(angle_max, 1:4)

pred.psi_anglemean <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"angle_mean"),
      angle_mean = 
        seq(from = min(umf@siteCovs$angle_mean, na.rm = T),
            to =   max(umf@siteCovs$angle_mean, na.rm = T),
            length.out = 30)
      )) %>% 
  select(angle_mean, 1:4)

pred.psi_anglemean_1 <-
  predict(
    hab_models$anglemean, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"angle_mean"),
      angle_mean = 
        seq(from = min(umf@siteCovs$angle_mean, na.rm = T),
            to =   max(umf@siteCovs$angle_mean, na.rm = T),
            length.out = 30)
      )) %>% 
  select(angle_mean, 1:4)

## Landscape ----
### Forest ----
pred.psi_for250 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"for_250"),
      for_250 = 
        seq(from = min(umf@siteCovs$for_250, na.rm = T),
            to =   max(umf@siteCovs$for_250, na.rm = T),
            length.out = 30)
      )) %>% 
  select(for_250, 1:4)

pred.psi_for500 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"for_500"),
      for_500 = 
        seq(from = min(umf@siteCovs$for_500, na.rm = T),
            to =   max(umf@siteCovs$for_500, na.rm = T),
            length.out = 30)
      )) %>% 
  select(for_500, 1:4)

pred.psi_for1000 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"for_1000"),
      for_1000 = 
        seq(from = min(umf@siteCovs$for_1000, na.rm = T),
            to =   max(umf@siteCovs$for_1000, na.rm = T),
            length.out = 30)
      )) %>% 
  select(for_1000, 1:4)

pred.psi_for2500 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"for_2500"),
      for_2500 = 
        seq(from = min(umf@siteCovs$for_2500, na.rm = T),
            to =   max(umf@siteCovs$for_2500, na.rm = T),
            length.out = 30)
      )) %>% 
  select(for_2500, 1:4)

pred.psi_for5000 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"for_5000"),
      for_5000 = 
        seq(from = min(umf@siteCovs$for_5000, na.rm = T),
            to =   max(umf@siteCovs$for_5000, na.rm = T),
            length.out = 30)
      )) %>% 
  select(for_5000, 1:4)

### Grassland ----
pred.psi_grs250 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"grs_250"),
      grs_250 = 
        seq(from = min(umf@siteCovs$grs_250, na.rm = T),
            to =   max(umf@siteCovs$grs_250, na.rm = T),
            length.out = 30)
      )) %>% 
  select(grs_250, 1:4)

pred.psi_grs500 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"grs_500"),
      grs_500 = 
        seq(from = min(umf@siteCovs$grs_500, na.rm = T),
            to =   max(umf@siteCovs$grs_500, na.rm = T),
            length.out = 30)
      )) %>% 
  select(grs_500, 1:4)

pred.psi_grs1000 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"grs_1000"),
      grs_1000 = 
        seq(from = min(umf@siteCovs$grs_1000, na.rm = T),
            to =   max(umf@siteCovs$grs_1000, na.rm = T),
            length.out = 30)
      )) %>% 
  select(grs_1000, 1:4)

pred.psi_grs2500 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"grs_2500"),
      grs_2500 = 
        seq(from = min(umf@siteCovs$grs_2500, na.rm = T),
            to =   max(umf@siteCovs$grs_2500, na.rm = T),
            length.out = 30)
      )) %>% 
  select(grs_2500, 1:4)

pred.psi_grs5000 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"grs_5000"),
      grs_5000 = 
        seq(from = min(umf@siteCovs$grs_5000, na.rm = T),
            to =   max(umf@siteCovs$grs_5000, na.rm = T),
            length.out = 30)
      )) %>% 
  select(grs_5000, 1:4)

### Cropland ----
pred.psi_crp250 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"crp_250"),
      crp_250 = 
        seq(from = min(umf@siteCovs$crp_250, na.rm = T),
            to =   max(umf@siteCovs$crp_250, na.rm = T),
            length.out = 30)
      )) %>% 
  select(crp_250, 1:4)

pred.psi_crp500 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"crp_500"),
      crp_500 = 
        seq(from = min(umf@siteCovs$crp_500, na.rm = T),
            to =   max(umf@siteCovs$crp_500, na.rm = T),
            length.out = 30)
      )) %>% 
  select(crp_500, 1:4)

pred.psi_crp1000 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"crp_1000"),
      crp_1000 = 
        seq(from = min(umf@siteCovs$crp_1000, na.rm = T),
            to =   max(umf@siteCovs$crp_1000, na.rm = T),
            length.out = 30)
      )) %>% 
  select(crp_1000, 1:4)

pred.psi_crp2500 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"crp_2500"),
      crp_2500 = 
        seq(from = min(umf@siteCovs$crp_2500, na.rm = T),
            to =   max(umf@siteCovs$crp_2500, na.rm = T),
            length.out = 30)
      )) %>% 
  select(crp_2500, 1:4)

pred.psi_crp5000 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"crp_5000"),
      crp_5000 = 
        seq(from = min(umf@siteCovs$crp_5000, na.rm = T),
            to =   max(umf@siteCovs$crp_5000, na.rm = T),
            length.out = 30)
      )) %>% 
  select(crp_5000, 1:4)

### Developed ----
pred.psi_dvp250 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"dvp_250"),
      dvp_250 = 
        seq(from = min(umf@siteCovs$dvp_250, na.rm = T),
            to =   max(umf@siteCovs$dvp_250, na.rm = T),
            length.out = 30)
      )) %>% 
  select(dvp_250, 1:4)

pred.psi_dvp500 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"dvp_500"),
      dvp_500 = 
        seq(from = min(umf@siteCovs$dvp_500, na.rm = T),
            to =   max(umf@siteCovs$dvp_500, na.rm = T),
            length.out = 30)
      )) %>% 
  select(dvp_500, 1:4)

pred.psi_dvp1000 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"dvp_1000"),
      dvp_1000 = 
        seq(from = min(umf@siteCovs$dvp_1000, na.rm = T),
            to =   max(umf@siteCovs$dvp_1000, na.rm = T),
            length.out = 30)
      )) %>% 
  select(dvp_1000, 1:4)

pred.psi_dvp2500 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"dvp_2500"),
      dvp_2500 = 
        seq(from = min(umf@siteCovs$dvp_2500, na.rm = T),
            to =   max(umf@siteCovs$dvp_2500, na.rm = T),
            length.out = 30)
      )) %>% 
  select(dvp_2500, 1:4)

pred.psi_dvp5000 <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"dvp_5000"),
      dvp_5000 = 
        seq(from = min(umf@siteCovs$dvp_5000, na.rm = T),
            to =   max(umf@siteCovs$dvp_5000, na.rm = T),
            length.out = 30)
      )) %>% 
  select(dvp_5000, 1:4)

## Management ----
# Ever burned
pred.psi_everburned <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"ever_burned"),
      ever_burned = levels(defaults$ever_burned)
      )) %>% 
  select(ever_burned, 1:4)

pred.psi_everburned_1 <-
  predict(
    mgmt_models$burn, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"ever_burned"),
      ever_burned = levels(defaults$ever_burned)
      )) %>% 
  select(ever_burned, 1:4)

# Leased
pred.psi_leased <-
  predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"leased"),
      leased = levels(defaults$leased)
      )) %>% 
  select(leased, 1:4)

pred.psi_leased_1 <-
  predict(
    mgmt_models$lease, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"leased"),
      leased = levels(defaults$leased)
      )) %>% 
  select(leased, 1:4)

### Submodels ----

#### Burn class
pred.psi_burnclass <-
  predict(
    mgmt_subset_models$burnclass_p.null, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"burn_class"),
      burn_class = levels(defaults$burn_class)
      )) %>% 
  select(burn_class, 1:4)

#### Years since burn
pred.psi_burntime <-
  predict(
    mgmt_subset_models$burntime, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"t_since_burn"),
      t_since_burn = 0:10
      )) %>% 
  select(t_since_burn, 1:4)

#### Harvest limit
pred.psi_limit <-
  predict(
    mgmt_subset_models$limit, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"harvest_limit"),
      harvest_limit = levels(defaults$harvest_limit)
      )) %>% 
  select(harvest_limit, 1:4)

#### Years limited
pred.psi_yearslimited <-
  predict(
    mgmt_subset_models$years_limited, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"years_limited"),
      years_limited = 0:10
      )) %>% 
  select(years_limited, 1:4)

## Trends ----
### Year 
pred.psi_year <-
   predict(
    best_full, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"year"),
      year = c(1:6, 8)
      )) %>% 
  select(year, 1:4) %>% 
  mutate(year = c(2014:2019, 2021)) 

pred.psi_year_1 <-
   predict(
    trend_models$year, type = "state", appendData = T, 
    newdata = data.frame(
      tidyselect:::select(defaults, !"year"),
      year = c(1:6, 8)
      )) %>% 
  select(year, 1:4) %>% 
  mutate(year = c(2014:2019, 2021)) 

# Occupancy betas (full model) --------------------------------------------

# library(broom) # Needed? Unclear
best_full_tbl <- tidy(best_full)


# Save all models ---------------------------------------------------------

models <-
  c("m.0",
    "m.psi0",
    "det_models", 
    "trend_models", 
    "hab_models", 
    "land_models", 
    "mgmt_models", 
    "mgmt_subset_models",
    "combo_models",
    "best_full")

write_rds(
  mget(models), 
  paste0("output/all_fitted_models_", sp, ".rds"))

write_rds(
  best_full_tbl, 
  paste0("output/best_model_", sp, ".rds"))


# Save all predictions ----------------------------------------------------

bind_rows(
  p.pred =
    mget(ls(pattern = "pred.p_")) %>% 
      bind_rows(.id = "model") %>% 
      as_tibble(),
  psi.pred =
    mget(ls(pattern = "pred.psi_")) %>% 
      bind_rows(.id = "model") %>% 
      as_tibble()) %>% 
    mutate(species = sp,
           common_name = sp_long) %>% 
  
  write_rds(paste0("output/predicted_", sp, ".rds"))

# Load outputs for testing purposes ---------------------------------------

read_rds(paste0("output/all_fitted_models_", sp, ".rds")) |> 
  list2env(.GlobalEnv)

read_rds(paste0("output/best_model_", sp, ".rds")) |> 
  list2env(.GlobalEnv)



