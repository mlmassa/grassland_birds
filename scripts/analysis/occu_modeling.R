# This script runs and compares models using the occu package.
# Required scripts: unmarked_statichack_setup.R (generates UMF)

# Setup -------------------------------------------------------------------

# Load required packages
library(unmarked)
library(tidyverse)
library(AICcmodavg)
library(beepr)
library(xlsx)

# Set focal species

# sp <- 'EAME'
# sp_long <- 'Eastern Meadowlark'

sp <- 'GRSP'
sp_long <- 'Grasshopper Sparrow'

# sp <- 'FISP'
# sp_long <- 'Field Sparrow'

# sp <- 'RWBL'
# sp_long <- 'Red-winged Blackbird'

# Import UMF
read_rds('data/processed/umf.rds') %>% 
  list2env(.GlobalEnv)

siteyear_covs$ever_burned <-
  as.factor(siteyear_covs$ever_burned)

umf <-
  unmarkedFrameOccu(
    y = 
      detections %>% 
      filter(species == sp) %>% 
      select(-grts, -year, -species) %>% 
      column_to_rownames(var = 'grts_year'),
    siteCovs = siteyear_covs,
    obsCovs = obs_covs)

summary(umf)

# Clean up
gc()


# Null model --------------------------------------------------------------

# All formulas take the form ~ p (detection) ~ Psi (occupancy)
m.0 <- 
  occu(data = umf, 
       formula = ~ 1 ~ 1)


# Detection models --------------------------------------------------------


# Suite of detection models
det_models <-
  list(
    null = m.0,
    all = 
      occu(data = umf, 
           formula = ~ observer + doy + disturbance + wind + temperature + time ~ 1),
    # Single models
    obs = 
      occu(data = umf, 
           formula = ~ observer ~ 1), 
    doy = 
      occu(data = umf, 
           formula = ~ doy ~ 1), 
    wind = 
      occu(data = umf, 
           formula = ~ wind ~ 1),
    dist =
      occu(data = umf,
           formula = ~ disturbance ~ 1),
    temp = 
      occu(data = umf, 
           formula = ~ temperature ~ 1),
    time = 
      occu(data = umf, 
           formula = ~ time ~ 1), 
    # 2-models
    wind_obs = 
      occu(data = umf, 
           formula = ~ wind + observer ~ 1),
    wind_doy = 
      occu(data = umf, 
           formula = ~ wind + doy ~ 1),
    wind_temp = 
      occu(data = umf, 
           formula = ~ wind + temperature ~ 1),
    wind_time = 
      occu(data = umf, 
           formula = ~ wind + time ~ 1),
    wind_dist =
      occu(data = umf,
           formula = ~ wind + disturbance ~ 1),
    obs_doy = 
      occu(data = umf, 
           formula = ~ observer + doy ~ 1),
    obs_temp = 
      occu(data = umf, 
           formula = ~ observer + temperature ~ 1),
    obs_time = 
      occu(data = umf, 
           formula = ~ observer + time ~ 1),
    doy_temp = 
      occu(data = umf, 
           formula = ~ doy + temperature ~ 1),
    doy_time = 
      occu(data = umf, 
           formula = ~ doy + time ~ 1),
    temp_time = 
      occu(data = umf, 
           formula = ~ temperature + time ~ 1),
    # 3-models
    wind_obs_doy =
      occu(data = umf, 
           formula = ~ wind + observer + doy ~ 1),
    wind_obs_temp = 
      occu(data = umf, 
           formula = ~ wind + observer + temperature ~ 1),
    wind_obs_time = 
      occu(data = umf, 
           formula = ~ wind + observer + time ~ 1),
    wind_doy_temp =
      occu(data = umf, 
           formula = ~ wind + doy + temperature ~ 1),
    wind_doy_time =
      occu(data = umf, 
           formula = ~ wind + doy + time ~ 1),
    wind_temp_time = 
      occu(data = umf, 
           formula = ~ wind + temperature + time ~ 1),
    obs_doy_temp = 
      occu(data = umf, 
           formula = ~ observer + doy + temperature ~ 1),
    obs_doy_time = 
      occu(data = umf, 
           formula = ~ observer + doy + temperature ~ 1),
    obs_temp_time = 
      occu(data = umf, 
           formula = ~ observer + temperature + time ~ 1),
    doy_temp_time = 
      occu(data = umf, 
           formula = ~ doy + temperature + time ~ 1),
    # 4-models
    wind_doy_temp_time =
      occu(data = umf, 
           formula = ~ wind + doy + temperature + time ~ 1),
    wind_doy_temp_dist = 
      occu(data = umf,
           formula = ~ wind + doy + temperature + disturbance ~ 1)
    ); beep('coin')


aictab(det_models)

# The following creates a Psi formula using that species's best det model
best_det <-
  as.character(pluck(det_models, aictab(det_models)[1, 1])@formula)[2]

# WINNING DETECTION MODELS
# EAME: disturbance
# GRSP: wind (not by much)

# FISP: disturbance (with others competitive)
# RBWL: time

# Annual trend models -----------------------------------------------------

m.psi0 <- 
  occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             '1', 
             sep = ' ~ ')))

trend_models <-
  list(
    null = m.psi0,
    year =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'year', 
             sep = ' ~ '))),
    yearfct =
      occu(data = umf,
           formula = 
             as.formula(paste(best_det, 
             'year_fct', 
             sep = ' ~ '))),
    year_park =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'year + park', 
             sep = ' ~ '))),
    yearfct_park =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'year_fct + park', 
             sep = ' ~ '))),
    yearxpark =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'year * park', 
             sep = ' ~ '))),
    yearfctxpark =
      occu(data = umf,
           formula = 
             as.formula(paste(best_det, 
             'year_fct * park', 
             sep = ' ~ ')))
  ) ; beep('coin')

aictab(trend_models)

# The following creates a Psi formula using that species's best trend model (with NO park)
best_trend <-
  as.character(pluck(trend_models, 
                     pull(filter(as_tibble(aictab(trend_models)), 
                                 !str_detect(Modnames, 'park'))[1, 1]))@formula)[3]

# GRSP: Across all, there's a small (-0.0852/yr) but sig. (p = 0.02) trend. Num > fct year
# EAME: Worse than null: NO trend across years Fct > num year but only slightly.
# FISP: year fct
# RWBL: Non-significantly positive. Fct > num 


trend_models$yearxpark
# Park-by-park, there are not significant annual trends (GRSP, RWBL) 
# or are negative and significant in some parks (EAME; FISP in MONO) 
# while being positive and significant in others (FISP in MANA, ANTI)

trend_models$yearfct_park



# Occupancy models --------------------------------------------------------


## Habitat submodel ----

# What is the impact of immediate habitat?

# Habitat variables (NAs) (+/- p)
#  field_type (42) (hay +***, meadow - ns, crop -***) 
#  clin_max (42) (-***) 
#  shrub_mean (42) (-***)

# Habitat type (all)

hab_models <-
  list(
    null = m.psi0,
    field_clin_shrub =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'field_type + clin_max + shrub_mean', 
             sep = ' ~ '))),
    # Single models
    field =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'field_type', 
             sep = ' ~ '))),
    shrub =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'shrub_mean', 
             sep = ' ~ '))),
    clin =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'clin_max', 
             sep = ' ~ '))),
    # 2-models
    field_clin =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'field_type + clin_max', 
             sep = ' ~ '))),
    field_shrub = 
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'field_type + shrub_mean', 
             sep = ' ~ '))),
    clin_shrub =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'clin_max + shrub_mean', 
             sep = ' ~ ')))); beep('coin')



aictab(hab_models)

# The following creates a Psi formula using that species's best hab model
best_hab <-
  as.character(pluck(hab_models, aictab(hab_models)[1, 1])@formula[3])

# BEST HABITAT MODELS:
# GRSP: All three field+clin+shrub
# EAME: Only field+clin
# FISP: Only field+clin
# RWBL: All three field+clin+shrub


## Management submodel ----

# What is the impact of burning and harvesting?

# Management variables: 
#  burn_class (1634) (0-2 + ns, 3+ - ns) # It's bad to not burn <3yrs
#  harvest day (1428) (-, ns) # No impact of DATE of limit, but little var.
#  harvest limit (653) (+***) # Harvest limit is good!
#  park (ANTI +***, HAFE - ns, MANA +*, MONO -***)
#  leased (+***)

# Management (ag)
mgmt_models <-
  list(
    null = m.psi0,
    # Singles
    park = 
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'park', 
             sep = ' ~ '))),
    lease =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'leased', 
             sep = ' ~ '))),
    burn =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'ever_burned', 
             sep = ' ~ '))),
    # 2-models
    park_lease =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'park + leased', 
             sep = ' ~ '))),
    park_burn = 
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'park + ever_burned', 
             sep = ' ~ '))),
    lease_burn = 
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'leased + ever_burned', 
             sep = ' ~ '))),
    # all 3
    park_lease_burn = 
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'park + leased + ever_burned', 
             sep = ' ~ ')))
  ); beep('coin')

aictab(mgmt_models)

# Get best management
best_mgmt <-
  as.character(pluck(mgmt_models, aictab(mgmt_models)[1, 1])@formula[3])

# FISP: No lease. Yes park. Yes burn.
# RWBL: Lease good. Yes park.
# EAME: Lease good. Yes park.
# GRSP: Lease good. Yes park. Burn bad but not sig.

mgmt_subset_models <-
  list(
    null = m.psi0,
    burnclass =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'burn_class2', 
             sep = ' ~ '))),
    burnclass_p.null =
      occu(data = umf,
           formula = ~ 1 ~ burn_class2),
    harvestday =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'harvest_day', 
             sep = ' ~ '))),
    limit =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'harvest_limit', 
             sep = ' ~ ')))
  ); beep('coin')

aictab(mgmt_subset_models)
mgmt_subset_models$burnclass
  # EAME: Nope
  # GRSP: No convergence; need p.null (3+ yrs bad, sig)
  mgmt_subset_models$burnclass_p.null
mgmt_subset_models$harvestday
  # EAME: Later is worse?
  # GRSP: No effect
mgmt_subset_models$limit
  # EAME: Limit is WAY better!
  # GRSP: Limit is better!

## Landscape submodel ----

# What are the most important landscape-level effects?

# Landscape variables:
#  grs_500m (+***), dvp_500m (-**), for_500m (-***), wet_500m (- ns)
#  grs_1km (+***), dvp_1km (+ ns), for_1km (-***), wet_1km (+ ns)
#  grs_5km (+ ns), dvp_5km (-**), for_5km (+***), wet_5km (+***)


land_models <-
  list(
    null = m.psi0,
    # Singles
    # Grass
    grs_500m =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'grs_500m', 
             sep = ' ~ '))),
    grs_1km =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'grs_1km', 
             sep = ' ~ '))),
    grs_5km =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'grs_5km', 
             sep = ' ~ '))),
    # Forest
    for_500m =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'for_500m', 
             sep = ' ~ '))),
    for_1km =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'for_1km', 
             sep = ' ~ '))),
    for_5km =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'for_5km', 
             sep = ' ~ '))),
    # Development
    dvp_500m =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'dvp_500m', 
             sep = ' ~ '))),
    dvp_1km =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'dvp_1km', 
             sep = ' ~ '))),
    dvp_5km =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'dvp_5km', 
             sep = ' ~ '))),
    # Wetland
    wet_500m =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'wet_500m', 
             sep = ' ~ '))),
    wet_1km = 
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'wet_1km', 
             sep = ' ~ '))),
    wet_5km =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'wet_5km', 
             sep = ' ~ '))),
    # Hypotheses
    grs_for_500m =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'grs_500m + for_500m', 
             sep = ' ~ '))),
    grs_500m_for_5km =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'grs_500m + for_5km', 
             sep = ' ~ '))),
    grs_500m_dvp_5km =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'grs_500m + dvp_5km', 
             sep = ' ~ '))),
    land_500m =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'for_500m + grs_500m + dvp_500m', 
             sep = ' ~ '))),
    land_1km =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'for_1km + grs_1km + dvp_1km', 
             sep = ' ~ '))),
    land_5km =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             'for_5km + grs_5km',  # no dvp; multicollinearity
             sep = ' ~ ')))
      ) ; beep('coin')


aictab(land_models)  

# Get best land
best_land <-
  as.character(pluck(land_models, aictab(land_models)[1, 1])@formula[3])

# GRSP: 500m all 
# EAME: grs500m + dvp5km
# FISP: 1km all (dvp bad, others both good)
# RWBL: 5km for+grs (and no stupid wetland!)

# Combine occupancy model -------------------------------------------------


combo_models <-
  list(
    null = m.psi0,
    # Trend
    park_year = trend_models$year_park,
    parkxyear = trend_models$yearxpark,
    # Casettes
    hab = 
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             best_hab, 
             sep = ' ~ '))),
    land = 
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             best_land, 
             sep = ' ~ '))),
    mgmt = 
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             best_mgmt, 
             sep = ' ~ '))),
    # Habitat + land
    hab_land =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             paste(best_hab, best_land, sep = '+'),
             sep = ' ~ '))),
        # Habitat + land + year
        hab_land_yr =
          occu(data = umf, 
               formula = 
                 as.formula(paste(best_det, 
                 paste(best_hab, best_land, 'year', sep = '+'),
                 sep = ' ~ '))),
        # Habitat + land + year(fct)
        hab_land_yrf =
          occu(data = umf, 
               formula = 
                 as.formula(paste(best_det, 
                 paste(best_hab, best_land, 'year_fct', sep = '+'),
                 sep = ' ~ '))),
    # Habitat + mgmt
    hab_mgmt =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             paste(best_hab, best_mgmt, sep = '+'),
             sep = ' ~ '))),
        # Habitat + mgmt + year
        hab_mgmt_yr =
          occu(data = umf, 
               formula = 
                 as.formula(paste(best_det, 
                 paste(best_hab, best_mgmt, 'year', sep = '+'),
                 sep = ' ~ '))),
        # Habitat + mgmt + year(fct)
        hab_mgmt_yrf =
          occu(data = umf, 
               formula = 
                 as.formula(paste(best_det, 
                 paste(best_hab, best_mgmt, 'year_fct', sep = '+'),
                 sep = ' ~ '))),
    # Land + mgmt
    land_mgmt =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             paste(best_land, best_mgmt, sep = '+'),
             sep = ' ~ '))),
        # Land + mgmt + year
        land_mgmt_yr =
          occu(data = umf, 
               formula = 
                 as.formula(paste(best_det, 
                 paste(best_land, best_mgmt, 'year', sep = '+'),
                 sep = ' ~ '))),
        # Land + mgmt + year(fct)
        mgmt_land_yr =
          occu(data = umf, 
               formula = 
                 as.formula(paste(best_det, 
                 paste(best_land, best_mgmt, 'year_fct', sep = '+'),
                 sep = ' ~ '))),
    # Habitat + land + mgmt
    hab_land_mgmt =
      occu(data = umf, 
           formula = 
             as.formula(paste(best_det, 
             paste(best_hab, best_land, best_mgmt, sep = '+'),
             sep = ' ~ '))),
        # Habitat + land + mgmt + year
        hab_land_mgmt_yr =
          occu(data = umf, 
               formula = 
                 as.formula(paste(best_det, 
                 paste(best_hab, best_land, best_mgmt, 'year', sep = '+'),
                 sep = ' ~ '))),
        # Habitat + landscape + mgmt + year(fct)
        hab_land_mgmt_yrf =
          occu(data = umf, 
               formula = 
                 as.formula(paste(best_det, 
                 paste(best_hab, best_land, best_mgmt, 'year_fct', sep = '+'),
                 sep = ' ~ ')))
    ) ; beep('coin')
  
aictab(combo_models)
# Adding management casette improves AIC for all species
# BUT best model for RWBL is hab+mgmt, no landscape
# EAME year is within 1 AIC, but not top model. GRSP year is in top. RWBL year fct is in top.
# FISP hab land yearf is best???
# GRSP: Hab mgmt year

combo_models$hab_land_mgmt

combo_models$hab_mgmt

best_formula <-
  pluck(combo_models, aictab(combo_models)[1, 1])@formula

best_full <-
  occu(data = umf, 
       formula = best_formula)

# Get every Psi formula
list_formulas <-  
  function(x) {
      modname <- c(NULL)
      modformula <- c(NULL)
      for (n in 1:length(x)) {
        modname[n] <- names(x)[n]
        modformula[n] <- as.character(x[[n]]@formula)[3]
      }
      tibble(Modnames = modname, formula = modformula)
  }


aictab(trend_models) %>% as_tibble() %>% left_join(list_formulas(trend_models))


# Save AIC tables ---------------------------------------------------------

AIC_tables <-
  list(
    detection = aictab(det_models),
    habitat = aictab(hab_models) %>% as_tibble() %>% 
      left_join(list_formulas(hab_models)),
    trend = aictab(trend_models) %>% as_tibble() %>% 
      left_join(list_formulas(trend_models)),
    landscape = aictab(land_models) %>% as_tibble() %>% 
      left_join(list_formulas(land_models)),
    management = aictab(mgmt_models) %>% as_tibble() %>% 
      left_join(list_formulas(mgmt_models)),
    management_subset = aictab(mgmt_subset_models) %>% as_tibble() %>% 
      left_join(list_formulas(mgmt_subset_models)),
    combo = aictab(combo_models) %>% as_tibble() %>% 
      left_join(list_formulas(combo_models))
  )

# Initialize excel document
write.xlsx(
  as.data.frame(AIC_tables$detection),
  paste0('output/aic_', sp, '.xlsx'),
  sheetName = 'detection')

write.xlsx(
  as.data.frame(AIC_tables$habitat),
  paste0('output/aic_', sp, '.xlsx'),
  sheetName = 'habitat',
  append = TRUE)

write.xlsx(
  as.data.frame(AIC_tables$trend),
  paste0('output/aic_', sp, '.xlsx'),
  sheetName = 'trend',
  append = TRUE)

write.xlsx(
  as.data.frame(AIC_tables$management),
  paste0('output/aic_', sp, '.xlsx'),
  sheetName = 'management',
  append = TRUE)

write.xlsx(
  as.data.frame(AIC_tables$management_subset),
  paste0('output/aic_', sp, '.xlsx'),
  sheetName = 'management_subset',
  append = TRUE)

write.xlsx(
  as.data.frame(AIC_tables$combo),
  paste0('output/aic_', sp, '.xlsx'),
  sheetName = 'combo',
  append = TRUE)

write.xlsx(
  as.data.frame(AIC_tables$landscape),
  paste0('output/aic_', sp, '.xlsx'),
  sheetName = 'landscape',
  append = TRUE)


# Predict detection variables ---------------------------------------------

# Wind 
pred.p_wind <-
  predict(
    best_full, 
    type = 'det', 
    newdata = data.frame(
      wind = 
        factor(levels(obs_covs$wind$wind_1), 
               levels = levels(obs_covs$wind$wind_1))), 
    appendData = T)

pred.p_wind

# Disturbance 
pred.p_dist <-
  predict(
    best_full, 
    type = 'det', 
    newdata = data.frame(
      disturbance = 
        factor(levels(obs_covs$disturbance$disturb_1), 
               levels = levels(obs_covs$disturbance$disturb_1))),
    appendData = T)

pred.p_dist

# Doy 
pred.p_doy <-
  predict(
    best_full, 
    type = 'det', 
    newdata = 
      data.frame(
        doy = 
          seq(from = unname(summary(umf@obsCovs$doy)[1]),
              to =   unname(summary(umf@obsCovs$doy)[6]),
              length.out = 30)),
    appendData = T)

pred.p_doy

# Temp 
pred.p_temp <-
  predict(
    best_full, 
    type = 'det', 
    newdata = 
      data.frame(
        temperature = 
          seq(from = unname(summary(umf@obsCovs$temperature)[1]),
              to =   unname(summary(umf@obsCovs$temperature)[6]),
              length.out = 30)),
    appendData = T)

pred.p_temp

# Time 
pred.p_time <-
  predict(
    best_full, 
    type = 'det', 
    newdata = 
      data.frame(
        time = 
          seq(from = unname(summary(umf@obsCovs$time)[1]),
              to =   unname(summary(umf@obsCovs$time)[6]),
              length.out = 30)),
    appendData = T)

pred.p_time

# Observer(just for fun)
predict(
  occu(data = umf, formula = ~ observer ~ 1), 
  type = 'det', 
  newdata = 
    data.frame(
      observer = factor(levels(obs_covs$observer$observer_1),
             levels = levels(obs_covs$observer$observer_1))),
  appendData = T) %>% 
  as_tibble() %>% 
  ggplot(aes(x = reorder(observer, upper), y = Predicted)) + 
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  labs(y = paste0('Detection of ', sp_long),
       x = '') +
  coord_flip() +
  theme_bw()

  ggsave(
    filename = paste0('output/plots/observers_', sp, '.png'), 
    width = 4, height = 5, units = 'in')


# Predict occupancy variables ---------------------------------------------
best_formula
best_full

# Defaults
defaults <-
  list(
    field_type = factor('Hayfield', levels = levels(siteyear_covs$field_type)),
    shrub_mean = median(siteyear_covs$shrub_mean, na.rm = T),
    clin_max =median(siteyear_covs$clin_max, na.rm = T),
    grs_500m =median(siteyear_covs$grs_500m, na.rm = T),
    grs_1km = median(siteyear_covs$grs_1km, na.rm = T),
    grs_5km = median(siteyear_covs$grs_5km, na.rm = T),
    for_500m =median(siteyear_covs$for_500m, na.rm = T),
    for_1km = median(siteyear_covs$for_1km, na.rm = T),
    for_5km = median(siteyear_covs$for_5km, na.rm = T),
    dvp_500m =median(siteyear_covs$dvp_500m, na.rm = T),
    dvp_1km = median(siteyear_covs$dvp_1km, na.rm = T),
    dvp_5km = median(siteyear_covs$dvp_5km, na.rm = T),
    ever_burned = factor(0, levels = levels(siteyear_covs$leased)),
    park = factor('ANTI', levels = levels(siteyear_covs$park)),
    leased = factor(1, levels = levels(siteyear_covs$leased)),
    year = 3, 
    year_fct = factor(3, levels = levels(siteyear_covs$year_fct)))

## Habitat ----
### Field type 
pred.psi_field <-
  predict(
    best_full, type = 'state', appendData = T, 
    newdata = data.frame(
      field_type = factor(levels(siteyear_covs$field_type), 
                          levels = levels(siteyear_covs$field_type)),
      shrub_mean = defaults$shrub_mean, clin_max = defaults$clin_max,
      grs_500m = defaults$grs_500m, grs_1km = defaults$grs_1km, 
      grs_5km = defaults$grs_5km, for_500m = defaults$for_500m,
      for_1km = defaults$for_1km, for_5km = defaults$for_5km,
      dvp_500m = defaults$dvp_500m, dvp_1km = defaults$dvp_1km,
      dvp_5km = defaults$dvp_5km, ever_burned = defaults$ever_burned,
      park = defaults$park, leased = defaults$leased, year = defaults$year,
      year_fct = defaults$year_fct
      )) %>% 
  select(field_type, 1:4)

pred.psi_field

### Shrub mean
pred.psi_shrub <-
  predict(
    best_full, type = 'state', appendData = T, 
    newdata = data.frame(
      field_type = defaults$field_type,
      shrub_mean = 
        seq(from = min(umf@siteCovs$shrub_mean, na.rm = T),
            to =   max(umf@siteCovs$shrub_mean, na.rm = T),
            length.out = 30),
      clin_max = defaults$clin_max, grs_500m = defaults$grs_500m,
      grs_1km = defaults$grs_1km, grs_5km = defaults$grs_5km,
      for_500m = defaults$for_500m, for_1km = defaults$for_1km,
      for_5km = defaults$for_5km, dvp_500m = defaults$dvp_500m,
      dvp_1km = defaults$dvp_1km, dvp_5km = defaults$dvp_5km,
      ever_burned = defaults$ever_burned, park = defaults$park,
      leased = defaults$leased, year = defaults$year,
      year_fct = defaults$year_fct
      )) %>% 
  select(shrub_mean, 1:4)

pred.psi_shrub

### Angle

pred.psi_clin <-
  predict(
    best_full, type = 'state', appendData = T, 
    newdata = data.frame(
      field_type = defaults$field_type,
      shrub_mean = defaults$shrub_mean, 
      clin_max = 
        seq(from = min(umf@siteCovs$clin_max, na.rm = T),
            to =   max(umf@siteCovs$clin_max, na.rm = T),
            length.out = 30),
      grs_500m = defaults$grs_500m, grs_1km = defaults$grs_1km, 
      grs_5km = defaults$grs_5km, for_500m = defaults$for_500m,
      for_1km = defaults$for_1km, for_5km = defaults$for_5km,
      dvp_500m = defaults$dvp_500m, dvp_1km = defaults$dvp_1km,
      dvp_5km = defaults$dvp_5km, ever_burned = defaults$ever_burned,
      park = defaults$park, leased = defaults$leased, year = defaults$year,
      year_fct = defaults$year_fct
      )) %>% 
  select(clin_max, 1:4)

pred.psi_clin


## Landscape ----

### 500m ----
# Grassland 500m
pred.psi_grs_500m <-
  predict(
    best_full, type = 'state', appendData = T, 
    newdata = data.frame(
      field_type = defaults$field_type, shrub_mean = defaults$shrub_mean,
      clin_max = defaults$clin_max, grs_500m = 
        seq(from = min(umf@siteCovs$grs_500m, na.rm = T),
          to =   max(umf@siteCovs$grs_500m, na.rm = T),
          length.out = 30),
      grs_1km = defaults$grs_1km, grs_5km = defaults$grs_5km,
      for_500m = defaults$for_500m, for_1km = defaults$for_1km,
      for_5km = defaults$for_5km, dvp_500m = defaults$dvp_500m,
      dvp_1km = defaults$dvp_1km, dvp_5km = defaults$dvp_5km,
      ever_burned = defaults$ever_burned, park = defaults$park,
      leased = defaults$leased, year = defaults$year,
      year_fct = defaults$year_fct
      )) %>% 
  select(grs_500m, 1:4)

pred.psi_grs_500m

# Forest 500m
pred.psi_for_500m <-
  predict(
    best_full, type = 'state', appendData = T, 
    newdata = data.frame(
      field_type = defaults$field_type, shrub_mean = defaults$shrub_mean,
      clin_max = defaults$clin_max, grs_500m = defaults$grs_500m, 
      grs_1km = defaults$grs_1km, grs_5km = defaults$grs_5km,
      for_500m = 
        seq(from = min(umf@siteCovs$for_500m, na.rm = T),
          to =   max(umf@siteCovs$for_500m, na.rm = T),
          length.out = 30),
      for_1km = defaults$for_1km, for_5km = defaults$for_5km,
      dvp_500m = defaults$dvp_500m, dvp_1km = defaults$dvp_1km,
      dvp_5km = defaults$dvp_5km, ever_burned = defaults$ever_burned,
      park = defaults$park,leased = defaults$leased,
      year = defaults$year, year_fct = defaults$year_fct
      )) %>% 
  select(for_500m, 1:4)

pred.psi_for_500m

# Developed 500m
pred.psi_dvp_500m <-
  predict(
    best_full, type = 'state', appendData = T, 
    newdata = data.frame(
      field_type = defaults$field_type, shrub_mean = defaults$shrub_mean,
      clin_max = defaults$clin_max, grs_500m = defaults$grs_500m,
      grs_1km = defaults$grs_1km, grs_5km = defaults$grs_5km,
      for_500m = defaults$for_500m, for_1km = defaults$for_1km,
      for_5km = defaults$for_5km,
      dvp_1km = defaults$dvp_1km, dvp_5km = defaults$dvp_5km,
      ever_burned = defaults$ever_burned, park = defaults$park,
      leased = defaults$leased, year = defaults$year,
      year_fct = defaults$year_fct,
      dvp_500m = 
        seq(from = min(umf@siteCovs$dvp_500m, na.rm = T),
          to =   max(umf@siteCovs$dvp_500m, na.rm = T),
          length.out = 30)
      )) %>% 
  select(dvp_500m, 1:4)

pred.psi_dvp_500m

### 1km ----
# Grassland 1km
pred.psi_grs_1km <-
  predict(
    best_full, type = 'state', appendData = T, 
    newdata = data.frame(
      field_type = defaults$field_type, shrub_mean = defaults$shrub_mean,
      clin_max = defaults$clin_max, 
      grs_500m = defaults$grs_500m,
      grs_5km = defaults$grs_5km,
      for_500m = defaults$for_500m, for_1km = defaults$for_1km,
      for_5km = defaults$for_5km, dvp_500m = defaults$dvp_500m,
      dvp_1km = defaults$dvp_1km, dvp_5km = defaults$dvp_5km,
      ever_burned = defaults$ever_burned, park = defaults$park,
      leased = defaults$leased, year = defaults$year,
      year_fct = defaults$year_fct,
      grs_1km =
        seq(from = min(umf@siteCovs$grs_1km, na.rm = T),
          to =   max(umf@siteCovs$grs_1km, na.rm = T),
          length.out = 30)
      )) %>% 
  select(grs_1km, 1:4)

pred.psi_grs_1km

# Forest 1km
pred.psi_for_1km <-
  predict(
    best_full, type = 'state', appendData = T, 
    newdata = data.frame(
      field_type = defaults$field_type, shrub_mean = defaults$shrub_mean,
      clin_max = defaults$clin_max, grs_500m = defaults$grs_500m, 
      grs_1km = defaults$grs_1km, grs_5km = defaults$grs_5km,
      for_500m = defaults$for_500m, for_5km = defaults$for_5km,
      dvp_500m = defaults$dvp_500m, dvp_1km = defaults$dvp_1km,
      dvp_5km = defaults$dvp_5km, ever_burned = defaults$ever_burned,
      park = defaults$park,leased = defaults$leased,
      year = defaults$year, year_fct = defaults$year_fct,
      for_1km = 
        seq(from = min(umf@siteCovs$for_1km, na.rm = T),
          to =   max(umf@siteCovs$for_1km, na.rm = T),
          length.out = 30)
      )) %>% 
  select(for_1km, 1:4)

pred.psi_for_1km

# Developed 1km
pred.psi_dvp_1km <-
  predict(
    best_full, type = 'state', appendData = T, 
    newdata = data.frame(
      field_type = defaults$field_type, shrub_mean = defaults$shrub_mean,
      clin_max = defaults$clin_max, grs_500m = defaults$grs_500m,
      grs_1km = defaults$grs_1km, grs_5km = defaults$grs_5km,
      for_500m = defaults$for_500m, for_1km = defaults$for_1km,
      for_5km = defaults$for_5km,
      dvp_500m = defaults$dvp_500m, dvp_5km = defaults$dvp_5km,
      ever_burned = defaults$ever_burned, park = defaults$park,
      leased = defaults$leased, year = defaults$year,
      year_fct = defaults$year_fct,
      dvp_1km = 
        seq(from = min(umf@siteCovs$dvp_1km, na.rm = T),
          to =   max(umf@siteCovs$dvp_1km, na.rm = T),
          length.out = 30)
      )) %>% 
  select(dvp_1km, 1:4)

pred.psi_dvp_1km

### 5km ----
# Grassland 5km
pred.psi_grs_5km <-
  predict(
    best_full, type = 'state', appendData = T, 
    newdata = data.frame(
      field_type = defaults$field_type, shrub_mean = defaults$shrub_mean,
      clin_max = defaults$clin_max, grs_500m = defaults$grs_500m, 
      grs_1km = defaults$grs_1km, for_500m = defaults$for_500m, 
      for_1km = defaults$for_1km, for_5km = defaults$for_5km, 
      dvp_500m = defaults$dvp_500m, dvp_1km = defaults$dvp_1km, 
      dvp_5km = defaults$dvp_5km, ever_burned = defaults$ever_burned, 
      park = defaults$park, leased = defaults$leased, year = defaults$year,
      year_fct = defaults$year_fct,
      grs_5km =
        seq(from = min(umf@siteCovs$grs_5km, na.rm = T),
          to =   max(umf@siteCovs$grs_5km, na.rm = T),
          length.out = 30)
      )) %>% 
  select(grs_5km, 1:4)

pred.psi_grs_5km

# Forest 5km
pred.psi_for_5km <-
  predict(
    best_full, type = 'state', appendData = T, 
    newdata = data.frame(
      field_type = defaults$field_type, shrub_mean = defaults$shrub_mean,
      clin_max = defaults$clin_max, grs_500m = defaults$grs_500m, 
      grs_1km = defaults$grs_1km, grs_5km = defaults$grs_5km,
      for_500m = defaults$for_500m, for_1km = defaults$for_1km,
      dvp_500m = defaults$dvp_500m, dvp_1km = defaults$dvp_1km,
      dvp_5km = defaults$dvp_5km, ever_burned = defaults$ever_burned,
      park = defaults$park,leased = defaults$leased,
      year = defaults$year, year_fct = defaults$year_fct,
      for_5km = 
        seq(from = min(umf@siteCovs$for_5km, na.rm = T),
            to =   max(umf@siteCovs$for_5km, na.rm = T),
            length.out = 30)
      )) %>% 
  select(for_5km, 1:4)

pred.psi_for_5km

# Developed 5km
pred.psi_dvp_5km <-
  predict(
    best_full, type = 'state', appendData = T, 
    newdata = data.frame(
      field_type = defaults$field_type, shrub_mean = defaults$shrub_mean,
      clin_max = defaults$clin_max, grs_500m = defaults$grs_500m,
      grs_1km = defaults$grs_1km, grs_5km = defaults$grs_5km,
      for_500m = defaults$for_500m, for_1km = defaults$for_1km,
      for_5km = defaults$for_5km, dvp_500m = defaults$dvp_500m, 
      dvp_1km = defaults$dvp_1km, ever_burned = defaults$ever_burned, 
      park = defaults$park, leased = defaults$leased, 
      year = defaults$year, year_fct = defaults$year_fct,
      dvp_5km = 
        seq(from = min(umf@siteCovs$dvp_5km, na.rm = T),
            to =   max(umf@siteCovs$dvp_5km, na.rm = T),
            length.out = 30)
      )) %>% 
  select(dvp_5km, 1:4)

pred.psi_dvp_5km


## Management ----
# Ever burned
pred.psi_ever_burned <-
  predict(
    best_full, type = 'state', appendData = T, 
    newdata = data.frame(
      field_type = defaults$field_type, shrub_mean = defaults$shrub_mean,
      clin_max = defaults$clin_max, grs_500m = defaults$grs_500m,
      grs_1km = defaults$grs_1km, grs_5km = defaults$grs_5km,
      for_500m = defaults$for_500m, for_1km = defaults$for_1km,
      for_5km = defaults$for_5km, dvp_5km = defaults$dvp_5km,
      dvp_500m = defaults$dvp_500m, dvp_1km = defaults$dvp_1km,
      park = defaults$park,leased = defaults$leased, 
      year = defaults$year, year_fct = defaults$year_fct,
      ever_burned = 
        factor(levels(siteyear_covs$ever_burned), 
               levels = levels(siteyear_covs$ever_burned))
      )) %>% 
  select(ever_burned, 1:4)

pred.psi_ever_burned

# Leased
pred.psi_leased <-
  predict(
    best_full, type = 'state', appendData = T, 
    newdata = data.frame(
      field_type = defaults$field_type, shrub_mean = defaults$shrub_mean,
      clin_max = defaults$clin_max, grs_500m = defaults$grs_500m,
      grs_1km = defaults$grs_1km, grs_5km = defaults$grs_5km,
      for_500m = defaults$for_500m, for_1km = defaults$for_1km,
      for_5km = defaults$for_5km, dvp_5km = defaults$dvp_5km,
      dvp_500m = defaults$dvp_500m, dvp_1km = defaults$dvp_1km,
      ever_burned = defaults$ever_burned, park = defaults$park, 
      year = defaults$year, year_fct = defaults$year_fct,
      leased = factor(levels(siteyear_covs$leased), 
               levels = levels(siteyear_covs$leased))
      )) %>% 
  select(leased, 1:4)

pred.psi_leased

# Park
pred.psi_park <-
   predict(
    best_full, type = 'state', appendData = T, 
    newdata = data.frame(
      field_type = defaults$field_type, shrub_mean = defaults$shrub_mean,
      clin_max = defaults$clin_max, grs_500m = defaults$grs_500m,
      grs_1km = defaults$grs_1km, grs_5km = defaults$grs_5km,
      for_500m = defaults$for_500m, for_1km = defaults$for_1km,
      for_5km = defaults$for_5km, dvp_5km = defaults$dvp_5km,
      dvp_500m = defaults$dvp_500m, dvp_1km = defaults$dvp_1km,
      ever_burned = defaults$ever_burned, leased = defaults$leased, 
      year = defaults$year, year_fct = defaults$year_fct,
      park = factor(levels(siteyear_covs$park),
             levels = levels(siteyear_covs$park))
      )) %>% 
  select(park, 1:4)

pred.psi_park

### Submodels ----

pred.psi_burnclass <-
  predict(
    mgmt_subset_models$burnclass_p.null,
    type = 'state', appendData = T,
    newdata = data.frame(
      burn_class2 = 
        factor(levels(siteyear_covs$burn_class2),
               levels = levels(siteyear_covs$burn_class2))))

pred.psi_burnclass

pred.psi_limit <-
  predict(
    mgmt_subset_models$limit,
    type = 'state', appendData = T,
    newdata = data.frame(
      harvest_limit =
        factor(levels(siteyear_covs$harvest_limit),
               levels = levels(siteyear_covs$harvest_limit))))

pred.psi_limit

## Trends ----
### Year 
pred.psi_year <-
   predict(
    best_full, type = 'state', appendData = T, 
    newdata = data.frame(
      field_type = defaults$field_type, shrub_mean = defaults$shrub_mean,
      clin_max = defaults$clin_max, grs_500m = defaults$grs_500m,
      grs_1km = defaults$grs_1km, grs_5km = defaults$grs_5km,
      for_500m = defaults$for_500m, for_1km = defaults$for_1km,
      for_5km = defaults$for_5km, dvp_5km = defaults$dvp_5km,
      dvp_500m = defaults$dvp_500m, dvp_1km = defaults$dvp_1km,
      ever_burned = defaults$ever_burned, park = defaults$park,
      leased = defaults$leased, year_fct = defaults$year_fct,
      year = c(1:6, 8)
      ))  %>% 
  select(year, 1:4) %>% 
  mutate(year = c(2014:2019, 2021)) 

pred.psi_year

### Year (factor) 
pred.psi_yearf <-
   predict(
    best_full, type = 'state', appendData = T, 
    newdata = data.frame(
      field_type = defaults$field_type, shrub_mean = defaults$shrub_mean,
      clin_max = defaults$clin_max, grs_500m = defaults$grs_500m,
      grs_1km = defaults$grs_1km, grs_5km = defaults$grs_5km,
      for_500m = defaults$for_500m, for_1km = defaults$for_1km,
      for_5km = defaults$for_5km, dvp_5km = defaults$dvp_5km,
      dvp_500m = defaults$dvp_500m, dvp_1km = defaults$dvp_1km,
      ever_burned = defaults$ever_burned, park = defaults$park,
      leased = defaults$leased, 
      year_fct = factor(levels(defaults$year_fct), levels = levels(defaults$year_fct)),
      year = defaults$year
      ))  %>% 
  select(year, 1:4) %>% 
  rename(yearf = year) %>% 
  mutate(yearf = c(2014:2019, 2021)) 

pred.psi_yearf


# Occupancy betas (full model) --------------------------------------------

source('scripts/wrangling/tidy_method_unmarked.R')
# library(broom) # Needed? Unclear
best_full_tbl <- tidy(best_full)


# Save all models ---------------------------------------------------------

models <-
  c('m.0',
    'm.psi0',
    'det_models', 
    'trend_models', 
    'hab_models', 
    'land_models', 
    'mgmt_models', 
    'mgmt_subset_models',
    'combo_models',
    'best_full')

write_rds(
  mget(models), 
  paste0('output/all_fitted_models_', sp, '.rds'))

write_rds(
  best_full_tbl, 
  paste0('output/best_model_', sp, '.rds'))


# Save all predictions ----------------------------------------------------

bind_rows(
  p.pred =
    mget(ls(pattern = 'pred.p_')) %>% 
      bind_rows(.id = 'model') %>% 
      as_tibble(),
  psi.pred =
    mget(ls(pattern = 'pred.psi_')) %>% 
      bind_rows(.id = 'model') %>% 
      as_tibble()) %>% 
    mutate(species = sp,
           common_name = sp_long) %>% 
  
  write_rds(paste0('output/predicted_', sp, '.rds'))
  

