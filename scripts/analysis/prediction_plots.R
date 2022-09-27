library(tidyverse)
library(unmarked)
source('scripts/wrangling/tidy_method_unmarked.R')
# Set parameters ----------------------------------------------------------

focal_sp <- c('GRSP', 'EAME')

# Set theme
theme_set(
  theme_bw() +
  theme(
    strip.text = element_text(size = 11),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 10),
    strip.background = element_blank()))


# Import data -------------------------------------------------------------
pred <-
  dir('./output', pattern = 'predicted_*') %>% 
  map_dfr(~ read_rds(file.path('./output', .)))

pred <-
  pred %>% 
  filter(species %in% focal_sp)


# Detection ---------------------------------------------------------------


## Wind ----
ggplot(
  data = pred %>% filter(model == 'pred.p_wind', wind != 5), 
  aes(x = wind, y = Predicted)) +
  geom_bar(
    fill = 'gray60',
    stat = 'identity') +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.25) +
  labs(
    x = 'Wind (Beaufort scale)',
    y = 'Predicted detection (95% CI)') +
  scale_y_continuous(
  expand = c(0, 0),
  limits = c(0, 1)) +
  facet_wrap(vars(common_name), ncol = 2) +
  theme(panel.grid = element_blank())

ggsave(paste0('output/plots/plot_detection_wind.pdf'), 
       width = 6.5, height = 4, units = 'in')
ggsave(paste0('output/plots/plot_detection_wind.png'), 
       width = 6.5, height = 4, units = 'in')

## Disturbance ----
ggplot(
  data = pred %>% filter(model == 'pred.p_dist'), 
  aes(x = disturbance, y = Predicted)) +
  geom_bar(
    fill = 'gray60',
    stat = 'identity') +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.25) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1)) +
  labs(
    x = 'Disturbance',
    y = 'Predicted detection (95% CI)') +
  facet_wrap(vars(common_name), ncol = 2) +
  theme(panel.grid = element_blank())

ggsave(paste0('output/plots/plot_detection_dist.pdf'), 
       width = 6.5, height = 4, units = 'in')
ggsave(paste0('output/plots/plot_detection_dist.png'), 
       width = 6.5, height = 4, units = 'in')

## Doy ----
# pred %>%
#   filter(model == 'pred.p_doy') %>%
#   ggplot(aes(x = doy, y = Predicted,
#              ymin = lower, ymax = upper)) +
#   geom_line() +
#   geom_line(aes(y = lower), color = 'gray60', linetype = 2) +
#   geom_line(aes(y = upper), color = 'gray60', linetype = 2) +
#   labs(y = 'Predicted occupancy (95% CI)',
#        x = 'Day of year') +
#   facet_grid(cols = vars(common_name), ncol = 2) +
#   scale_y_continuous(
#     expand = c(0, 0),
#     limits = c(0, 1))
# 
# ggsave(paste0('output/plots/plot_detection_doy.pdf'), 
#        width = 6.5, height = 4, units = 'in')
# ggsave(paste0('output/plots/plot_detection_doy.png'), 
#        width = 6.5, height = 4, units = 'in')



## Temperature ----
# pred %>%
#   filter(model == 'pred.p_temp') %>%
#   ggplot(aes(x = temperature, y = Predicted,
#              ymin = lower, ymax = upper)) +
#   geom_line() +
#   geom_line(aes(y = lower), color = 'gray60', linetype = 2) +
#   geom_line(aes(y = upper), color = 'gray60', linetype = 2) +
#   labs(y = 'Predicted detection (95% CI)',
#        x = 'Temperature') +
#   facet_grid(cols = vars(common_name), ncol = 2) +
#   scale_y_continuous(
#     expand = c(0, 0),
#     limits = c(0, 1))
# 
# ggsave(paste0('output/plots/plot_detection_temp.pdf'), 
#        width = 6.5, height = 4, units = 'in')
# ggsave(paste0('output/plots/plot_detection_temp.png'), 
#        width = 6.5, height = 4, units = 'in')

## Time ----
# pred %>%
#   filter(model == 'pred.p_time') %>%
#   ggplot(aes(x = time, y = Predicted,
#              ymin = lower, ymax = upper)) +
#   geom_line() +
#   geom_line(aes(y = lower), color = 'gray60', linetype = 2) +
#   geom_line(aes(y = upper), color = 'gray60', linetype = 2) +
#   labs(y = 'Predicted detection (95% CI)',
#        x = 'Time of day') +
#   facet_grid(cols = vars(common_name), ncol = 2) +
#   scale_y_continuous(
#     expand = c(0, 0),
#     limits = c(0, 1))
# 
# ggsave(paste0('output/plots/plot_detection_time.pdf'), 
#        width = 6.5, height = 4, units = 'in')
# ggsave(paste0('output/plots/plot_detection_time.png'), 
#        width = 6.5, height = 4, units = 'in')


# Occupancy ---------------------------------------------------------------


## H: Field type ----
pred %>% 
  filter(model == 'pred.psi_field') %>% 
  ggplot(aes(x = field_type, y = Predicted, 
             ymin = lower, ymax = upper)) +
  geom_bar(fill = 'gray60', stat = 'identity') +
  geom_errorbar(width = 0.3) +
  labs(y = 'Predicted occupancy (95% CI)',
       x = 'Field type') +
  facet_grid(cols = vars(common_name)) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1)) +
  theme(panel.grid = element_blank())

ggsave(paste0('output/plots/pred_psi_fieldtype.pdf'), 
       width = 6.5, height = 2, units = 'in')
ggsave(paste0('output/plots/pred_psi_fieldtype.png'), 
       width = 6.5, height = 2, units = 'in')

## H: Clin ----
pred %>% 
  filter(model == 'pred.psi_clin') %>% 
  ggplot(aes(x = clin_max, y = Predicted, 
             ymin = lower, ymax = upper)) +
  geom_line() +
  geom_line(aes(y = lower), color = 'gray60', linetype = 2) +
  geom_line(aes(y = upper), color = 'gray60', linetype = 2) +
  labs(y = 'Predicted occupancy (95% CI)',
       x = 'Max. angle to horizon') +
  facet_grid(cols = vars(common_name)) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1))

ggsave(paste0('output/plots/pred_psi_clin.pdf'), 
       width = 6.5, height = 2, units = 'in')
ggsave(paste0('output/plots/pred_psi_clin.png'), 
       width = 6.5, height = 2, units = 'in')

## H: Shrub ----
pred %>% 
  filter(model == 'pred.psi_shrub', 
         species %in% c('EAME', 'GRSP')) %>% 
  ggplot(aes(x = shrub_mean, y = Predicted, 
             ymin = lower, ymax = upper)) +
  geom_line() +
  geom_line(aes(y = lower), color = 'gray60', linetype = 2) +
  geom_line(aes(y = upper), color = 'gray60', linetype = 2) +
  labs(y = 'Predicted occupancy (95% CI)',
       x = 'Shrub cover within 100m') +
  facet_grid(cols = vars(common_name)) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1))

ggsave(paste0('output/plots/pred_psi_shrub.pdf'), 
       width = 6.5, height = 2, units = 'in')
ggsave(paste0('output/plots/pred_psi_shrub.png'), 
       width = 6.5, height = 2, units = 'in')


## L: Grass 500m ----
pred %>% 
  filter(model == 'pred.psi_grs_500m') %>% 
  ggplot(aes(x = grs_500m, y = Predicted, 
             ymin = lower, ymax = upper)) +
  geom_line() +
  geom_line(aes(y = lower), color = 'gray60', linetype = 2) +
  geom_line(aes(y = upper), color = 'gray60', linetype = 2) +
  labs(y = 'Predicted occupancy (95% CI)',
       x = 'Grassland (500m)') +
  facet_grid(cols = vars(common_name)) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1))

ggsave(paste0('output/plots/pred_psi_grs_500m.pdf'), 
       width = 6.5, height = 3, units = 'in')
ggsave(paste0('output/plots/pred_psi_grs_500m.png'), 
       width = 6.5, height = 3, units = 'in')

## L: For 500m ----
pred %>% 
  filter(model == 'pred.psi_for_500m') %>% 
  ggplot(aes(x = for_500m, y = Predicted, 
             ymin = lower, ymax = upper)) +
  geom_line() +
  geom_line(aes(y = lower), color = 'gray60', linetype = 2) +
  geom_line(aes(y = upper), color = 'gray60', linetype = 2) +
  labs(y = 'Predicted occupancy (95% CI)',
       x = 'Forest (500m)') +
  facet_grid(cols = vars(common_name)) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1))

ggsave(paste0('output/plots/pred_psi_for_500m.pdf'), 
       width = 6.5, height = 3, units = 'in')
ggsave(paste0('output/plots/pred_psi_for_500m.png'), 
       width = 6.5, height = 3, units = 'in')

## L: Dvp 500m ----
pred %>% 
  filter(model == 'pred.psi_dvp_500m') %>% 
  ggplot(aes(x = dvp_500m, y = Predicted, 
             ymin = lower, ymax = upper)) +
  geom_line() +
  geom_line(aes(y = lower), color = 'gray60', linetype = 2) +
  geom_line(aes(y = upper), color = 'gray60', linetype = 2) +
  labs(y = 'Predicted occupancy (95% CI)',
       x = 'Developed (500m)') +
  facet_grid(cols = vars(common_name)) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1))

ggsave(paste0('output/plots/pred_psi_dvp_500m.pdf'), 
       width = 6.5, height = 3, units = 'in')
ggsave(paste0('output/plots/pred_psi_dvp_500m.png'), 
       width = 6.5, height = 3, units = 'in')

## L: Dvp 5km ----
pred %>% 
  filter(model == 'pred.psi_dvp_5km') %>% 
  ggplot(aes(x = dvp_5km, y = Predicted, 
             ymin = lower, ymax = upper)) +
  geom_line() +
  geom_line(aes(y = lower), color = 'gray60', linetype = 2) +
  geom_line(aes(y = upper), color = 'gray60', linetype = 2) +
  labs(y = 'Predicted occupancy (95% CI)',
       x = 'Developed (5km)') +
  facet_grid(cols = vars(common_name)) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1))

ggsave(paste0('output/plots/pred_psi_dvp_5km.pdf'), 
       width = 6.5, height = 3, units = 'in')
ggsave(paste0('output/plots/pred_psi_dvp_5km.png'), 
       width = 6.5, height = 3, units = 'in')
## M: Lease ----
levels(pred$leased) <- c('Not leased', 'Leased')

pred %>% 
  filter(model == 'pred.psi_leased') %>% 
  ggplot(aes(x = leased, y = Predicted, 
             ymin = lower, ymax = upper)) +
  geom_bar(fill = 'gray60', stat = 'identity') +
  geom_errorbar(width = 0.3) +
  labs(y = 'Predicted occupancy (95% CI)',
       x = 'Agricultural lease') +
  facet_grid(cols = vars(common_name)) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1))  +
  theme(panel.grid = element_blank())

ggsave(paste0('output/plots/pred_psi_lease.pdf'), 
       width = 6.5, height = 3, units = 'in')
ggsave(paste0('output/plots/pred_psi_lease.png'), 
       width = 6.5, height = 3, units = 'in')

## M*: Limit ----
levels(pred$harvest_limit) <- c('Absent', 'Present')

pred %>% 
  filter(model == 'pred.psi_limit') %>% 
  ggplot(aes(x = harvest_limit, y = Predicted, 
             ymin = lower, ymax = upper)) +
  geom_bar(fill = 'gray60', stat = 'identity') +
  geom_errorbar(width = 0.3) +
  labs(y = 'Predicted occupancy (95% CI)',
       x = 'Harvest timing restriction') +
  facet_grid(cols = vars(common_name)) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1))  +
  theme(panel.grid = element_blank())

ggsave(paste0('output/plots/pred_psi_limit.pdf'), 
       width = 6.5, height = 3, units = 'in')
ggsave(paste0('output/plots/pred_psi_limit.png'), 
       width = 6.5, height = 3, units = 'in')

## M: Ever burned ----
# levels(pred$ever_burned) <- c('Absent', 'Present')
# pred %>% 
#   filter(model == 'pred.psi_limit') %>% 
#   ggplot(aes(x = ever_burned, y = Predicted, 
#              ymin = lower, ymax = upper)) +
#   geom_bar(fill = 'gray60', stat = 'identity') +
#   geom_errorbar(width = 0.3) +
#   labs(y = 'Predicted occupancy (95% CI)',
#        x = 'Prescribed fire') +
#   facet_grid(cols = vars(common_name)) +
#   scale_y_continuous(
#     expand = c(0, 0),
#     limits = c(0, 1))
# 
# ggsave(paste0('output/plots/pred_psi_everburned.pdf'), 
#        width = 6.5, height = 3, units = 'in')
# ggsave(paste0('output/plots/pred_psi_everburned.png'), 
#        width = 6.5, height = 3, units = 'in')

## M*: Years since last burn ----

pred %>%
  filter(model == 'pred.psi_burnclass') %>% 
  ggplot(aes(x = burn_class2, y = Predicted, 
             ymin = lower, ymax = upper)) +
  geom_bar(fill = 'gray60', stat = 'identity') +
  geom_errorbar(width = 0.3) +
  labs(y = 'Predicted occupancy (95% CI)',
       x = 'Time since last burn') +
  facet_grid(cols = vars(common_name)) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1))  +
  theme(panel.grid = element_blank())

ggsave(paste0('output/plots/pred_psi_burnclass.pdf'), 
       width = 6.5, height = 3, units = 'in')
ggsave(paste0('output/plots/pred_psi_burnclass.png'), 
       width = 6.5, height = 3, units = 'in')
## M: Park ----
pred %>% 
  filter(model == 'pred.psi_park') %>% 
  mutate(
    park_name = 
      case_when(park == 'ANTI' ~ 'Antietam',
                park == 'HAFE' ~ 'Harpers\nFerry',
                park == 'MANA' ~ 'Manassas',
                park == 'MONO' ~ 'Monocacy')) %>%
  ggplot(aes(x = park_name, y = Predicted, 
             ymin = lower, ymax = upper)) +
  geom_bar(fill = 'gray60', stat = 'identity') +
  geom_errorbar(width = 0.3) +
  labs(y = 'Predicted occupancy (95% CI)',
       x = 'Park') +
  facet_grid(cols = vars(common_name)) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1))  +
  theme(panel.grid = element_blank())

ggsave(paste0('output/plots/pred_psi_park.pdf'), 
       width = 6.5, height = 4, units = 'in')
ggsave(paste0('output/plots/pred_psi_park.png'), 
       width = 6.5, height = 4, units = 'in')

## Year ----
pred %>% 
  filter(model == 'pred.psi_year') %>% 
  ggplot(aes(x = year, y = Predicted, 
             ymin = lower, ymax = upper)) +
  geom_line() +
  geom_line(aes(y = lower), color = 'gray60', linetype = 2) +
  geom_line(aes(y = upper), color = 'gray60', linetype = 2) +
  labs(y = 'Predicted occupancy (95% CI)',
       x = 'Year') +
  facet_grid(cols = vars(common_name)) +
  scale_x_continuous(breaks = c(2014:2021)) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1))  +
  theme(panel.grid.minor.x = element_blank())

ggsave(paste0('output/plots/plot_occupancy_year.pdf'), 
       width = 6.5, height = 4, units = 'in')
ggsave(paste0('output/plots/plot_occupancy_year.png'), 
       width = 6.5, height = 4, units = 'in')

## Year(fct) ----
# pred %>% 
#   filter(model == 'pred.psi_yearf') %>% 
#   ggplot(aes(x = yearf, y = Predicted, 
#              ymin = lower, ymax = upper)) +
#   geom_line() +
#   geom_line(aes(y = lower), color = 'gray60', linetype = 2) +
#   geom_line(aes(y = upper), color = 'gray60', linetype = 2) +
#   labs(y = 'Predicted occupancy (95% CI)',
#        x = 'Year') +
#   facet_grid(cols = vars(common_name)) +
#   scale_y_continuous(
#     expand = c(0, 0),
#     limits = c(0, 1))
# 
# ggsave(paste0('output/plots/plot_occupancy_yearfct.pdf'), 
#        width = 6.5, height = 4, units = 'in')
# ggsave(paste0('output/plots/plot_occupancy_yearfct.png'), 
#        width = 6.5, height = 4, units = 'in')

## Betas ----
models_eame <-
  read_rds('output/all_fitted_models_EAME.rds')

models_grsp <-
  read_rds('output/all_fitted_models_GRSP.rds') 

betas <-
  rbind(
    tidy(models_grsp$best_full) %>% 
      mutate(species = 'GRSP', common_name = 'Grasshopper Sparrow'),
    tidy(models_eame$best_full) %>% 
      mutate(species = 'EAME', common_name = 'Eastern Meadowlark')) %>% 
    separate(term, into = c('submodel', 'term'), sep = ' ', 
             extra = 'merge') %>% 
    mutate(
      category = 
        case_when(str_detect(term, 'field|clin|shrub') ~ 'Habitat',
                  str_detect(term, '[0-9][km]') ~ 'Landscape',
                  str_detect(term, 'park|burn|lease') ~ 'Management',
                  !str_detect(submodel, 'psi') ~ 'Detection',
                  TRUE ~ 'Other'),
      submodel = str_remove_all(submodel, '\\[|\\]'),
      signif = 
        case_when(
          p.value > 0.05 ~ 'n.s.',
          0.05 >= p.value & p.value > 0.001 ~ 'p < 0.05',
          0.001 >= p.value ~ 'p < 0.001'),
      term = 
        str_replace_all(term, 'field_type', 'Field type: ') %>% 
        str_replace_all('clin_max', 'Max. angle to horizon') %>% 
        str_replace_all('shrub_mean', 'Shrub cover') %>% 
        str_replace_all('m$', 'm)') %>% 
        str_replace_all('leased1', 'Agricultural lease') %>% 
        str_replace_all('ever_burned1', 'Prescribed fire') %>% 
        str_replace_all('year', 'Year') %>% 
        str_replace_all('for_', 'Forest (') %>% 
        str_replace_all('dvp_', 'Developed (') %>% 
        str_replace_all('grs_', 'Grassland (') %>% 
        str_replace_all('park', 'Park: ') %>% 
        str_replace_all('HAFE', 'Harpers Ferry') %>% 
        str_replace_all('MANA', 'Manassas') %>% 
        str_replace_all('ANTI', 'Antietam') %>% 
        str_replace_all('MONO', ' Monocacy'))

betas %>% 
  filter(submodel == 'psi') %>%
  mutate(term = factor(term, levels = rev(sort(unique(term))))) %>% 
  ggplot(
    aes(x = term, y = estimate, ymin = estimate-std.error, 
        ymax = estimate + std.error)) +
    geom_hline(yintercept = 0, color = 'black', linetype = 'dashed')  +
  geom_pointrange(aes(color = category), fatten = 0.8) +
  scale_color_manual(
    values = c('Habitat' = '#2d6b2c', 
               'Landscape'= '#63acff', 
               'Management' = '#e3ab10')) +
  labs(x = '', y = 'Coefficient estimate (SE)', color = '') +
  coord_flip() +
  facet_wrap(vars(common_name), nrow = 2, scales = 'free') +
  theme(
    legend.justification = c(1,1),
    legend.position = c(1,1),
    legend.background = element_blank())


ggsave(paste0('output/plots/plot_occupancy_betas.pdf'), 
       width = 6.5, height = 5, units = 'in')
ggsave(paste0('output/plots/plot_occupancy_betas.png'), 
       width = 6.5, height = 5, units = 'in')
