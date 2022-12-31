# BBS trends graph


# setup -------------------------------------------------------------------

library(tidyverse)


# data import -------------------------------------------------------------

grass_species <-
  read_csv('data/processed/all_grassland_species.csv')

bbs <- 
  read_csv('data/raw/bbs_summary.csv') %>% 
    select(region = `Region Name`,
           common_name = `Species Name`,
           trend = Trend,
           ci_lower = `2.5%CI`,
           ci_upper = `97.5%CI`) %>% 
    filter(region %in% c('Maryland', 
                          'Virginia', 
                          'West Virginia', 
                          'Appalachian Mountains', 
                          'Piedmont', 
                          'Survey-wide'),
           common_name %in% grass_species$common_name)


# make graph --------------------------------------------------------------



bbs %>% 
  filter(
    common_name %in% c('Grasshopper Sparrow', 
                       'Eastern Meadowlark', 
                       'Field Sparrow', 
                       'Red-winged Blackbird')) %>% 
  ggplot(
    aes(x = region,
        fill = common_name,
        y = trend)) +
  
  geom_col() +
  geom_errorbar(aes(ymin = ci_lower, 
                    ymax = ci_upper, 
                    width = 0.5)) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(
    limits = c('Survey-wide', 'Appalachian Mountains', 'Piedmont', 'Virginia', 'West Virginia', 'Maryland'),
    labels = c('All', 'Appalachian\nMountains', 'Piedmont', 'Virginia', 'West\nVirginia', 'Maryland')) +
  labs(x = 'Breeding Bird Survey region', 
       y = 'Annual % change\n(1966-2019) ± 95% CI', 
       title = 'Grassland species are in decline',
       caption = 
       'Data from Sauer, J.R., Link, W.A., and Hines, J.E., 2020, The North\n
       American Breeding Bird Survey, Analysis Results 1966 - 2019:\n
       U.S. Geological Survey data release, https://doi.org/10.5066/P96A7675.') +
  facet_wrap(~common_name) +
  theme_minimal() +
  theme(
    title = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    legend.position = 'none',
    axis.text.x = element_text(size = 8),
    plot.caption = element_text(size = 7, lineheight = 0.6, face = 'italic'))

ggsave('output/plots/bbs_focalspp_trends.png', width = 6, height = 4, units = "in")
  