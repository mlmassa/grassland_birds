# This script generates a correlation matrix and
# examines covariate reduction potentials.

# setup -------------------------------------------------------------------

library(tidyverse)
library(sf)

# Import data
annual_covs <- 
  read_rds('data/processed/annual_covs.rds')

static_covs <-
  read_rds('data/processed/static_covs.rds') 

read_rds('data/processed/birds.rds') %>% 
  list2env(.GlobalEnv)


# Hist of all -------------------------------------------------------------

static_covs %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(
    cols = c(2:length(.)),
    names_to = 'variable') %>% 
  left_join(static_covs %>% select(grts, park)) %>% 

  
ggplot(
  aes(x = value,
      color = park)) +
  geom_density() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = 'Value', 
       y = 'Density', 
       title = 'Density plot of habitat covariates by park',
       color = 'Park') +
  facet_wrap(~variable, ncol = 3, scales = 'free') +
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = 'white'),
        panel.grid = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.ticks.y = element_blank(),
        strip.background = element_blank()) 


# Export plot
ggsave(
  filename = 'cov_hists_park.png',
    path = 'output/plots',
    width = 6,
    height = 8,
    units = 'in')

  

# Correlation matrix ------------------------------------------------------

# Create function to mod matrix to triangle
get_lower_tri <-
  function(x) {
    x[upper.tri(x)] <- NA
    return(x)
  }

# Run correlation
corr_table <- 
  static_covs %>% 
  select(order(colnames(static_covs))) %>% 
  select(where(is.numeric)) %>% 
  select(-c(grts, ever_burned, harvest_limit, leased)) %>% 
  
  # Run correlation
  cor(
    method = 'spearman',
    use = 'pairwise.complete.obs') %>% 
  round(digits = 2) %>% 
  
  # Get only one triangle
  get_lower_tri() %>% 
  as_tibble(rownames = 'var1') %>% 
  
  # Transform to long tibble with var1, var2, value
  pivot_longer(
    2:length(.),
    names_to = 'var2') %>% 
  filter(!is.na(value))


# Show most highly correlated variables
corr_table %>% 
  filter(value != 1,
         abs(value) >= 0.6) %>% 
  arrange(-abs(value)) %>% 
  print(n = nrow(.))

# Basic triangle plot
plot <-
  corr_table %>% 
  ggplot(
    aes(x = var1, y = var2, fill = value)) +
  geom_tile(
    color = 'white') +
  scale_fill_gradient2(
    low = '#0e69c9',
    high = '#ff6600',
    mid = 'white',
    midpoint = 0,
    limit = c(-1,1),
    space = 'Lab',
    name = 'Spearman\ncorrelation') +
  scale_y_discrete(position = "right") +
  theme_minimal() +
  theme(
    axis.text.x = 
      element_text(angle = 90, vjust = 0.5, hjust = 1, size = 11),
    axis.text.y =
      element_text(size = 11)) +
  coord_fixed()


# Labeled triangle plot
labeled_corr_plot <- 
  plot +  
    geom_text(
      aes(var1, var2, label = value), color = 'black', size = 2) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal") +
    guides(
      fill = 
        guide_colorbar(
          barwidth = 7, 
          barheight = 1,
          title.position = "top", 
          title.hjust = 0.5))

labeled_corr_plot

# Explort plot
ggsave(
  filename = 'corr_matrix_update.png',
    path = 'output/plots',
    width = 7.5,
    height = 5,
    units = 'in')

# Random other plots ------------------------------------------------------
static_covs %>% 
  ggplot(aes(x = for_500, y = crp_1000)) +
  geom_point()