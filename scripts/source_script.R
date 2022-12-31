
# Functions ---------------------------------------------------------------


# Plot themes -------------------------------------------------------------

# BrianEvans theme
plot_theme_evans <-
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size=16),
        axis.text = element_text(size=12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.background = element_rect(fill = 'white', color = 'black'),
        panel.grid = element_line(color="#d9d9d9"),
        panel.grid.minor = element_line(linetype = 'dashed'),
        axis.line = element_line(color = 'black'))
