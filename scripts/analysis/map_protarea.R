# Map protected areas and point count locations

# Setup -------------------------------------------------------------------

library(tidyverse)
library(tigris)
library(sf)
library(tmap)
theme_set(theme_bw())

# Download of shapefile: Keep download and cache to speed things up.
options(tigris_use_cache = TRUE, keep_zipped_shapefile = TRUE)

# Import data -------------------------------------------------------------

# Protected areas database
pl <-
  st_read("data/processed/prot_areas.shp")

# Data points surveyed
pts <-
  read_rds("data/processed/ch2_combined_birds.rds")$points

properties <-
  pts %>% 
  select(point_id, property_id) %>% 
  group_by(property_id) %>% 
  summarize(st_union(geometry)) %>% 
  st_centroid()

states <- # NAD83
  states(cb = TRUE, resolution = '5m') %>% 
  filter(NAME %in% c("Virginia", "West Virginia", "Maryland", "District of Columbia")) %>% 
  st_transform(crs = st_crs(properties))

# In which lands were surveys located -------------------------------------

pts <-
  st_join(pts, pl)

# Who manages? Who owns?
pts %>% 
    st_drop_geometry() %>% 
    group_by(d_Own_Type, d_Mang_Typ) %>% 
    count() %>% 
  ggplot(aes(x = reorder(d_Own_Type, -n), y = n, fill = d_Mang_Typ)) + 
  scale_fill_brewer(palette = "Set2", na.value = "gray") +
  scale_x_discrete(
    labels = c("Federal", "Private\n(Easement)", "State", "NGO", "Local\ngovernment", "Unprotected")) +
  labs(x = "Owner", fill = "Manager", y = "Points") +
  geom_col() +
  theme(axis.text.x = element_text(size = 8))

# Category new: PRIVATE EASEMENT, PUBLIC LAND (NGO + STATE + LOCAL + FED), PRIVATE LAND?

# Interactive maps --------------------------------------------------------

tmap_mode("view")

tm_basemap(server = "OpenStreetMap") +
tm_shape(pl) + 
  tm_polygons(
    "d_Mang_Typ",
    popup.vars = c(
      "Owner type" = "d_Own_Type",
      "Manager type" = "d_Mang_Typ",
      "Manager" = "d_Mang_Nam",
      "Location name" = "Unit_Nm",
      "Public access" = "d_Pub_Acce",
      "Use" = "d_GAP_Sts"),
    alpha = 0.5) +
tm_shape(pts) +
  tm_dots(
    popup.vars = c(
      "Data source" = "source",
      "Property ID" = "property_id"))


# Map for chapter ---------------------------------------------------------

bbox <-
  st_buffer(properties, dist = 8000) %>% 
  st_bbox()

ggplot() +
  geom_sf(
    data = pl,
    aes(fill = owner, color = owner),
    size = 0.2) +
  scale_fill_discrete(type = c("#ACCCE4", "#B8DD81", "gray90")) +
  scale_color_discrete(type = c("#ACCCE4", "#B8DD81", "gray90")) +
# US state borders
  geom_sf(
    data = states, 
    size = 0.5, 
    fill = NA,
    color = 'gray50') +
# properties
  geom_sf(
    data = properties,
    color = "black",
    shape = 1,
    stroke = 0.9,
    size = 2.5) +
  coord_sf(
    xlim = c(bbox$xmin, bbox$xmax),
    ylim = c(bbox$ymin, bbox$ymax)
  ) +
  # Edit legend
  labs(
    fill = "Protected areas",
    color = "Protected areas") +
  theme(
    panel.grid = element_blank(),
    legend.position = "top")

ggsave(
  "output/plots/maps/protarea_properties.png", 
  width = 6,
  height = 6,
  units = "in",
  dpi = 300)

# Static maps -------------------------------------------------------------

pl_own_mng <-
  pl %>% 
    mutate(
      owner = case_when(
        !d_Own_Type %in% unique(pts$d_Own_Type) ~ "Other",
        is.na(d_Own_Type) ~ "Unprotected",
        TRUE ~ d_Own_Type) %>% 
        factor(levels = c(
          pts$d_Own_Type %>% unique() %>% sort()
        )),
      manager = case_when(
        !d_Mang_Typ %in% unique(pts$d_Mang_Typ) ~ "Other",
        is.na(d_Mang_Typ) ~ "Other/Unknown",
        TRUE ~ d_Mang_Typ) %>% 
        factor(levels = c(
          "Federal", "Local Government", "Non-Governmental Organization",
          "Other", "Private", "State", "Regional Agency Special District"))
      ) %>% 
  select(owner, manager, geometry)

# This took 52 minutes. Never run it again

# gc()
# 
# properties <- unique(pts$property_id)
# 
# start <- Sys.time()
# 
# for(x in properties) {
# 
# zoom <-
#   pts %>%
#   filter(property_id == x) %>%
#   st_buffer(dist = 500) %>%
#   st_bbox()
# 
# plot_data <-
#   st_crop(pl_own_mng, zoom)
# 
# p <-
#   ggplot() +
#     geom_sf(
#       data = plot_data,
#       aes(fill = owner, color = manager),
#       size = 1,
#       alpha = 0.75) +
#     scale_fill_brewer(palette = "Set1", drop = F) +
#     scale_color_brewer(palette = "Set1", drop = F) +
#     geom_sf(data = pts) +
#     coord_sf(
#       xlim = c(zoom$xmin, zoom$xmax),
#       ylim = c(zoom$ymin, zoom$ymax),
#       expand = F) +
#     labs(
#       fill = "Landowner",
#       color = "Manager",
#       title = paste0(
#         "Protected areas: ", 
#         filter(pts, property_id == x)$source[1], 
#         " property ", 
#         x)) +
#   guides(
#     fill = guide_legend(override.aes = list(size = 0.5)),
#     color = guide_legend(override.aes = list(size = 0.5))) +
#   theme(
#     legend.text = element_text(size = 8),
#     legend.key.size = unit(1, "line")) 
# 
# ggsave(
#   filename = paste0("output/plots/maps/pad_", x, ".png"),
#   plot = p,
#   width = 6,
#   height = 4,
#   units = "in")
# 
# print(paste0("Completed: ", 
#         which(properties == x), 
#         " out of ", 
#         length(properties),
#         " (",
#         round(100 * which(properties == x)/length(properties), 1),
#         "%)"))
# 
# }
# end<-Sys.time()
# end-start
