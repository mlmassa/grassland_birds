# Map protected areas and point count locations

# Setup -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(tmap)
theme_set(theme_bw())

# Import data -------------------------------------------------------------

# Protected areas database
pl <-
  st_read("data/raw/PADUS3_0Combined_Region1.shp") %>% 
  filter(State_Nm %in% c("VA", "MD", "WV")) %>% 
  st_make_valid() %>% 
  st_transform(crs = 4326)

# Data points surveyed
pts <-
  bind_rows(
    VWL = 
      read_rds("data/processed/vwl_birds.rds")$points %>% 
      mutate(property_id = as.character(property_id)),
    NPS = 
      read_rds("data/processed/birds.rds")$points %>% 
      rename(property_id = park, point_id = point_name),
    .id = "source") %>% 
  filter(!is.na(long), !is.na(lat)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

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
