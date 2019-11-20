# #30DayMapChallenge

# By @aschiff

# Challenge no: 20 -- Rural

# Data sources:
# Auckland Council Unitary Plan shapefiles
# http://hub.arcgis.com/datasets/4673e8f3f20942d7a21cfcb36971e103


# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(glue)

conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data -------------------------------------------------------------------

zones <- read_sf(here("data/UnitaryPlan_Shapefile/Shapefile/UP_BaseZone.shp")) %>%
  clean_names()

regions <- read_sf(here("data/statsnzregional-council-2019-clipped-generalised-SHP/regional-council-2019-clipped-generalised.shp")) %>%
  clean_names()

rural_zones <- zones %>%
  filter(groupzone_2 == "Rural")

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

bbox_width <- 55000
bbox_height <- 85000

bbox <- st_point(x = c(174.767603, -36.844212), dim = "XY") %>%
  st_sfc(crs = 4326) %>%
  st_as_sf() %>%
  st_transform(crs = 2193) %>%
  st_bbox()

bbox["xmin"] <- bbox["xmin"] - bbox_width
bbox["xmax"] <- bbox["xmax"] + bbox_width
bbox["ymin"] <- bbox["ymin"] - bbox_height
bbox["ymax"] <- bbox["ymax"] + bbox_height

akl_region <- regions %>% filter(regc2019_1 == "Auckland Region") %>%
  st_crop(bbox)

map_rural_zones <- st_crop(x = rural_zones, y = bbox)

map <- ggplot() + 
  geom_sf(data = akl_region, 
          fill = "white", 
          linetype = "blank") + 
  geom_sf(data = map_rural_zones,
          mapping = aes(fill = zone_descr,
                        colour = zone_descr),
          size = 0.1) +
  geom_sf(data = akl_region, 
          fill = NA, 
          colour = "black", 
          size = 0.2) + 
  scale_x_continuous(expand = expand_scale(0, 0), 
                     limits = c(bbox["xmin"], bbox["xmax"])) + 
  scale_y_continuous(expand = expand_scale(0, 0), 
                     limits = c(bbox["ymin"], bbox["ymax"])) + 
  scale_fill_manual(values = c("Rural - Countryside Living Zone" = "#B9BD00", 
                               "Rural - Mixed Rural Zone" = "#A4DE02", 
                               "Rural - Rural Coastal Zone" = "#76BA1B", 
                               "Rural - Rural Conservation Zone" = "#4C9A2A", 
                               "Rural - Rural Production Zone" = "#1E5631", 
                               "Rural - Waitakere Foothills Zone" = "#ACDF87", 
                               "Rural - Waitakere Ranges Zone" = "#68BB59"), 
                    aesthetics = c("colour", "fill"), 
                    name = NULL) + 
  theme_void() +
  theme(plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = grey(0.9), 
                                        linetype = "blank"), 
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
        legend.title = element_text(size = rel(1), face = "bold"), 
        legend.text = element_text(size = rel(1), face = "bold")) 

ggsave(filename = here("outputs/20-rural.png"), 
       plot = map, 
       device = "png", 
       width = 25, 
       height = 25, 
       units = "cm")

# *****************************************************************************
