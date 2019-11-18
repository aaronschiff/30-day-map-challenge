# #30DayMapChallenge

# By @aschiff

# Challenge no: 19 -- Urban

# Data sources:
# Census 2018 data on NZ.Stat
# Statistical Area 2 boundaries clipped
# https://datafinder.stats.govt.nz/layer/92213-statistical-area-2-2018-clipped-generalised/


# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(glue)
library(readxl)

conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data -------------------------------------------------------------------

# Load population data from NZ.Stat horrible Excel file
pop_dat <- read_excel(path = here("data/census-2018-pop-auckland-sa2.xlsx"), 
                      skip = 7, 
                      col_names = c("area", "junk", "2006", "2013", "2018")) %>%
  select(-junk) %>%
  filter(!is.na(`2018`)) %>%
  pivot_longer(names_to = "year", values_to = "pop", -area) %>%
  mutate(year = as.integer(year))

# Read SA2 shapes
sa2_dat <- read_sf(here("data/statsnzstatistical-area-2-2018-clipped-generalised-SHP/statistical-area-2-2018-clipped-generalised.shp")) %>%
  clean_names() %>%
  select(sa22018_v1, sa22018_1) %>%
  mutate(landarea = as.numeric(st_area(.)))

# Join pop data to SA2 shapes and calculate change in density
pop_density_changes_sa2 <- pop_dat %>%
  filter(year != 2013) %>%
  pivot_wider(values_from = pop, names_from = year, names_prefix = "pop") %>%
  left_join(sa2_dat, by = c("area" = "sa22018_1")) %>%
  filter(!is.na(sa22018_v1)) %>%
  mutate(density2006 = pop2006 / (landarea / 10000), 
         density2018 = pop2018 / (landarea / 10000)) %>%
  mutate(density_change = density2018 - density2006) %>%
  st_as_sf()

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

bbox_width <- 25000
bbox_height <- 30000

bbox <- st_point(x = c(174.767603, -36.844212), dim = "XY") %>%
  st_sfc(crs = 4326) %>%
  st_as_sf() %>%
  st_transform(crs = 2193) %>%
  st_bbox()

bbox["xmin"] <- bbox["xmin"] - bbox_width
bbox["xmax"] <- bbox["xmax"] + bbox_width
bbox["ymin"] <- bbox["ymin"] - bbox_height
bbox["ymax"] <- bbox["ymax"] + bbox_height

map_dat <- st_crop(x = pop_density_changes_sa2, y = bbox)

map <- map_dat %>%
  ggplot() + 
  geom_sf(mapping = aes(fill = density_change, 
                        colour = density_change), 
          size = 0.1) + 
  scale_colour_gradient(low = "white", 
                        high = "#08306b", 
                        aesthetics = c("colour", "fill"), 
                        name = "Change in population density\n2018 vs 2006\n(people per 10,000 sqm)") + 
  scale_x_continuous(expand = expand_scale(0, 0), 
                     limits = c(bbox["xmin"], bbox["xmax"])) + 
  scale_y_continuous(expand = expand_scale(0, 0), 
                     limits = c(bbox["ymin"], bbox["ymax"])) + 
  theme_void() +
  theme(plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = grey(0.5), 
                                        linetype = "blank"), 
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
        legend.title = element_text(size = rel(1), face = "bold"), 
        legend.text = element_text(size = rel(1), face = "bold")) 

ggsave(filename = here("outputs/19-urban.png"), 
       plot = map, 
       device = "png", 
       width = 25, 
       height = 25, 
       units = "cm")

# *****************************************************************************