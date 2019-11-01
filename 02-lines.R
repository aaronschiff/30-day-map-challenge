# #30DayMapChallenge

# By @aschiff

# Challenge no: 02 -- lines

# Data sources:
# NZ Contours, 1:50k
# https://data.linz.govt.nz/layer/50768-nz-contours-topo-150k/


# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(sf)
library(ggthemes)

conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data -------------------------------------------------------------------

dat1 <- read_sf(here("data/nz-contours-topo-150k/nz-contours-topo-150k-1.shp"))
dat2 <- read_sf(here("data/nz-contours-topo-150k/nz-contours-topo-150k-2.shp"))
dat3 <- read_sf(here("data/nz-contours-topo-150k/nz-contours-topo-150k-3.shp"))

dat <- rbind(dat1, dat2, dat3)

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

# Set up bounding box
bbox_size <- 15000   # Metres

bbox <- st_point(x = c(174.063185, -39.295573), dim = "XY") %>%
  st_sfc(crs = 4326) %>%
  st_as_sf() %>%
  st_transform(crs = 2193) %>%
  st_bbox()

bbox["xmin"] <- bbox["xmin"] - bbox_size
bbox["xmax"] <- bbox["xmax"] + bbox_size
bbox["ymin"] <- bbox["ymin"] - bbox_size
bbox["ymax"] <- bbox["ymax"] + bbox_size

# Map data
map_dat <- st_crop(x = dat, y = bbox)

# Make map
map <- ggplot(map_dat) + 
  geom_sf(size = 0.15, colour = rgb(1, 1, 1, 1)) + 
  theme_void() + 
  theme(plot.background = element_rect(fill = "black"), 
        plot.margin = margin(0, 0, 0, 0)) + 
  scale_x_continuous(expand = expand_scale(0, 0)) + 
  scale_y_continuous(expand = expand_scale(0, 0))

ggsave(filename = here("outputs/02-lines.png"), 
       plot = map, 
       device = "png", 
       width = 20, 
       height = 20, 
       units = "cm")

# *****************************************************************************
