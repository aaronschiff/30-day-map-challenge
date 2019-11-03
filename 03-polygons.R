# #30DayMapChallenge

# By @aschiff

# Challenge no: 03 -- Polygons

# Data sources -
# NZ building outlines
# https://data.linz.govt.nz/layer/101290-nz-building-outlines/


# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(glue)
library(ggthemes)

conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data -------------------------------------------------------------------

dat1 <- read_sf(here("data/nz-building-outlines/nz-building-outlines-1.shp"))
dat2 <- read_sf(here("data/nz-building-outlines/nz-building-outlines-2.shp"))

dat <- rbind(dat1, dat2)

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

# Set up bounding box
bbox_size <- 1600   # Metres

bbox <- st_point(x = c(174.763778, -36.850875), dim = "XY") %>%
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
map <- ggplot() + 
  geom_sf(data = map_dat,
          fill = "black", 
          size = 0) +
  theme_void() + 
  theme(plot.background = element_rect(fill = grey(0.75)), 
        plot.margin = margin(0, 0, 0, 0)) + 
  scale_x_continuous(expand = expand_scale(0, 0)) + 
  scale_y_continuous(expand = expand_scale(0, 0))

ggsave(filename = here("outputs/03-polygons.png"), 
       plot = map, 
       device = "png", 
       width = 20, 
       height = 20, 
       units = "cm")


# *****************************************************************************
