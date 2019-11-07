# #30DayMapChallenge

# By @aschiff

# Challenge no: 08 -- Green

# Data sources:
# MFE LUCAS NZ Land Use Map
# https://data.mfe.govt.nz/layer/52375-lucas-nz-land-use-map-1990-2008-2012-2016-v006/ 


# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(glue)
library(rmapshaper)
library(parallel)

conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data -------------------------------------------------------------------

dat <- read_sf(here("data/mfe-lucas-nz-land-use-map-1990-2008-2012-2016-v006-SHP/lucas-nz-land-use-map-1990-2008-2012-2016-v006.shp")) %>%
  clean_names()

coastline <- read_sf(here("data/lds-nz-coastlines-and-islands-polygons-topo-150k-SHP/nz-coastlines-and-islands-polygons-topo-150k.shp"))

grass_ids <- c(74, 75, 76)  # lucid of grassland areas

grass_2016 <- dat %>%
  filter(lucid_2016 %in% grass_ids) %>%
  mutate(year = 2016L) %>%
  select(year)

grass_2012 <- dat %>%
  filter(lucid_2012 %in% grass_ids) %>%
  mutate(year = 2012L) %>%
  select(year)

grass_2008 <- dat %>%
  filter(lucid_2008 %in% grass_ids) %>%
  mutate(year = 2008L) %>%
  select(year)

grass_1990 <- dat %>%
  filter(lucid_1990 %in% grass_ids) %>%
  mutate(year = 1990L) %>%
  select(year)

grass <- rbind(grass_2016, grass_2012, grass_2008, grass_1990)

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

map <- ggplot() + 
  geom_sf(data = grass, 
          fill = "forestgreen", 
          size = 0) + 
  facet_wrap(facets = vars(year), ncol = 2) + 
  theme_void() + 
  theme(plot.margin = margin(1, 0, 0, 0, "cm"), 
        panel.spacing.x = unit(1, "cm"), 
        panel.spacing.y = unit(1, "cm"))

ggsave(filename = here("outputs/08-green.png"), 
       plot = map, 
       device = "png", 
       width = 20, 
       height = 20, 
       units = "cm")

# *****************************************************************************
