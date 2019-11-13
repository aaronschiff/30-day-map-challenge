# #30DayMapChallenge

# By @aschiff

# Challenge no: 14 -- Boundaries

# Data sources:
# Auckland Council Unitary Plan shapefiles
# http://hub.arcgis.com/datasets/4673e8f3f20942d7a21cfcb36971e103
# LINZ 0.075m aerial photos
# https://data.linz.govt.nz/layer/95497-auckland-0075m-urban-aerial-photos-2017


# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(leaflet)
library(mapview)

conflict_prefer("filter", "dplyr")

lds_api_key <- ""   # LINZ Data Service API key

# *****************************************************************************


# *****************************************************************************
# Load data -------------------------------------------------------------------

rub <- read_sf(here("data/UnitaryPlan_Shapefile/Shapefile/UP_RuralUrbanBoundary.shp")) %>%
  clean_names() %>%
  st_transform(4326)

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

# Create a leaflet map with RUB lines on top
rubmap <- function(centre_lon, centre_lat, zoom) {
  m <- leaflet() %>% setView(lng = centre_lon, lat = centre_lat, zoom = zoom) %>%
    addTiles(urlTemplate = paste0("http://tiles-a.data-cdn.linz.govt.nz/services;key=", 
                                  lds_api_key, 
                                  "/tiles/v4/layer=95497/EPSG:3857/{z}/{x}/{y}.png")) %>%
    addPolylines(data = st_zm(rub), 
                 opacity = 1, 
                 color = "white", 
                 weight = 7) %>%
    addPolylines(data = st_zm(rub), 
                 opacity = 1, 
                 color = "red", 
                 weight = 4) 
  
  return(m)
}

# Make some maps
m1 <- rubmap(centre_lon = 174.623944, 
             centre_lat = -36.906430, 
             zoom = 16)

mapshot(x = m1, file = here("outputs/14-boundaries-1.png"))

m2 <- rubmap(centre_lon = 174.980656, 
             centre_lat = -37.055148, 
             zoom = 15)

mapshot(x = m2, file = here("outputs/14-boundaries-2.png"))

m3 <- rubmap(centre_lon = 174.928898, 
             centre_lat = -36.934918, 
             zoom = 14)

mapshot(x = m3, file = here("outputs/14-boundaries-3.png"))

m4 <- rubmap(centre_lon = 174.577955, 
             centre_lat = -36.779704, 
             zoom = 14)

mapshot(x = m4, file = here("outputs/14-boundaries-4.png"))

# *****************************************************************************