# #30DayMapChallenge

# By @aschiff

# Challenge no: 10 -- Black and white

# Data sources:
# NZ airport polygons, 1:50k
# https://data.linz.govt.nz/layer/50237-nz-airport-polygons-topo-150k/

# NZ runway polygons, 1:50k
# https://data.linz.govt.nz/layer/50333-nz-runway-polygons-topo-150k/ 

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

conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data -------------------------------------------------------------------

airports <- read_sf(here("data/nz-airport-polygons-topo-150k/nz-airport-polygons-topo-150k.shp")) %>%
  filter(!is.na(name)) %>%
  filter(!str_detect(name, "Aerodrome"))

runways <- read_sf(here("data/nz-runway-polygons-topo-150k/nz-runway-polygons-topo-150k.shp"))

buildings1 <- read_sf(here("data/nz-building-outlines/nz-building-outlines-1.shp"))
buildings2 <- read_sf(here("data/nz-building-outlines/nz-building-outlines-2.shp"))

buildings <- rbind(buildings1, buildings2) %>%
  select(building_i)

# Intersect runways with airports
runways_x_airports <- st_intersection(x = runways, y = airports)

# Intersect buildings with airports
buildings_x_airports <- st_intersection(x = buildings, y = airports)

# Re-centre airport polygons on airport centroids
airports_recentred <- airports %>%
  st_set_geometry(value = st_geometry(airports) - 
                    st_centroid(st_geometry(airports))) %>%
  st_set_crs(2193)

# Re-centre runway polygons on airport centroids
recentre_runway <- function(r) {
  cp <- airports %>% filter(name == r$name) 
  ra <- st_geometry(r) - st_centroid(st_geometry(cp))
  return(ra)
}

runways_x_airports_recentred_list <- list()
for (i in 1:nrow(runways_x_airports)) {
  r <- runways_x_airports[i, ]
  rr <- st_set_geometry(x = r, value = recentre_runway(r))
  runways_x_airports_recentred_list[[i]] <- rr
}

runways_x_airports_recentred <- do.call(rbind, runways_x_airports_recentred_list) %>%
  st_set_crs(2193)

# Recentre building polygons on airport centroids
recentre_building <- function(b) {
  cp <- airports %>% filter(name == b$name) 
  ba <- st_geometry(b) - st_centroid(st_geometry(cp))
  return(ba)
}

buildings_x_airports_recentred_list <- list()
for (i in 1:nrow(buildings_x_airports)) {
  b <- buildings_x_airports[i, ]
  bb <- st_set_geometry(x = b, value = recentre_building(b))
  buildings_x_airports_recentred_list[[i]] <- bb
}

buildings_x_airports_recentred <- do.call(rbind, buildings_x_airports_recentred_list) %>%
  st_set_crs(2193)

# Add northing of each airport
airport_northings <- airports %>%
  st_geometry() %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  select(airport_northing = Y) %>%
  mutate(name = airports$name)

airports_recentred <- airports_recentred %>%
  left_join(airport_northings, by = "name")

runways_x_airports_recentred <- runways_x_airports_recentred %>%
  left_join(airport_northings, by = "name")

buildings_x_airports_recentred <- buildings_x_airports_recentred %>%
  left_join(airport_northings, by = "name")

# Tidy up airport names
airports_recentred <- airports_recentred %>%
  mutate(name = str_replace(name, "Airport", "")) %>%
  mutate(name = str_trim(name)) %>%
  mutate(name = fct_reorder(name, -airport_northing))

runways_x_airports_recentred <- runways_x_airports_recentred %>%
  mutate(name = str_replace(name, "Airport", "")) %>%
  mutate(name = str_trim(name)) %>%
  mutate(name = fct_reorder(name, -airport_northing))

buildings_x_airports_recentred <- buildings_x_airports_recentred %>%
  mutate(name = str_replace(name, "Airport", "")) %>%
  mutate(name = str_trim(name)) %>%
  mutate(name = fct_reorder(name, -airport_northing))

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

map <- ggplot() + 
  geom_sf(data = airports_recentred, 
          fill = grey(0.85), 
          linetype = "blank") + 
  geom_sf(data = buildings_x_airports_recentred,
          fill = grey(0.6),
          linetype = "blank") +
  geom_sf(data = runways_x_airports_recentred, 
          fill = "black", 
          linetype = "blank") + 
  facet_wrap(facets = vars(name), 
             ncol = 7, 
             labeller = label_wrap_gen(width = 17)) + 
  theme_void() + 
  theme(plot.margin = margin(0.5, 0, 0.5, 0, "cm"), 
        panel.spacing.x = unit(0.5, "cm"), 
        panel.spacing.y = unit(0.5, "cm"), 
        plot.background = element_rect(fill = grey(0.95))) + 
  scale_x_continuous(expand = expand_scale(0, 0)) + 
  scale_y_continuous(expand = expand_scale(0, 0))

ggsave(filename = here("outputs/10-black-and-white.png"), 
       plot = map, 
       device = "png", 
       width = 23, 
       height = 19, 
       units = "cm")

# *****************************************************************************
