# #30DayMapChallenge

# By @aschiff

# Challenge no: 11 -- Elevation

# Data sources:
# NZ Contours, 1:500k
# https://data.linz.govt.nz/layer/50205-nz-contours-topo-1500k/
# NZ Coastline polygons, 1:500k
# https://data.linz.govt.nz/layer/51560-nz-coastlines-and-islands-polygons-topo-1500k/


# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(glue)
library(parallel)

conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data -------------------------------------------------------------------

contours <- read_sf(here("data/lds-nz-contours-topo-1500k-SHP/nz-contours-topo-1500k.shp"))

coastline <- read_sf(here("data/lds-nz-coastlines-and-islands-polygons-topo-1500k-SHP/nz-coastlines-and-islands-polygons-topo-1500k.shp"))

# Remove small islands
coastline_mainland <- coastline %>%
  mutate(area = as.numeric(st_area(.))) %>%
  filter(area > 1e8)

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

# Find all areas above a certain elevation, as polygons
area_above <- function(emin) {
  a <- contours %>%
    filter(elevation > emin) %>%
    st_polygonize() %>%
    st_union()
  
  asf <- tibble(emin = as.integer(emin)) %>%
    st_set_geometry(a)
  
  return(asf)
}

# Find areas above elevations from 250m to 2000m
erange <- seq(250, 2000, 250)

elevation_areas_list <- mclapply(X = erange, 
                                 FUN = area_above, 
                                 mc.cores = detectCores() - 1)

elevation_areas <- do.call(rbind, elevation_areas_list)

# Add coastline polygon as 0 elevation shape
coastline_mainland_area <- 
  tibble(emin = 0L) %>%
  st_set_geometry(st_union(coastline_mainland))

elevation_areas_with_coastline <- rbind(coastline_mainland_area, elevation_areas)

# Plot map
map <- ggplot() + 
  geom_sf(data = coastline_mainland, 
          fill = "white", 
          linetype = "blank") + 
  geom_sf(data = elevation_areas_with_coastline, 
          fill = "red", 
          linetype = "blank") + 
  facet_wrap(facets = vars(emin), 
             ncol = 3, 
             labeller = as_labeller(function(x) { paste0("Above ", x, " metres")})) + 
  theme_void() + 
  theme(plot.margin = margin(0.5, 1, 0.5, 1, "cm"), 
        panel.spacing.x = unit(0, "cm"), 
        panel.spacing.y = unit(1, "cm"),
        strip.text = element_text(margin = margin(0, 0, 0.25, 0, "cm"), 
                                  face = "bold"), 
        plot.background = element_rect(fill = grey(0.9))) + 
  scale_x_continuous(expand = expand_scale(0, 0)) + 
  scale_y_continuous(expand = expand_scale(0, 0))

ggsave(filename = here("outputs/11-elevation.png"), 
       plot = map, 
       device = "png", 
       width = 14, 
       height = 20, 
       units = "cm")

# *****************************************************************************