# #30DayMapChallenge

# By @aschiff

# Challenge no: 16 -- Places

# Data sources:
# NZ Place Names
# https://data.linz.govt.nz/layer/51681-nz-place-names-nzgb/


# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(glue)
library(parallel)
library(rmapshaper)

conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data -------------------------------------------------------------------

# NZ street addresses
addr1 <- read_sf(here("data/nz-street-address/nz-street-address-1.shp"))
addr2 <- read_sf(here("data/nz-street-address/nz-street-address-2.shp"))
addr3 <- read_sf(here("data/nz-street-address/nz-street-address-3.shp"))

addr <- rbind(addr1 %>% select(address_id), 
              addr2 %>% select(address_id), 
              addr3 %>% select(address_id))

rm(addr1, addr2, addr3)

addr_points <- addr %>%
  st_geometry() %>%
  st_coordinates() %>%
  as_tibble()

# NZ coastlines
coastline <- read_sf(here("data/lds-nz-coastlines-and-islands-polygons-topo-1500k-SHP/nz-coastlines-and-islands-polygons-topo-1500k.shp"))

coastline_simple <- ms_simplify(coastline)

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

# Make boxes around each address point
boxpoint <- function(i, d) {
  xL <- addr_points[[i, "X"]] - d
  xU <- addr_points[[i, "X"]] + d
  yL <- addr_points[[i, "Y"]] - d
  yU <- addr_points[[i, "Y"]] + d
  return(tibble(xL = xL, xU = xU, yL = yL, yU = yU))
}

addr_boxes <- mclapply(X = 1:nrow(addr_points), 
                       FUN = boxpoint, 
                       d = 50, 
                       mc.cores = detectCores() - 1) %>%
  bind_rows()

# Make the maps
map1 <- ggplot() + 
  geom_sf(data = coastline_simple, 
          fill = "black", 
          linetype = "blank") + 
  geom_rect(mapping = aes(xmin = xL, xmax = xU, ymin = yL, ymax = yU), 
            fill = grey(0.9), 
            linetype = "blank", 
            data = addr_boxes) + 
  theme_void() + 
  theme(plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
        plot.background = element_rect(fill = grey(0.9))) + 
  scale_x_continuous(expand = expand_scale(0, 0)) + 
  scale_y_continuous(expand = expand_scale(0, 0))

ggsave(filename = here("outputs/16-places-1.png"), 
       plot = map1, 
       device = "png",
       width = 16, 
       height = 20, 
       units = "cm")

map2 <- ggplot() + 
  geom_sf(data = coastline_simple, 
          fill = grey(0.9), 
          linetype = "blank") + 
  geom_rect(mapping = aes(xmin = xL, xmax = xU, ymin = yL, ymax = yU), 
            fill = "black", 
            linetype = "blank", 
            data = addr_boxes) + 
  theme_void() + 
  theme(plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
        plot.background = element_rect(fill = grey(0.9))) + 
  scale_x_continuous(expand = expand_scale(0, 0)) + 
  scale_y_continuous(expand = expand_scale(0, 0))

ggsave(filename = here("outputs/16-places-2.png"), 
       plot = map2, 
       device = "png",
       width = 16, 
       height = 20, 
       units = "cm")

# *****************************************************************************