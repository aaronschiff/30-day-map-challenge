# #30DayMapChallenge

# By @aschiff

# Challenge no: 09 - Yellow

# Data sources:
# http://wlol.arlhs.com/index.php?lname=&sc=none&section=&number=&iota=&grid=&go=Submit&mode=search


# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(glue)
library(rvest)
library(biogeo)
library(rnaturalearth)

conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data -------------------------------------------------------------------

url <- "http://wlol.arlhs.com/index.php?lname=&sc=none&section=&number=&iota=&grid=&go=Submit&mode=search"

lighthouses <- url %>%
  read_html() %>%
  html_nodes(xpath = "/html/body/center/table") %>%
  html_nodes("tr") 

parse_lighthouse <- function(x) {
  l_dat <- x %>% 
    html_children() %>% 
    html_text %>% 
    gsub("\n","",.) %>% 
    enframe() %>%
    select(value) %>%
    mutate(value = str_trim(value)) %>%
    bind_cols(tibble(name = c("name", 
                              "arlhs_number", 
                              "status", 
                              "state_province", 
                              "lat", 
                              "lon", 
                              "map", 
                              "gridsquare", 
                              "iota", 
                              "activations"))) %>%
    pivot_wider(names_from = name, values_from = value)
  
  return(l_dat)
}

parsed_lighthouses <- map_dfr(.x = lighthouses[3:length(lighthouses)], 
                              .f = parse_lighthouse)

world <- ne_countries(returnclass = "sf") %>%
  st_transform(54016)

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

# Clean up data for map 
map_lighthouses <- parsed_lighthouses %>%
  filter(status == "- -", lat != "-", lon != "-") %>%
  select(name, lat, lon) %>%
  separate(lat, into = c("lat_dd", "lat_x"), sep = "°", convert = TRUE) %>%
  separate(lat_x, into = c("lat_mm", "lat_ns"), sep = "\\'", convert = TRUE) %>%
  mutate(lat_ns = str_trim(lat_ns)) %>%
  separate(lon, into = c("lon_dd", "lon_x"), sep = "°", convert = TRUE) %>%
  separate(lon_x, into = c("lon_mm", "lon_ns"), sep = "\\'", convert = TRUE) %>%
  mutate(lon_ns = str_trim(lon_ns)) %>%
  mutate(lat = dms2dd(dd = lat_dd, mm = lat_mm, ss = 0, ns = lat_ns)) %>%
  mutate(lon = dms2dd(dd = lon_dd, mm = lon_mm, ss = 0, ns = lon_ns)) %>%
  select(name, lat, lon) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(4326) %>%
  st_transform(crs = 54016)

# Plot map
map <- ggplot() + 
  geom_sf(data = world, 
          fill = "black", 
          colour = "black", 
          size = 0.1) + 
  geom_sf(data = map_lighthouses, 
          colour = rgb(1, 1, 0, 0.5), 
          stroke = 0, 
          size = 0.4) + 
  theme_void() + 
  theme(plot.background = element_rect(fill = grey(0.05)), 
        plot.margin = margin(0, 0.1, 0, 0.1, "cm")) + 
  scale_x_continuous(expand = expand_scale(0, 0)) +
  scale_y_continuous(expand = expand_scale(0, 0))

ggsave(filename = here("outputs/09-yellow.png"), 
       plot = map, 
       device = "png", 
       width = 20, 
       height = 15, 
       units = "cm")

# *****************************************************************************
