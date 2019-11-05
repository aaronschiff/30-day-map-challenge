# #30DayMapChallenge

# By @aschiff

# Challenge no: 06 -- Blue

# Data sources:
# NZ Lake Polygons, 1:50k
# https://data.linz.govt.nz/layer/50293-nz-lake-polygons-topo-150k/ 


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

lakes <- read_sf(here("data/nz-lake-polygons-topo-150k/nz-lake-polygons-topo-150k.shp"))

# Fix Lake Ellesmere -- one of its three polygons is missing the name!
lakes <- lakes %>%
  mutate(name = ifelse(t50_fid == 5002915, "Lake Ellesmere (Te Waihora)", name))

# Aggregate polygons of named lakes
named_lakes <- lakes %>%
  filter(!is.na(name)) %>%
  select(name) %>%
  mutate(area = as.numeric(st_area(.)))

aggregated_named_lakes <- aggregate(x = named_lakes %>% select(-name), 
                                    by = list(name = factor(named_lakes$name)), 
                                    FUN = sum, 
                                    do_union = TRUE) %>%
  mutate(name = as.character(name))

# Find largest lakes
top_lakes <- aggregated_named_lakes %>% 
  top_n(n = 9, wt = area) %>%
  arrange(desc(area))

# Re-centre each lake on its centroid
top_lakes_recentred <- top_lakes %>%
  st_set_geometry(value = st_geometry(top_lakes) - 
                    st_centroid(st_geometry(top_lakes)))

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

map <- ggplot(top_lakes_recentred) + 
  geom_sf(fill = "blue", 
          linetype = "blank") + 
  facet_wrap(facets = vars(fct_reorder(name, -area)), 
             ncol = 3) + 
  theme_void()

ggsave(filename = here("outputs/06-blue.png"), 
       plot = map, 
       device = "png", 
       width = 20, 
       height = 20, 
       units = "cm")

# *****************************************************************************
