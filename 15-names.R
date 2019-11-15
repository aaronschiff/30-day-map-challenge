# #30DayMapChallenge

# By @aschiff

# Challenge no: 15 -- Names

# Data sources:
# NZ Place Names
# https://data.linz.govt.nz/layer/51681-nz-place-names-nzgb/
# Regional Council 2019 areas
# https://datafinder.stats.govt.nz/layer/98765-regional-council-2019-clipped-generalised/


# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(glue)
library(stringdist)
library(parallel)
library(ggrepel)
library(rmapshaper)

conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data -------------------------------------------------------------------

# NZ Place Names
names <- read_sf(here("data/lds-nz-place-names-nzgb-SHP/nz-place-names-nzgb.shp")) %>%
  st_transform(crs = 2193) %>%
  select(name, feat_type) %>%
  filter(feat_type != "Stream")

# NZ regional council areas
regions <- read_sf(here("data/statsnzregional-council-2019-clipped-generalised-SHP/regional-council-2019-clipped-generalised.shp")) %>%
  clean_names() %>%
  filter(regc2019_1 != "Area Outside Region") %>%
  select(regc2019_1)

# Find the place name in each region that is "most similar" to all other names
# in that region
names_x_regions <- st_intersection(x = names, y = regions)

compare_names_for_region = function(r) {
  rn <- names_x_regions %>%
    st_drop_geometry() %>%
    filter(regc2019_1 == r) %>%
    select(name)
  
  rn_combos <- crossing(n1 = rn$name, n2 = rn$name) %>%
    filter(n1 != n2)
  
  rn_dists <- rn_combos %>%
    mutate(dist = stringdist(a = n1, b = n2, method = "osa")) %>%
    group_by(n1) %>%
    summarise(totaldist = sum(dist)) %>%
    mutate(totaldist = totaldist + runif(n = nrow(.))) %>%
    ungroup()
  
  rn_selected <- rn_dists %>% top_n(n = 1, wt = -totaldist)
  
  return(tibble(
    region = r, 
    selected_name = rn_selected$n1
  ))
}

names_for_regions <- mclapply(X = names_x_regions %>% pull(regc2019_1) %>% unique(), 
                              FUN = compare_names_for_region, 
                              mc.cores = detectCores() - 1) %>%
  bind_rows()

# Region centroids with selected names
region_centroids_with_names <- regions %>%
  st_geometry() %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  mutate(region = regions$regc2019_1) %>%
  left_join(names_for_regions, by = "region")

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

regions_simple <- ms_simplify(input = regions)

map <- ggplot() + 
  geom_sf(data = regions_simple, 
          fill = grey(0.8),   
          colour = "white", 
          stroke = 0.5) + 
  geom_point(data = region_centroids_with_names, 
             mapping = aes(x = X, y = Y), 
             colour = "red", 
             size = 1) + 
  geom_text_repel(data = region_centroids_with_names, 
                  mapping = aes(x = X, y = Y, label = selected_name), 
                  colour = "black", 
                  fontface = "bold") + 
  theme_void() + 
  theme(plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
        plot.background = element_rect(fill = "white")) + 
  scale_x_continuous(expand = expand_scale(0, 0)) + 
  scale_y_continuous(expand = expand_scale(0, 0))

ggsave(filename = here("outputs/15-names.png"), 
       plot = map, 
       device = "png", 
       width = 16, 
       height = 20, 
       units = "cm")

# *****************************************************************************