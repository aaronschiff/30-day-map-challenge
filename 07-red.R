# #30DayMapChallenge

# By @aschiff

# Challenge no: 07 -- Red

# Data sources:
# Census 2018 data on NZ.Stat
# Statistical Area 2 boundaries clipped
# https://datafinder.stats.govt.nz/layer/92213-statistical-area-2-2018-clipped-generalised/


# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(glue)
library(readxl)

conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data -------------------------------------------------------------------

# Load population data from NZ.Stat horrible Excel file
pop_dat <- read_excel(path = here("data/census-2018-pop-auckland-sa2.xlsx"), 
                      skip = 7, 
                      col_names = c("area", "junk", "2006", "2013", "2018")) %>%
  select(-junk) %>%
  filter(!is.na(`2018`)) %>%
  pivot_longer(names_to = "year", values_to = "pop", -area) %>%
  mutate(year = as.integer(year))

# Calculate population changes 2018 vs 2013
pop_changes_dat <- pop_dat %>%
  filter(year != 2006) %>%
  pivot_wider(names_from = year, values_from = pop) %>%
  mutate(delta_pop = `2018` - `2013`) %>%
  select(area, delta_pop)

# Read SA2 shapes
sa2_dat <- read_sf(here("data/statsnzstatistical-area-2-2018-clipped-generalised-SHP/statistical-area-2-2018-clipped-generalised.shp")) %>%
  clean_names()

# Join pop_changes_dat and SA2 shapes
pop_changes_dat_sa2 <- left_join(x = pop_changes_dat, 
                                 y = sa2_dat, 
                                 by = c("area" = "sa22018_1")) %>%
  filter(!is.na(sa22018_v1)) %>%   
  st_as_sf()


# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

# Generate random dots for SA2 areas with population growth
pop_changes_dat_sa2_postive <- pop_changes_dat_sa2 %>% filter(delta_pop > 0)

# Make the map
# Uses geom_point instead of geom_sf as it seems to be a lot faster
dots <- st_sample(x = pop_changes_dat_sa2_postive, 
                  size = pop_changes_dat_sa2_postive$delta_pop) %>%
  st_geometry() %>%
  st_coordinates() %>%
  as_tibble()

map <- ggplot() + 
  geom_point(aes(x = X, y = Y), 
             colour = "red", 
             size = 0.15, 
             shape = 16, 
             stroke = 0, 
             data = dots) + 
  geom_sf(colour = grey(0.5), 
          fill = NA, 
          size = 0.1, 
          data = st_geometry(pop_changes_dat_sa2)) + 
  theme_void() + 
  theme(plot.background = element_rect(fill = "white"), 
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) + 
  scale_x_continuous(expand = expand_scale(0, 0)) + 
  scale_y_continuous(expand = expand_scale(0, 0))

ggsave(filename = here("outputs/07-red.png"), 
       plot = map, 
       device = "png", 
       width = 20, 
       height = 25, 
       units = "cm")

# *****************************************************************************
