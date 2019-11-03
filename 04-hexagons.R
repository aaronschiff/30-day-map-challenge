# #30DayMapChallenge

# By @aschiff

# Challenge no: 04 -- Hexagons!

# Data sources: 
# NZ Tree Points (Topo, 1:50k)
# https://data.linz.govt.nz/layer/50365-nz-tree-points-topo-150k/


# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(glue)
library(ggthemes)
library(hexbin)

conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data -------------------------------------------------------------------

dat <- read_sf(here("data/lds-nz-tree-points-topo-150k-SHP/nz-tree-points-topo-150k.shp"))

treepoints <- dat %>% st_coordinates() %>% as_tibble()

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

map <- ggplot(treepoints) + 
  geom_hex(aes(x = X, 
               y = Y, 
               fill = factor(x = as.integer(1000 * round(..count.. / 1000, 0)), 
                             labels = c("More than 0 to fewer than 1000", 
                                        "More than 1000 to fewer than 2000", 
                                        "More than 2000 to fewer than 3000", 
                                        "More than 3000 to fewer than 4000"))), 
           binwidth = c(10000, 10000), 
           size = 0) + 
  coord_fixed() + 
  theme_void() + 
  scale_fill_brewer(palette = "BrBG", 
                    na.value = grey(0.5), 
                    na.translate = TRUE, 
                    name = "Number of tree points")

ggsave(filename = here("outputs/04-hexagons.png"), 
       plot = map, 
       device = "png", 
       width = 20, 
       height = 20, 
       units = "cm")

# *****************************************************************************
