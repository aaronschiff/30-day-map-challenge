# #30DayMapChallenge

# By @aschiff

# Challenge no: 01 -- points

# Data sources:
# LINZ / NZ street address
# https://data.linz.govt.nz/layer/53353-nz-street-address/


# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(glue)
library(ggthemes)
library(viridis)

conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data -------------------------------------------------------------------

dat1 <- read_sf(here("data/nz-street-address/nz-street-address-1.shp"))
dat2 <- read_sf(here("data/nz-street-address/nz-street-address-2.shp"))
dat3 <- read_sf(here("data/nz-street-address/nz-street-address-3.shp"))

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

# Combined reduced data
dat <- rbind(
  dat1 %>% select(address_id, address_nu), 
  dat2 %>% select(address_id, address_nu), 
  dat3 %>% select(address_id, address_nu)
)

downtown_akl_point <- st_point(x = c(174.763817, -36.842837), dim = "XY") %>%
  st_sfc(crs = 4326) %>%
  st_transform(crs = 2193) %>%
  st_as_sf()

downtown_akl_radius <- st_buffer(x = downtown_akl_point, dist = 10000)   # 10km circle

plot_dat <- st_intersection(x = downtown_akl_radius, y = dat)

plot_map <- ggplot(plot_dat, aes(colour = address_nu)) + 
  geom_sf(size = 0.1) + 
  scale_colour_viridis(guide = "none") + 
  theme_void()

ggsave(filename = here("outputs/01-points.png"), 
       plot = plot_map, 
       width = 20, 
       height = 20, 
       units = "cm")

# *****************************************************************************
