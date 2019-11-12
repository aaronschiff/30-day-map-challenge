# #30DayMapChallenge

# By @aschiff

# Challenge no: 13 -- Tracks

# Data sources:
# NOAA IBTrACS
# https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/shapefile/IBTrACS.SP.list.v04r00.lines.zip

# Hat tip to @zentree
# https://gist.github.com/zentree/a02354093d5ef2d47732f51a0d8b8589


# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(janitor)
library(lubridate)
library(sf)
library(glue)
library(rnaturalearth)

conflict_prefer("filter", "dplyr")
conflict_prefer("here", "here")

# *****************************************************************************


# *****************************************************************************
# Load data -------------------------------------------------------------------

storms <- read_sf("data/IBTrACS/IBTrACS.SP.list.v04r00.lines.shp") %>%
  clean_names() %>%
  mutate(iso_time = ymd_hms(iso_time)) %>%
  st_transform(crs = 3832)   # PDC Mercator -- for Pacific 

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs = 3832)

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

# Storms by decade since 1980, with at least two wmo_wind observations
selected_sid <- storms %>%
  st_drop_geometry() %>%
  filter(track_type == "main", 
         year(iso_time) > 1979, 
         !is.na(wmo_wind)) %>%
  count(sid) %>%
  filter(n > 1) %>%
  pull(sid)

# Interpolate missing values in wmo_wind
map_dat <- storms %>%
  filter(track_type == "main", 
         sid %in% selected_sid) %>%
  mutate(decade = as.integer(floor(year(iso_time) / 10) * 10)) %>%
  group_by(sid) %>%
  mutate(wmo_wind_interp = approx(x = iso_time, 
                                  y = wmo_wind, 
                                  xout = iso_time, 
                                  rule = 2)$y) %>%
  ungroup()

# Make the map
bbox = st_bbox(map_dat)

map <- ggplot() + 
  geom_sf(data = world, 
          fill = grey(0.1), 
          linetype = "blank") + 
  geom_sf(data = map_dat, 
          mapping = aes(colour = wmo_wind_interp), 
          size = 0.3, 
          alpha = 0.75) + 
  facet_wrap(facets = vars(decade), 
             ncol = 2, 
             labeller = as_labeller(function(x) { paste0(x, "s") } )) + 
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) + 
  theme_void() + 
  theme_void() + 
  theme(plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
        plot.background = element_rect(fill = "black"), 
        panel.spacing.x = unit(0.5, "cm"), 
        panel.spacing.y = unit(0.5, "cm"), 
        strip.text = element_text(face = "bold", 
                                  colour = "white", 
                                  margin = margin(0, 0, 0.5, 0, "cm"))) + 
  scale_x_continuous(expand = expand_scale(0, 0)) + 
  scale_y_continuous(expand = expand_scale(0, 0)) + 
  scale_colour_gradient(low = "midnightblue", 
                        high = "deepskyblue1", 
                        guide = "none")

ggsave(filename = here("outputs/13-tracks.png"), 
       plot = map, 
       device = "png", 
       width = 30, 
       height = 18, 
       units = "cm")

# *****************************************************************************