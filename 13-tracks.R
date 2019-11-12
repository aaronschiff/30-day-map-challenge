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

# South Pacific storms
storms <- read_sf("data/IBTrACS/IBTrACS.SP.list.v04r00.lines.shp") %>%
  clean_names() %>%
  mutate(iso_time = ymd_hms(iso_time)) %>%
  st_transform(crs = 3832)   # PDC Mercator -- for Pacific 

# World map
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs = 3832)

# Tidy up storms wind speeds for mapping
storms_since_1980 <- storms %>%
  filter(track_type == "main", year(iso_time) > 1979)

mean_wmo_wind <- mean(storms_since_1980$wmo_wind, na.rm = TRUE)

storms_since_1980_missing_wind <- storms_since_1980 %>%
  st_drop_geometry() %>%
  mutate(wind_obs = ifelse(!is.na(wmo_wind), 1, 0)) %>%
  group_by(sid) %>%
  summarise(wind_obs = sum(wind_obs)) %>%
  ungroup() %>%
  filter(wind_obs < 2)

storms_since_1980_interpolated_wind <- storms_since_1980 %>%
  mutate(wmo_wind_interp = ifelse(sid %in% storms_since_1980_missing_wind$sid, 
                                  mean_wmo_wind, 
                                  wmo_wind)) %>%
  group_by(sid) %>%
  mutate(wmo_wind_interp = approx(x = iso_time, 
                                  y = wmo_wind_interp, 
                                  xout = iso_time, 
                                  rule = 2)$y) %>%
  mutate(wmo_wind_max = max(wmo_wind_interp, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(decade = as.integer(floor(year(iso_time) / 10) * 10)) %>%
  mutate(sid2 = fct_reorder(sid, wmo_wind_max))

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

bbox = st_bbox(map_dat)

map <- ggplot() + 
  geom_sf(data = world, 
          fill = grey(0.1), 
          linetype = "blank") + 
  geom_sf(data = storms_since_1980_interpolated_wind, 
          mapping = aes(colour = wmo_wind_interp, group = sid2), 
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