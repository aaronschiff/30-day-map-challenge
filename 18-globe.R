# #30DayMapChallenge

# By @aschiff

# Challenge no: 18 -- Globe

# Colours from: https://www.color-hex.com/color-palette/51090

# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(lubridate)
library(sf)
library(glue)
library(suncalc)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("here", "here")

# *****************************************************************************


# *****************************************************************************
# Set up data -----------------------------------------------------------------

# Sequence of days in 2019, extend slightly into 2018 and 2020 due to timezone
# weirdness that I can't figure out
days_2019 <- seq(from = ymd("2018-12-30"), 
                 to = ymd("2020-01-02"), 
                 by = "1 day")

# Convert dttm to decimal hours
dttm_to_decimal <- function(d) {
  y <- hour(d) + minute(d) / 60 + second(d) / 3600
  return(y)
}

# Compute sunlight times data for location
compute_times_for_location <- function(lat, lon, location_name) {
  y <- map_dfr(.x = days_2019, 
               .f = getSunlightTimes,
               lat = lat, 
               lon = lon, 
               tz = "Pacific/Auckland") %>%
    as_tibble() %>%
    select(-lat, -lon) %>%
    mutate(dayend = ymd_hms(glue("{year(date)}-{month(date)}-{day(date)} 23:59:59"), 
                            tz = "Pacific/Auckland")) %>%
    pivot_longer(cols = -date, names_to = "measure", values_to = "value") %>%
    mutate(time_decimal = dttm_to_decimal(value)) %>%
    arrange(value) %>%
    filter(year(value) > 2018, year(value) < 2020) %>%
    mutate(day_in_year = as.integer(yday(value))) %>%
    group_by(day_in_year) %>%
    mutate(time_decimal_incr = time_decimal - lag(time_decimal)) %>%
    mutate(time_decimal_incr = ifelse(is.na(time_decimal_incr), 
                                      time_decimal, 
                                      time_decimal_incr)) %>%
    mutate(location = location_name) 
  
  return(y)
}

# Generate data
dat <- bind_rows(
  compute_times_for_location(lat = -36.848461, 
                             lon = 174.763336, 
                             location_name = "Auckland"), 
  compute_times_for_location(lat = -41.2866402, 
                             lon =  174.7755737, 
                             location_name = "Wellington"), 
  compute_times_for_location(lat = -43.533329,
                             lon = 172.6333313, 
                             location_name = "Christchurch"), 
  compute_times_for_location(lat = -45.8741608,
                             lon = 170.5036163, 
                             location_name = "Dunedin")
) %>%
  mutate(location = factor(location, 
                           levels = c("Auckland",
                                      "Wellington", 
                                      "Christchurch", 
                                      "Dunedin"), 
                           ordered = TRUE))

# *****************************************************************************


# *****************************************************************************
# Make the plot ----------------------------------------------------------------

test_plot <- dat %>%
  ggplot(aes(x = day_in_year, 
             y = time_decimal_incr, 
             fill = measure, 
             colour = measure, 
             group = day_in_year)) + 
  geom_col(position = position_stack(), 
           width = 1) + 
  scale_y_continuous(breaks = seq(0, 24, 1), 
                     expand = expand_scale(0, 0)) + 
  facet_wrap(facets = vars(location), ncol = 1) + 
  scale_fill_manual(aesthetics = c("fill", "colour"), 
    values = c(
    "nadir" = grey(0.1), 
    "nightEnd" = grey(0.1), 
    "nauticalDawn" = grey(0.1), 
    "dawn" = "#5b2c6f", 
    "sunrise" = "#e74c3c", 
    "sunriseEnd" = "#f39c12", 
    "goldenHourEnd" = "#f5b041", 
    "solarNoon" = "#f7dc6f", 
    "goldenHour" = "#f7dc6f", 
    "sunsetStart" = "#f5b041", 
    "sunset" = "#f39c12", 
    "dusk" = "#e74c3c", 
    "nauticalDusk" = "#5b2c6f", 
    "night" = grey(0.1), 
    "dayend" = grey(0.1)
  ), 
  guide = "none") + 
  coord_flip() + 
  theme_void() + 
  theme(plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
        plot.background = element_rect(fill = "white"), 
        strip.text = element_text(face = "bold",
                                  margin = margin(0, 0, 0.25, 0, "cm")), 
        panel.spacing.y = unit(0.5, "cm")) + 
  scale_x_continuous(expand = expand_scale(0, 0))

ggsave(filename = here("outputs/18-globe.png"), 
       plot = test_plot, 
       device = "png",
       width = 20, 
       height = 30, 
       units = "cm")

# *****************************************************************************