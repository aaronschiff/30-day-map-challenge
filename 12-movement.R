# #30DayMapChallenge

# By @aschiff

# Challenge no: 12 -- Movement

# Data sources:
# Stats NZ Infoshare international travel and migration 


# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(glue)
library(rnaturalearth)

conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data -------------------------------------------------------------------

# Read horrible CSV file exported by Infoshare
migration_raw <- read_csv(file = here("data/ITM552301_20191111_012213_62.csv"), 
                          skip = 2)

# Clean up the horrible CSV file
migration <- migration_raw %>%
  filter(!is.na(X1), !is.na(`New Zealand`)) %>%
  rename(month = X1) %>%
  select(month:`South Africa`) %>%
  pivot_longer(names_to = "citizenship", values_to = "net_migr", -month) %>%
  mutate(net_migr = as.numeric(net_migr)) %>%
  separate(month, into = c("year", "month"), sep = "M", convert = TRUE)

# Calculate net migration totals by citizenship for 2010 to 2019
migration_by_citizenship_2010_2019 <- migration %>%
  filter(year > 2009, year < 2020) %>% 
  group_by(citizenship) %>%
  summarise(net_migr = sum(net_migr)) %>%
  ungroup() %>%
  filter(citizenship != "New Zealand") %>%
  arrange(desc(net_migr))

# World countries
countries <- ne_countries(returnclass = "sf", scale = "medium") %>%
  select(name, name_long) %>%
  st_transform(54016)

countries_centroids <- countries %>%
  st_set_geometry(value = st_centroid(st_geometry(countries)))

# Name concordance for names that don't match in the two datasets
names_lookup <- tribble(
  ~name_statsnz, ~name_ne, 
  "China, People's Republic of", "China", 
  "United States of America", "United States", 
  "Viet Nam", "Vietnam", 
  "Korea, Republic of", "Republic of Korean", 
  "Hong Kong (Special Administrative Region)", "Hong Kong", 
  "Czechia", "Czech Republic"
)

# Recode country names in migration data
migration_by_citizenship_2010_2019_recoded <- 
  migration_by_citizenship_2010_2019 %>%
  left_join(y = names_lookup, 
            by = c("citizenship" = "name_statsnz")) %>%
  mutate(name_ne = ifelse(is.na(name_ne), citizenship, name_ne))

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

# Join country centroids with migration data and make into something can be
# plotted with geom_point because geom_sf with size scaling doesn't seem to
# appear in the legend properly and I want to do labels
countries_centroids_x_migration <- left_join(x = countries_centroids %>%
                                               st_geometry() %>%
                                               st_centroid() %>%
                                               st_coordinates() %>%
                                               as_tibble() %>%
                                               mutate(name_long = countries_centroids$name_long), 
                                             y = migration_by_citizenship_2010_2019_recoded, 
                                             by = c("name_long" = "name_ne")) %>%
  filter(!is.na(net_migr)) %>%
  select(name_long, net_migr, X, Y)

# Make countries data for chart with short labels
countries_centroids_x_migration_chart <- 
  countries_centroids_x_migration %>%
  top_n(n = 10, wt = net_migr) %>%
  mutate(name_short = case_when(
    name_long == "Australia" ~ "Aust.", 
    name_long == "United Kingdom" ~ "U.K.", 
    name_long == "United States" ~ "U.S.", 
    name_long == "South Africa" ~ "S. Africa", 
    TRUE ~ name_long
  )) %>%
  mutate(label = glue("{name_short}\n{round(net_migr / 1000, 0)}k"))

# Bubble map
map <- ggplot() + 
  geom_sf(data = countries, 
          fill = grey(0.5), 
          colour = "black", 
          size = 0.1) + 
  geom_point(data = countries_centroids_x_migration_chart, 
             colour = "orange",
             stroke = 0, 
             aes(x = X, y = Y, size = net_migr)) + 
  geom_text(data = countries_centroids_x_migration_chart,
            colour = "white", 
            size = 2, 
            lineheight = 1, 
            fontface = "bold", 
            aes(x = X, y = Y, label = label)) + 
  scale_size_area(guide = "none", max_size = 12) + 
  theme_void() + 
  theme(plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"), 
        plot.background = element_rect(fill = grey(0.3))) + 
  scale_x_continuous(expand = expand_scale(0, 0)) + 
  scale_y_continuous(expand = expand_scale(0, 0))
  

ggsave(filename = here("outputs/12-movement.png"), 
       plot = map, 
       device = "png", 
       width = 20, 
       height = 15, 
       units = "cm")


# *****************************************************************************