# #30DayMapChallenge

# By @aschiff

# Challenge no: 17 -- zones

# Data sources:
# Auckland Unitary Plan shapefiles
# http://hub.arcgis.com/datasets/4673e8f3f20942d7a21cfcb36971e103


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

# Read all shapefiles into a list, except "contours" layers
file_names <- list.files(path = here("data/UnitaryPlan_Shapefile/Shapefile"), 
                         pattern = "*.shp") %>%
  enframe() %>%
  filter(!str_detect(value, "Contours"))

shapefiles <- vector(mode = "list", length = nrow(file_names))

for (i in 1:nrow(file_names)) {
  fn <- file_names[[i, "value"]]
  shapefiles[[i]] <- read_sf(here(glue("data/UnitaryPlan_Shapefile/Shapefile/{fn}")))
}

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

# Find overall bounding box
bboxes_list <- vector(mode = "list", length = length(shapefiles))

for (i in 1:length(shapefiles)) {
  bboxes_list[[i]] <- st_bbox(shapefiles[[i]])
}

bboxes <- do.call(rbind, bboxes_list) %>% as_tibble()

bbox <- bboxes %>%
  summarise(
    xmin = min(xmin), 
    ymin = min(ymin), 
    xmax = max(xmax), 
    ymax = max(ymax)
  )

aspect <- (bbox[[1, "ymax"]] - bbox[[1, "ymin"]]) / 
  (bbox[[1, "xmax"]] - bbox[[1, "xmin"]])

# Plot using base graphics because I'm not sure how to plot all the shapefiles
# using ggplot without tediously defining a geom_sf for each layer.

png(filename = here("outputs/17-zones.png"), 
    width = 2000, 
    height = round(aspect * 2000))

par(mar = c(0, 0, 0, 0), 
    oma = c(0, 0, 0, 0), 
    bg = "white")

plot(x = 0, y = 0, 
     type = "n", 
     xaxt = "n", yaxt = "n", 
     frame.plot = FALSE, 
     xlim = c(bbox[[1, "xmin"]], bbox[[1, "xmax"]]), 
     ylim = c(bbox[[1, "ymin"]], bbox[[1, "ymax"]]))

for (i in 1:length(shapefiles)) {
  plot(st_geometry(shapefiles[[i]]), 
       col = rgb(0, 0, 0, 0.1), 
       border = NA, 
       add = TRUE)
}

dev.off()

# *****************************************************************************