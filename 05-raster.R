# #30DayMapChallenge

# By @aschiff

# Challenge no: 05 -- Raster

# Data sources:
# NIWA bathymetry 250m model
# https://data-niwa.opendata.arcgis.com/datasets/a2582b1eb3584237a3b50418f379ca84


# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(janitor)
library(raster)

conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data -------------------------------------------------------------------

dat <- raster(x = here("data/bathymetry-250m.tif"))

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

png(filename = here("outputs/05-raster.png"), 
    width = 3000, 
    height = 3000, 
    units = "px")

par(mar = c(0, 0, 0, 0), 
    oma = c(0, 0, 0, 0), 
    bg = NA)

image(x = dat, 
      xlim = c(6000000, 7000000), 
      ylim = c(-4000000, -3000000), 
      xaxt = "n", yaxt = "n", 
      maxpixels = 100000000, 
      col = colorRampPalette(colors = c("blue", "plum"), 
                             space = "rgb", 
                             interpolate = "linear")(1024))

dev.off()


# *****************************************************************************
