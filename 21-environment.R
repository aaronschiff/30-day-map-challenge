# #30DayMapChallenge

# By @aschiff

# Challenge no: 21 -- Environment

# Data sources:
# NZ Coastlines & Islands Polygons 1:50k
# https://data.linz.govt.nz/layer/51153-nz-coastlines-and-islands-polygons-topo-150k/
# NZ Lake Polygons 1:50k
# https://data.linz.govt.nz/layer/50293-nz-lake-polygons-topo-150k/
# NZ River Polygons 1:50k
# https://data.linz.govt.nz/layer/50328-nz-river-polygons-topo-150k/
# MFE River Environment Classification
# https://data.mfe.govt.nz/layer/51845-river-environment-classification-new-zealand-2010/


# *****************************************************************************
# Setup -----------------------------------------------------------------------

library(conflicted)
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(glue)
library(parallel)
library(readxl)
library(conca)

conflict_prefer("filter", "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data -------------------------------------------------------------------

# River environment classification
rivers <- read_sf(here("data/mfe-river-environment-classification-new-zealand-2010-SHP/river-environment-classification-new-zealand-2010.shp")) %>%
  clean_names()

rivers_nogeom <- rivers %>%
  select(objectid, nzreach, nztnode, nzfnode) %>%
  st_drop_geometry()

river_names <- read_excel(here("data/NZ-river-names.xlt")) %>%
  clean_names()

# Other topo data
coastline <- read_sf(here("data/lds-nz-coastlines-and-islands-polygons-topo-150k-SHP/nz-coastlines-and-islands-polygons-topo-150k.shp"))

lakes <- read_sf("data/nz-lake-polygons-topo-150k/nz-lake-polygons-topo-150k.shp") %>%
  mutate(area = as.numeric(st_area(.)))

river_polygons <- read_sf("data/nz-river-polygons-topo-150k/nz-river-polygons-topo-150k.shp") %>%
  mutate(area = as.numeric(st_area(.)))

# *****************************************************************************


# *****************************************************************************
# Process data ----------------------------------------------------------------

# Separate the river networks data into discrete networks
# Terminal segments have a tnode that is not the fnode for any other segment
terminal_segments <- anti_join(
  rivers_nogeom, 
  rivers_nogeom, 
  by = c("nztnode" = "nzfnode")
) %>%
  pull(nzreach)

# Find upstream segments of given segments
upstream <- function(s) {
  u <- rivers_nogeom %>% filter(nztnode %in% unique(s$nzfnode))
  return(u)
}

# Traverse the network upstream from a given segment with nzreach ID, then return the network with geometry
traverse_network <- function(nzr) {
  seg <- rivers_nogeom %>% filter(nzreach == nzr)
  net <- seg
  
  su <- upstream(seg)
  net <- bind_rows(net, su)
  
  while (nrow(su) > 0) {
    su <- upstream(su)
    net <- bind_rows(net, su)
  }
  
  net <- net %>%
    mutate(source_nzreach = nzr)
  
  return(net)
}

# Find network for all terminal nodes
terminal_networks <- mclapply(terminal_segments, 
                              traverse_network, 
                              mc.cores = detectCores() - 1) %>%
  bind_rows() %>%
  left_join(river_names, by = "nzreach")

# Add source_nzreach to rivers -- that's enough to define each discrete network
rivers_networks <- rivers %>%
  left_join(terminal_networks %>%
              select(nzreach, source_nzreach, rivername), 
            by = "nzreach")

# Count river network sizes
river_network_sizes <- terminal_networks %>%
  count(source_nzreach)

# *****************************************************************************


# *****************************************************************************
# Make the map ----------------------------------------------------------------

# Plot river network for a given source
plot_river <- function(s) {
  # Plot config
  OCEAN_COLOUR <- "blue"
  COASTLINE_COLOUR <- OCEAN_COLOUR
  COASTLINE_LWD <- 1
  LAND_COLOUR <- gray(level = 0.05)
  
  LAKE_COLOUR <- OCEAN_COLOUR
  LAKE_BORDER_COLOUR <- LAKE_COLOUR
  LAKE_BORDER_LWD <- 1
  
  RIVER_POLYGON_COLOUR <- OCEAN_COLOUR
  RIVER_POLYGON_BORDER_COLOUR <- RIVER_POLYGON_COLOUR
  RIVER_POLYGON_BORDER_LWD <- 3
  RIVER_COLOUR <- OCEAN_COLOUR
  
  MIN_LAKE_SIZE <- 100000
  RIVER_BOUNDARY_BUFFER <- 10
  
  BBOX_EXPANSION <- 0.1
  OUTPUT_SCALE <- 50   # Metres per pixel
  
  # Return plotting widths for stream order
  stream_order_lwd <- function(o) {
    y <- round(sqrt(4 * o), 1)
    return(y)
  }
  
  # Get data for this map and add plotting characteristics
  river_dat <- rivers_networks %>%
    filter(source_nzreach == s) %>%
    mutate(plot_colour = stream_order_colour(order), 
           plot_lwd = stream_order_lwd(order)) %>%
    arrange(order) %>%
    st_transform(crs = 2193)
  
  river_dat_boundary <- river_dat %>%
    st_combine() %>%
    st_boundary() %>%
    st_sf() %>%
    concaveman() %>%
    st_buffer(RIVER_BOUNDARY_BUFFER)
  
  lakes_dat <- lakes %>%
    filter(area >= MIN_LAKE_SIZE) %>%
    filter(st_intersects(., river_dat_boundary, sparse = FALSE))
  
  river_polygons_dat <- river_polygons %>%
    filter(st_intersects(., river_dat_boundary, sparse = FALSE))
  
  # Set up plot
  river_bbox <- st_bbox(river_dat)
  plot_xlim = c(river_bbox[["xmin"]] - BBOX_EXPANSION * (river_bbox[["xmax"]] - river_bbox[["xmin"]]), 
                river_bbox[["xmax"]] + BBOX_EXPANSION * (river_bbox[["xmax"]] - river_bbox[["xmin"]]))
  plot_ylim = c(river_bbox[["ymin"]] - BBOX_EXPANSION * (river_bbox[["ymax"]] - river_bbox[["ymin"]]), 
                river_bbox[["ymax"]] + BBOX_EXPANSION * (river_bbox[["ymax"]] - river_bbox[["ymin"]]))
  plot_aspect = (max(plot_ylim) - min(plot_ylim)) / (max(plot_xlim) - min(plot_xlim))
  
  output_width = round((max(plot_xlim) - min(plot_xlim)) / OUTPUT_SCALE, 0)
  output_height = round(output_width * plot_aspect, 0)
  
  png(filename = here(glue("outputs/21-environment-{s}.png")), 
      width = output_width, 
      height = output_height, 
      type = "cairo", 
      antialias = "none")
  
  par(mar = c(0, 0, 0, 0), 
      bg = OCEAN_COLOUR)
  
  plot(0, 0, 
       xlim = plot_xlim, 
       ylim = plot_ylim, 
       type = "n", 
       frame.plot = FALSE, 
       axes = FALSE, 
       xaxs = "i", 
       yaxs = "i", 
       asp = 1)
  
  # Plot land
  plot(st_geometry(coastline), 
       add = TRUE, 
       col = LAND_COLOUR, 
       border = COASTLINE_COLOUR, 
       lwd = COASTLINE_LWD)
  
  # Plot river network
  plot(river_dat["order"], 
       add = TRUE, 
       col = RIVER_COLOUR, 
       lwd = river_dat$plot_lwd)
  
  # River polygons
  plot(st_geometry(river_polygons_dat), 
       add = TRUE, 
       col = RIVER_POLYGON_COLOUR,
       border = RIVER_POLYGON_BORDER_COLOUR,
       lwd = RIVER_POLYGON_BORDER_LWD)
  
  # Plot lakes
  plot(st_geometry(lakes_dat), 
       add = TRUE, 
       col = LAKE_COLOUR,
       border = LAKE_BORDER_COLOUR, 
       lwd = LAKE_BORDER_LWD)
  
  dev.off()
}

# Plot 4 largest river networks
large_rivers <- river_network_sizes %>%
  top_n(n = 4, wt = n) %>%
  pull(source_nzreach)

lapply(X = large_rivers, FUN = plot_river)

# *****************************************************************************
