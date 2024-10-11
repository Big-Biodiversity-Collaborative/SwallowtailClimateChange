# Create images to illustrate changes in individual species' suitability in 
#   areas designated as richness hotspots
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2024-10-04

require(terra)
require(dplyr)
source(file = "load_functions.R")


# Create binary raster of contemporary hotspots (areas suitable for >= 4 
# species).
# For each species of Papilio:
#   + Create binary raster of areas suitable for insect and at least one host 
#     for contemporary climate
#   + Do intersection with hotspot binary raster (contemporary ∩ hotspot)
#   + For each forecast climate model:
#     + Create binary raster of areas suitable for insect and at least one host 
#       for forecast climate model
#     + Do intersection of forecast binary raster with (contemporary ∩ hotspot) 
#       raster, which should take two states of interest:
#       + Suitable in hotspot for contemporary and forecast climate model
#       + Suitable in hotspot for contemporary but not forecast climate model
#     + Write raster as map to file. Consider including outline of contemporary 
#       hotspots in each output map (some will ultimately be just an outline, 
#       if a species does not have *any* suitable areas in contemporary 
#       hotspots).
