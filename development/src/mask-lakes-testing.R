# Running some tests to see whether we can mask out the Great Lakes (or other
# inland water bodies) from our climate data layers, and thus from predicted 
# species' distributions
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-12-09

require(raster)
require(terra)
# Note: Using terra to do all the work here, but since the distribution rasters
# were saved with the raster package (as RasterLayers and not SpatRasters), need
# the raster package to read in the file.

# First, grab the predicted distribution for a species/SDM/climate model where 
# we encountered issues with the Great Lakes

genus <- "Ptelea"
species <- "trifoliata"
species_name <- paste0(genus, " ", species)
nice_name <- tolower(x = gsub(pattern = " ",
                              replacement = "_",
                              x = species_name))

model <- "maxent-notune"

predictor <- "ensemble_ssp370_2041"

# Get predicted distribution raster
distribution_file <- paste0("output/distributions/", nice_name, 
                            "-distribution-", model, "-", predictor, ".rds")
dist<- readRDS(file = distribution_file)
dist <- terra::rast(dist)

# Lakes layer from here:
# https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-lakes/
# This is the same source that Kass et al. 2022 (Sci Adv) used.

lakes <- terra::vect("development/data/lakes/ne_10m_lakes.shp")
lakes <- terra::project(lakes, crs(dist))
lakes

# Visualize the problem (green areas are predicted suitable for P. trifoliata
# in 2041 under the ssp370 climate model)
plot(dist)
plot(lakes, add = TRUE)
# Zoom in on Great Lakes
plot(dist, xlim = c(-90, -80), ylim = c(41, 47))
plot(lakes, add = TRUE)

# Ultimately, we want to mask out raster cells that fall completely within a
# lake and retain cells that contain any land area. Given the size of our raster
# cells (2.5 min resolution ~ 4.5 km), this will only mask out relatively large 
# water bodies, including the Great Lakes

# Generate values for each cell that represent the proportional area of that 
# cell that falls in a water body (ie, cell values range from 0-1)
lakes2 <- terra::rasterize(lakes, dist, cover = TRUE)

# Set cells that are >99% lake == 1, all other cells NA
lakesr <- terra::ifel(round(lakes2, 2) != 1, NA, 1)
lakesr
plot(lakesr, col = "blue")

# Mask the predicted species distribution by the new rasterized lake layer
dist_mask <- terra::mask(dist, lakesr, inverse = TRUE)

# Look at distribution layer masked by lakes
plot(dist_mask)
plot(lakes, add = T)
# Look more closely at the Great Lakes region
plot(dist_mask, xlim = c(-90, -80), ylim = c(41, 47))
plot(lakes, add = T)
