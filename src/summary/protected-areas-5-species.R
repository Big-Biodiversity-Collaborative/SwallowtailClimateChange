# Do species accumulation curves for four types of protected areas
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-12-01

require(vegan)
require(dplyr)
require(terra)

# Would like to build species accumulation curves (SAC), to compare (estimated) 
# richness between current climate and forecast conditions. Essentially, would 
# like a plot for each forecast climate model and each of the four protection 
# levels, with a curve for current richness and a curve for the forecast 
# richness. 

# There is some question about what the sample unit is for the plot - it could 
# be grid cells, which is probably the "easiest" as our data are currently in 
# that form. I wonder though if square kilometers (area) would be a little 
# easier to understand.

# In order to take advantage of the vegan package for this, we will want a 
# data frame that looks like:
#         Sp. 1  Sp. 2. ... Sp. T
# Site 1      0      1  ...     0
# Site 2      1      1  ...     0
#   ...     ...    ...  ...   ...
# Site F      0      0  ...     1

# A little wary, as F is going to be not small...
# Let's just try a sample dataset
species <- paste0("Sp", c(1:15))
counts <- lapply(X = 1:15, FUN = function(x) {
  sample(x = c(0,1), size = 60000, replace = TRUE, prob = c(0.9, 0.1))
  })
names(counts) <- species
# This gets us to that desired data shape:
df <- dplyr::bind_cols(counts)

# Do the species accumulation curve calculations
sac <- vegan::specaccum(df)
# Plot the curve (really a square)
plot(sac)
# Zoom in to part of the curve that is interesting
plot(sac, xlim = c(0, 200))
# OK, seems pretty good. The sac function might have taken a couple seconds, 
# but did not crash.

# Read in protected areas info (may take 3 minutes or so)
shpfile_path <- "data/protected-areas/protected-areas-categorized.shp"
message("Reading protected areas shapefile (may take a few minutes)...")
protected_areas <- terra::vect(shpfile_path)
message("Protected areas shapefile read.")

# Read in insect information
ih <- read.csv(file = "data/insect-host.csv")

# Read in climate models data frame
climate_models <- read.csv(file = "data/climate-models.csv")

# Need to create a SpatRaster of all current suitability rasters (0, 1)
# May be able to avoid species-level iteration, if we can create a vector of 
# filenames to pass to terra::rast()
# nice_names <- tolower(gsub(pattern = " ",
#                            replacement = "_",
#                            x = unique(ih$insect)))
# overlap_files <- paste0("output/overlaps/",
#                           nice_names, "-overlap-current.rds")
overlap_files <- list.files(path = "output/overlaps",
                            pattern = "*-overlap-current.rds",
                            full.names = TRUE)

overlap_rasters <- lapply(X = overlap_files,
                          FUN = readRDS)

# After this, the SpatRaster Collection need to be (re)classified to 0,1 values  
overlap_rasters <- terra::sprc(overlap_rasters)

# ...some more magic here...

# TODO: Need some what of doing stuff below separately on each type of 
# protected area?

# Create a data frame of species presence/absence, one row per grid cell

# Run vegan::specaccum on that data frame. Consider extracting stuff in a tidy 
# way...

# Do much of the above, for each of the forecast climate models

# Now plot curves