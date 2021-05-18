# Proof of concept for part of workflow: Papilio multicaudata & hosts
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-05-18

library(raster)
library(dplyr)
library(tidyr)
library(dismo) # requires sp, too
library(maptools) # for some QA plotting, may not be necessary

# TODO: Consider creating a list of lists, where each element in the top-level list 
# is a two-element list with elements
#   + type: character vector with "insect" or "host"
#   + obs:  data frame of observations
# This way, iterating over the list elements and doing all the same things 
# (filtering out NAs, selecting columns, running contemporary & forecast models)
# becomes easier; can also start attaching things like 
#   + background: data frame of pseudo-absence points

# Papilio multicaudata data downloaded 2021-05-18 from GBIF
# https://doi.org/10.15468/dl.v2puuk
# Remember, GBIF is a dick and provides TAB-separated files (not CSV)
insect_obs <- read.delim(file = "data/papilio_multicaudata.csv")

# Drop unused columns (most everything)
# TODO: if standard, convert to function
insect_obs <- insect_obs %>%
  dplyr::select(gbifID, species, decimalLongitude, decimalLatitude, 
         year, month, day) %>%
  drop_na() # Need complete records for all these


# Hosts
# Vauquelinia californica, Arizona rosewood
# Fraxinus anomala, single-leaf ash
# Prunus virginiana, chokecherry
# Prunus emarginata, bitter cherry
# Ptelea trifoliata, hoptree, 
# Platanus wrightii, Arizona sycamore
# WAIT (introduced to western US) Fraxinus pennsylvanica, green ash 

host_obs_list <- list()
# Starting with just two of the hosts
# Vauquelinia californica, Arizona rosewood
# https://doi.org/10.15468/dl.rfrynw
host_obs_list[[1]] <- read.delim(file = "data/vauquelinia_californica.csv")
host_obs_list[[1]] <- host_obs_list[[1]] %>%
  dplyr::select(gbifID, species, decimalLongitude, decimalLatitude, 
       year, month, day) %>%
  drop_na()

# Fraxinus anomala, single-leaf ash
# https://doi.org/10.15468/dl.tgfk9n
host_obs_list[[2]] <- read.delim(file = "data/fraxinus_anomala.csv")
host_obs_list[[2]] <- host_obs_list[[2]] %>%
  dplyr::select(gbifID, species, decimalLongitude, decimalLatitude, 
         year, month, day) %>%
  drop_na()


########################################
# Background points
# Get the geographic extent of the organism in question
insect_extent <- extent(x = c(floor(min(insect_obs$decimalLongitude)),
                              ceiling(max(insect_obs$decimalLongitude)),
                              floor(min(insect_obs$decimalLatitude)),
                              ceiling(max(insect_obs$decimalLatitude))))

# Check for bioclim data and download if it isn't there
if (!file.exists("data/wc2-5/bio1.bil")) {
  bioclim.data <- getData(name = "worldclim",
                          var = "bio",
                          res = 2.5,
                          path = "data/")
}

# Using the first bil file to create a raster to use as mask for sampling 
# background points
bil_file <- list.files(path = "data/wc2-5", 
                       pattern = ".bil$", 
                       full.names = TRUE)[1]
mask <- raster(bil_file)

# Randomly sample points (same number as our observed points)
insect_background <- randomPoints(mask = mask,    # Provides resolution of sampling points
                           n = nrow(insect_obs),  # Number of random points
                           ext = insect_extent,   # Spatially restricts sampling
                           extf = 1.25)           # Expands sampling a little bit


# QA/QC on background sampling

data("wrld_simpl")

# Plot the base map
plot(wrld_simpl, 
     xlim = insect_extent[c(1, 2)],
     ylim = insect_extent[c(3, 4)],
     axes = TRUE, 
     col = "grey95",
     main = "Presence and pseudo-absence points")

# Add the observations
points(x = insect_obs$decimalLongitude,
       y = insect_obs$decimalLatitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)

# Draw a box around the extent we used for randomPoints restriction
plot(insect_extent, 
     add = TRUE, 
     col = "red")

# Add the background points
points(insect_background,
       col = "grey30",
       pch = 3,
       cex = 0.5)

