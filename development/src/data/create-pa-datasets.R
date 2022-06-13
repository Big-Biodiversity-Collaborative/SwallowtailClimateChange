# Generate presence-absence dataset for each species, to be used in any SDM
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-06-13

require(raster)
require(dplyr)  # load *after* raster for easier use of select
require(dismo)  # background point sampling

# Load up the functions from the functions folder
source(file = "load_functions.R")

replace <- FALSE
verbose <- TRUE

# Read in gbif-reconcile
species_list <- read.csv("development/data/gbif-reconcile-4spp.csv")

# Using the first bil file to create a raster to use as mask for sampling 
# background points
bil_file <- list.files(path = "data/wc2-1", 
                       pattern = ".tif$", 
                       full.names = TRUE)[1]
mask <- raster(bil_file)
# Do not need this anymore
rm(bil_file)

for (species in species_list$accepted_name) {
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species))
  filename <- paste0("development/data/presence-absence/", nice_name, "-pa.csv") 

  # Only proceed if file doesn't exist or we want to replace existing files
  if (!file.exists(filename) | replace) {
    if (verbose) {
      message(paste0("\n****  Beginning process for ", species, "  ****"))
    }    
  }
  
  # Load in observation data
  obs_file <- paste0("data/gbif/",
                     nice_name,
                     "-gbif.csv")
  if (!file.exists(obs_file)) {
    unzip(zipfile = "data/gbif.zip")
  }
  obs <- read.csv(file = obs_file)
  
  # Get the geographic extent of the observation data
  obs_extent <- get_extent(data = obs)  
  
  # Use random sampling to generate pseudo-absence points
  # mask:  Provides resolution of sampling points
  # n:     Number of random points
  # ext:   Spatially restricts sampling
  # extf:  Expands sampling a little bit
  absence <- dismo::randomPoints(mask = mask,  
                                 n = 5000, # Will want to make this bigger
                                 ext = obs_extent, 
                                 extf = 1.25)
  
  # Convert to data frame
  absence <- as.data.frame(absence) 

  # Grab worldclim data to use as predictors
  predictors <- raster::stack(list.files(path = "data/wc2-1",
                                         pattern = ".tif$",
                                         full.names = TRUE))
  
  # Only need geo coordinates, so extract those (in x, y order)
  presence <- obs %>%
    dplyr::select(longitude, latitude)
  
  # Extract predictor values for observed and background points
  predictors_presence <- raster::extract(x = predictors, y = presence)
  predictors_absence <- raster::extract(x = predictors, y = absence)
  
  # Make a vector of appropriate length with 0/1 values for 
  # (pseudo)absence/presence
  pa_data <- c(rep(x = 1, times = nrow(presence)), 
               rep(x = 0, times = nrow(absence)))    
  
  # Create a vector of folds for easier splitting into testing/training
  num_folds <- 5 # for 20/80 split
  fold <- c(rep(x = 1:num_folds, length.out = nrow(presence)),
            rep(x = 1:num_folds, length.out = nrow(absence)))
  
  # Combine our presence / absence and fold vectors with environmental data we 
  # extracted
  full_data <- data.frame(cbind(pa = pa_data,
                                fold = fold,
                                rbind(predictors_presence, predictors_absence)))
  write.csv(x = full_data,
            file = filename,
            row.names = FALSE)
  
  if (verbose) {
    message(paste0("\n****  ",nrow(presence), " gbif records and ", 
                   nrow(absence), " pseudo-absence records written to ",
                   filename, "  ****"))
  }
}
