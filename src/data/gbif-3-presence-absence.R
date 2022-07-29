# Generate presence-absence dataset for each species, to be used in any SDM
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-06-13

require(terra)
require(dplyr)

# Load up the functions from the functions folder
source(file = "load_functions.R")

replace <- FALSE
verbose <- TRUE

# Read in gbif-reconcile
species_list <- read.csv("data/gbif-reconcile.csv")

for (species in species_list$accepted_name) {
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species))
  filename <- paste0("data/gbif/presence-absence/", nice_name, "-pa.csv") 

  # Only proceed if file doesn't exist or we want to replace existing files
  if (file.exists(filename) & replace == FALSE) next
    
    if (verbose) {
      message(paste0("\n****  Beginning process for ", species, "  ****"))
    }    

  # Load observation data
  obs_file <- paste0("data/gbif/filtered/",
                     nice_name,
                     "-gbif.csv")
  if (!file.exists(obs_file)) {
    unzip(zipfile = "data/gbif-filtered.zip")
  }

  # There's a chance that a file might not be present, even after unzipping; 
  # note this and move on.
  if (!file.exists(obs_file)) {
    message(paste0("No filtered data for ", species, " on disk"))
  } else {
    obs <- read.csv(file = obs_file)

    if (nrow(obs) == 0) {
      message(paste0("Filtered data for ", species, " has zero rows"))
    } else {
      # Retain just the geographic coordinates
      presence <- obs %>%
        dplyr::select(longitude, latitude)
      
      # Get the geographic extent of the observation data
      obs_extent <- get_extent(data = presence)
      
      # Grab climate data to use as predictors
      predictors <- terra::rast(list.files(path = "data/wc2-1",
                                           pattern = ".tif$",
                                           full.names = TRUE))  
      
      # Extract predictor values for observed points
      presence <- terra::extract(x = predictors, 
                                 y = presence, 
                                 xy = TRUE)
      
      # Rearrange columns
      presence <- presence %>%
        dplyr::select(-ID) %>%
        dplyr::relocate(c(x,y), .before = bio1)
      
      # Use random sampling to generate pseudo-absence points and extract predictor 
      # values
      absence <- terra::spatSample(x = predictors,  
                                   size = 20000,
                                   method = "random",
                                   na.rm = TRUE,
                                   values = TRUE,
                                   xy = TRUE,
                                   ext = obs_extent * 1.25)
      # Note: if extent is small, function may not be able to generate the indicated 
      # number of points (size, here 20000) because the default is to select cells
      # without replacement. If this happens, R will return a warning:
      # [spatSample] fewer cells returned than requested
      
      # Make a vector of appropriate length with 0/1 values for 
      # (pseudo)absence/presence
      pa_data <- c(rep(x = 1, times = nrow(presence)), 
                   rep(x = 0, times = nrow(absence)))  
      
      # Create a vector of folds for easier splitting into testing/training
      num_folds <- 5 # for 20/80 split
      fold <- c(rep(x = 1:num_folds, length.out = nrow(presence)),
                rep(x = 1:num_folds, length.out = nrow(absence)))
      
      # Combine our presence / absence and fold vectors with environmental data
      full_data <- data.frame(cbind(pa = pa_data,
                                    fold = fold,
                                    rbind(presence, absence)))
      write.csv(x = full_data,
                file = filename,
                row.names = FALSE)
      
      if (verbose) {
        message(paste0("****  ",nrow(presence), " gbif records and ", 
                       nrow(absence), " pseudo-absence records written to ",
                       filename, "  ****"))
      }
    }
  }
}

# Create list of all those files we just created so we can write them to a zip
# archive
pa_files <- list.files(path = "data/gbif/presence-absence", 
                       pattern = "*-pa.csv",
                       full.names = TRUE)
zipfile <- "data/gbif-pa.zip"
if (file.exists(zipfile)) {
  invisible(file.remove(zipfile))
}
zip(zipfile = zipfile,
    files = pa_files)
 