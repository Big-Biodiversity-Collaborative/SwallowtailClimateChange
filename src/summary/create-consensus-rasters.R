# Create consensus rasters (identifying the number of SDMs that predict 
# presence) for a set of species and climate scenarios
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-12-20

require(stringr)
require(raster)
require(terra)
# Note: Using terra to do all the work here, but since the distribution rasters
# were saved with the raster package (as RasterLayers and not SpatRasters), need
# the raster package to read in the file.

# Load insect-host file
ih <- read.csv("data/insect-host.csv")

# Load list of climate models
climate_models <- read.csv(file = "data/climate-models.csv")

# List of SDMs to include
sdms <- c("brt", "gam", "lasso", "maxent-tune", "rf")

# Logical indicating whether to create consensus rasters for all species 
# or just a subset of insects and their host plants
all_insects <- FALSE

# If not all insects, identify which insects (and their host plants) to include
insects <- c("Papilio rumiko", "Papilio cresphontes")

# Extract all species names
if (all_insects) {
  species <- unique(c(ih$insect, ih$host_accepted))
} else {
  plants <- ih$host_accepted[ih$insect %in% insects]
  species <- unique(c(insects, plants))
}
nice_names <- species %>%
  str_replace(pattern = " ", replacement = "_") %>%
  tolower()

# Identify files with predicted distributions
all_dist_files <- list.files("output/distributions/", 
                             pattern = paste(nice_names, collapse = "|"),
                             full.names = TRUE)
all_dist_files <- str_subset(all_dist_files, paste(sdms, collapse = "|"))

for (i in 1:length(species)) {
  
  spp_dist_files <- str_subset(all_dist_files, nice_names[i])
  
  if (length(spp_dist_files) == 0) {
    message("*** No distribution files for ", species[i], 
            ". Did not create consensus rasters.")
  } else {
    
    cat(paste0("Creating consensus rasters for ", species[i], ".\n"))
    
    for (clim_model in climate_models$name) {
      
      dist_files <- str_subset(spp_dist_files, clim_model)

      if (length(dist_files) < length(sdms)) {
        message("*** Missing at least one SDM distribution file for ", 
                species[i], ", ", clim_model, 
                ". Did not create consensus raster.")
      } else {
        dist_list <- list()
        
        for (j in 1:length(dist_files)) {
          dist_list[[j]] <- readRDS(dist_files[j])
          dist_list[[j]] <- terra::rast(dist_list[[j]])
        }
        consensus <- rast(dist_list)
        
        # Create a raster with cell values equal to the number of SDMs
        # predicting presence (or suitability)
        consensus <- sum(consensus)

        consensus_file <- paste0("output/consensus-rasters/",
                                 nice_names[i], 
                                 "-consensus-",
                                 clim_model,
                                 ".rds")
        saveRDS(object = consensus, 
                file = consensus_file)

      }
    }
  }
}

