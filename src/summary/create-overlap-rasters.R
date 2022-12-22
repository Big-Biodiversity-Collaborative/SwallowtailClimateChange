# Create rasters identifying geographic overlap (based on consensus rasters) 
# between each insect species and its host plants
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-12-22

require(terra)
require(dplyr)
require(stringr)

# Want to create a raster for each insect, with the following classification:
# 0 = Insect and all host plants predicted absent
# 1 = Insect predicted present, but all host plants predicted absent
# 2 = At least one host plant predicted present, but insect predicted absent
# 3 = Insect and at least one host plant predicted present

# TODO: save overlap maps (eg, pngs) to file?

# Load insect-host file
ih <- read.csv("data/insect-host.csv")

# Load list of climate models
climate_models <- read.csv(file = "data/climate-models.csv")

# Set the threshold number of SDMs that predict presence/suitability for a cell
# to be included in a consensus distribution for a given species
min_sdms <- 4

# Logical indicating whether to create overlap rasters for all species or just a 
# subset of insects
all_insects <- FALSE

# Extract species names
if (all_insects) {
  insects <- unique(ih$insect)
} else {
  # If not all insects, identify which insects to include
  insects <- c("Papilio rumiko", "Papilio cresphontes")
}

# Loop through insect species to create an overlap raster for each climate model
for (i in 1:length(insects)) {
  
  # Identify host plants
  insect <- insects[i]
  insect_nice_name <- insect %>%
    str_replace(pattern = " ", replacement = "_") %>%
    tolower()
  
  insect_files <- list.files("output/consensus-distributions/", 
                             pattern = paste(insect_nice_name, collapse = "|"),
                             full.names = TRUE)
  
  # Abort if there's no consensus raster for the insect
  if (length(insect_files) == 0) {
    message("*** No consensus rasters for ", insect, 
            ". Did not create overlap rasters.")
  } else {
    
    cat(paste0("Creating overlap rasters for ", insect, ".\n"))
  
    # Identify host plants and extract consensus raster files
    plants <- ih$host_accepted[ih$insect == insect]
    plant_nice_names <- plants %>%
      str_replace(pattern = " ", replacement = "_") %>%
      tolower()
    plant_files <- list.files("output/consensus-distributions/", 
                               pattern = paste(plant_nice_names, collapse = "|"),
                               full.names = TRUE)    
    
    for (clim_model in climate_models$name) {
      
      dist_files <- str_subset(c(insect_files, plant_files), clim_model)
      
      # Abort if consensus rasters missing for all host plants
      if (length(dist_files) == 1) {
        message("*** Missing consensus rasters for all hosts associated with ",
                insect, ", ", clim_model, 
                " climate scenario. Did not create overlap raster.")
      } else {
        
        # Print message (but continue with process) if missing a consensus 
        # raster for one or more (but not all) host plants
        if (length(dist_files) < length(c(insect, plants))) {
          cat(paste0("Missing consensus raster for at least 1 host associated with ",
                     insect, ", ", clim_model, ".\n"))
        } 
        
        # Load consensus raster for insect and convert to binary values (1/0) 
        # based on the threshold number of SDMs that predict presence (specified
        # at top of script)
        insect_dist <- readRDS(dist_files[1])
        insect_dist <- as.numeric(insect_dist >= min_sdms)

        # Load consensus rasters for host plants, crop rasters to match extent 
        # of the insect, and convert to binary values. 
        plant_list <- list()
        for (j in 2:length(dist_files)) {
          plant_list[[j-1]] <- readRDS(dist_files[j])
          plant_list[[j-1]] <- as.numeric(plant_list[[j-1]] >= min_sdms)
          plant_list[[j-1]] <- terra::crop(plant_list[[j-1]], insect_dist)
        }
        
        # Calculate spatial extent across all plant rasters
        all_plants_ext <- ext(sprc(plant_list))
        
        # Extend plant rasters where needed so they can be combined into a
        # single SpatRaster
        for (j in 1:length(plant_list)) {
          plant_list[[j]] <- terra::extend(plant_list[[j]], all_plants_ext)
        }
        host_dist <- terra::rast(plant_list)
        
        # Calculate the number of host plants present in each cell
        host_dist <- sum(host_dist, na.rm = TRUE)
        
        # Make cell values = 2 where at least one host plant is present, zero 
        # otherwise
        host_dist <- terra::classify(host_dist, 
                                     cbind(c(0, 1), c(0, 5), c(0, 2)), 
                                     right = NA)
        
        # Combine insect and host distributions 
        
        # Plant distributions have already been cropped by the insect 
        # distribution, but it's possible that the resulting host distribution 
        # is smaller than the insect distribution.  If so, need to extend it.
        host_dist <- terra::extend(host_dist, insect_dist)
        
        # Mask host distribution so it has NA values wherever the insect
        # distribution is NA
        host_dist <- terra::mask(host_dist, insect_dist)
        
        # Calculate overlap values (0, 1, 2, or 3)
        overlap <- rast(list(insect_dist, host_dist))
        overlap <- sum(overlap)
        
        # Save overlap raster to file
        overlap_file <- paste0("output/overlaps/",
                               insect_nice_name, 
                               "-overlap-",
                               clim_model,
                               ".rds")
        saveRDS(object = overlap, 
                file = overlap_file)
        
      }
    } 
  }
}      


