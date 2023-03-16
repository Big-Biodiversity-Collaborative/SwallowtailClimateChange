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

# Logical indicating whether to replace an overlap raster if one already exists
replace <- TRUE

# Logical indicating whether to save overlap maps (i.e., images) to file
save_maps <- TRUE
file_ext <- "png"
# Load up the functions (really just needed to load the overlap_map function)
source(file = "load_functions.R")

# Load insect-host file
ih <- read.csv("data/insect-host.csv")

# Load information about data availability for each species
species_info <- read.csv("data/gbif-pa-summary.csv")

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
  # insects <- c("Papilio rumiko", "Papilio cresphontes")
  insects <- "Papilio troilus"
}

# Remove insects from list that have an insufficient number of filtered 
# occurrence records and therefore should not be included in analyses. 
# (Technically, they shouldn't have consensus rasters, but some obsolete 
# files may still remain in the output/consensus-rasters folder)
exclude <- species_info$species[species_info$pa_csv == "no"]
insects <- insects[!insects %in% exclude]

# Loop through insect species to create an overlap raster for each climate model
for (i in 1:length(insects)) {
  
  # Find consensus rasters for the insect
  insect <- insects[i]
  insect_nice_name <- insect %>%
    str_replace(pattern = " ", replacement = "_") %>%
    tolower()
  
  insect_files <- list.files("output/consensus-rasters/", 
                             pattern = insect_nice_name,
                             full.names = TRUE)
  
  # Abort if missing one or more consensus rasters for the insect
  if (length(insect_files) < 5) {
    message("*** Missing one or more consensus rasters for ", insect, 
            ". Did not create overlap rasters.")
  } else {

    # Identify host plants and extract consensus raster files
    plants <- ih$host_accepted[ih$insect == insect]
    plant_nice_names <- plants %>%
      str_replace(pattern = " ", replacement = "_") %>%
      tolower()
    
    # Remove plants from list that have an insufficient number of filtered 
    # occurrence records and therefore should not be included in analyses. 
    # (Technically, they shouldn't have consensus rasters, but some obsolete 
    # files may still remain in the output/consensus-rasters folder)
    plants <- plants[!plants %in% exclude]
    
    plant_files <- NULL
    for (j in 1:length(plants)) {
      plant_files_new <- list.files("output/consensus-rasters/", 
                                    pattern = plant_nice_names[j],
                                    full.names = TRUE) 
      # Print message if consensus rasters are missing for a host plant
      if (length(plant_files_new) == 0) {
        cat(paste0("Missing consensus rasters for ", 
                   plants[j], " (host plant for ", insect, ").\n"))
      } else {
        plant_files <- c(plant_files, plant_files_new)
      }
    }
    
    for (clim_model in climate_models$name) {
      
      dist_files <- str_subset(c(insect_files, plant_files), clim_model)
      
      # Abort if consensus rasters missing for all host plants
      if (length(dist_files) == 1) {
        message("*** Missing consensus rasters for all hosts associated with ",
                insect, ", ", clim_model, ". Did not create overlap raster.")
      } else {
        
        overlap_file <- paste0("output/overlaps/",
                               insect_nice_name, 
                               "-overlap-",
                               clim_model,
                               ".rds")
        
        # Skip rest of loop if file exists and we don't want to replace it
        if (file.exists(overlap_file) & replace == FALSE) next
        
        # Update on progress
        cat(paste0("Creating overlap raster for ", insect, 
                   ", ", clim_model, ".\n"))
        
        # Load consensus raster for insect and create a raster with the species'
        # predicted distribution (ie, convert consensus raster to binary values 
        # based on the number of SDMs that predict presence)
        insect_dist <- readRDS(dist_files[1])
        insect_dist <- as.numeric(insect_dist >= min_sdms)
        
        # Load consensus rasters for host plants, crop rasters to match extent 
        # of the insect, and convert to binary values. 
        plant_list <- list()
        for (j in 1:(length(dist_files) - 1)) {
          plant_list[[j]] <- readRDS(dist_files[j + 1])
          plant_list[[j]] <- as.numeric(plant_list[[j]] >= min_sdms)
          plant_list[[j]] <- terra::crop(plant_list[[j]], insect_dist)
        }
        
        # Calculate spatial extent across all plant rasters
        all_plants_ext <- ext(terra::sprc(plant_list))
        
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
        # First create 3-column matrix, with values in columns 1-2 
        # representing a range of cell values you want to reclassify, and a 
        # value in the 3rd column containing the new value for those cells.
        rcl <- rbind(c(0, 0, 0), c(1, 100, 2))
        host_dist <- terra::classify(x = host_dist, 
                                     rcl = rcl,
                                     right = NA)
        
        # Combine insect and host distributions 
        
        # Individual plant distributions have already been cropped by the insect 
        # distribution, but it's possible that the resulting host distribution 
        # is smaller than the insect distribution.  If so, we'll need to extend 
        # it.
        host_dist <- terra::extend(host_dist, insect_dist)
        
        # Mask host distribution so it has NA values wherever the insect
        # distribution is NA
        host_dist <- terra::mask(host_dist, insect_dist)
        
        # Calculate overlap values (0, 1, 2, or 3)
        overlap <- rast(list(insect_dist, host_dist))
        overlap <- sum(overlap)
        
        # Save overlap raster to file
        saveRDS(object = overlap, 
                file = overlap_file)
        
        # Save map to file
        if (save_maps) {
          
          cat(paste0("Saving overlap map for ", insect, 
                     ", ", clim_model, ".\n"))
          
          map_object <- overlap_map(species_name = insects[i],
                                    overlap_raster = overlap,
                                    clim_model = clim_model,
                                    include_legend = TRUE,
                                    horizontal_legend = FALSE,
                                    generic_legend = FALSE,
                                    title_scenarioyear = TRUE)
          
          map_file <- paste0("output/maps/",
                             insect_nice_name, 
                             "-overlap-",
                             clim_model,
                             ".", file_ext)
          
          ggsave(filename = map_file,
                 plot = map_object, 
                 width = 6, 
                 height = 6,
                 units = "in")
        }
      }
    } 
  }
} 
