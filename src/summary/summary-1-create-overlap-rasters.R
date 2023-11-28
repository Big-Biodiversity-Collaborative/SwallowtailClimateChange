# Create rasters identifying geographic overlap between each insect species and 
# its host plants
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2023-10-16

require(terra)
require(dplyr)
require(stringr)

# Create a raster for each insect, with the following classification:
# 0 = (Insect and hosts absent) Insect and all host plants predicted absent
# 1 = (1 host only) Insect predicted absent, only 1 host predicted present
# 2 = (2 or more hosts) Insect predicted absent, >= 2 hosts predicted present
# 3 = (Insect, no hosts) Insect predicted present, all hosts predicted absent
# 4 = (Insect, only 1 host) Insect and only 1 host predicted present
# 5 = (Insect, 2 or more hosts) Insect and >= 2 hosts predicted present

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

# Logical indicating whether to create overlap rasters for all species or just a 
# subset of insects
all_insects <- FALSE

# Extract species names
if (all_insects) {
  insects <- unique(ih$insect)
} else {
  # If not all insects, identify which insects to include
  insects <- c("Papilio rumiko")
}

# Remove insects from list that have an insufficient number of filtered 
# occurrence records and therefore should not be included in analyses. 
# (Technically, they shouldn't have distribution rasters, but some obsolete 
# files may still remain in the output/distributions folder)
exclude <- species_info$species[species_info$pa_csv == "no"]
insects <- insects[!insects %in% exclude]

# Loop through insect species to create an overlap raster for each climate model
for (i in 1:length(insects)) {

  # Find distribution rasters for the insect
  insect <- insects[i]
  insect_nice_name <- insect %>%
    str_replace(pattern = " ", replacement = "_") %>%
    tolower()
  
  insect_files <- list.files(path = "output/distributions", 
                             pattern = insect_nice_name,
                             full.names = TRUE)
  # Won't need this eventually, but for now, remove any files from list that 
  # are distributions for particular SDMs
  sdms <- paste0(c("brt", "gam", "glm", "lasso", "maxent", "rf"), collapse = "|")
  insect_files <- insect_files[!grepl(sdms, insect_files)]
  
  # Abort if missing one or more distribution rasters for the insect
  if (length(insect_files) < 5) {
    
    message("*** Missing one or more distribution rasters for ", insect, 
            ". Did not create overlap rasters.")
  
  } else {
    
    # Identify host plants and extract distribution raster files
    plants <- ih$host_accepted[ih$insect == insect]
    
    # Remove plants from list that have an insufficient number of filtered 
    # occurrence records and therefore should not be included in analyses. 
    # (Technically, they shouldn't have distribution rasters, but some obsolete 
    # files may still remain in the output/distributions folder)
    plants <- plants[!plants %in% exclude]
    
    plant_nice_names <- plants %>%
      str_replace(pattern = " ", replacement = "_") %>%
      tolower()
    
    plant_files <- NULL
    
    for (j in 1:length(plants)) {
      
      plant_files_new <- list.files(path = "output/distributions", 
                                    pattern = plant_nice_names[j],
                                    full.names = TRUE)
      plant_files_new <- plant_files_new[!grepl(sdms, plant_files_new)]
      
      # Print message if distribution rasters are missing for a host plant
      if (length(plant_files_new) < 5) {
        cat(paste0("Missing one or more distribution rasters for ", 
                   plants[j], " (host plant for ", insect, ").\n"))
      } else {
        plant_files <- c(plant_files, plant_files_new)
      }
    }
    
    for (clim_model in climate_models$name) {
      
      dist_files <- str_subset(c(insect_files, plant_files), clim_model)
      
      # Abort if distribution rasters missing for all host plants
      if (length(dist_files) == 1) {
        
        message("*** Missing distribution rasters for all hosts associated with ",
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
        
        # Load distribution raster for insect
        insect_dist <- readRDS(dist_files[1])

        # Load distribution rasters for host plants, crop rasters to match 
        # extent of the insect
        plant_list <- list()
        for (j in 1:(length(dist_files) - 1)) {
          plant_list[[j]] <- readRDS(dist_files[j + 1])
          plant_list[[j]] <- terra::crop(plant_list[[j]], insect_dist)
        }
        
        # Calculate spatial extent across all plant rasters
        all_plants_ext <- terra::ext(terra::sprc(plant_list))
        
        # Extend plant rasters where needed so they can be combined into a
        # single SpatRaster
        for (j in 1:length(plant_list)) {
          plant_list[[j]] <- terra::extend(plant_list[[j]], all_plants_ext)
        }
        host_dist <- terra::rast(plant_list)
        
        # Calculate the number of host plants present in each cell
        host_dist <- sum(host_dist, na.rm = TRUE)

        # For insect, make cell values = 3 where present, zero otherwise.
        insect_dist[insect_dist == 1] <- 3
        
        # For hosts, make cell values = 1 where 1 one host plant is present, 2 
        # where 2 or more hosts present, zero otherwise.
        host_dist[host_dist > 1] <- 2
        
        # Combine insect and host distributions 
        
        # Individual plant distributions have already been cropped by the insect 
        # distribution, but it's possible that the resulting host distribution 
        # is smaller than the insect distribution.  If so, we'll need to extend 
        # it.
        host_dist <- terra::extend(x = host_dist, y = insect_dist)
        
        # Mask host distribution so it has NA values wherever the insect
        # distribution is NA
        host_dist <- terra::mask(x = host_dist, mask = insect_dist)
        
        # Calculate overlap values (0 to 5)
        overlap <- terra::rast(list(insect_dist, host_dist))
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
                                    horizontal_legend = TRUE,
                                    generic_legend = FALSE,
                                    boundaries = TRUE,
                                    obs_points = FALSE,
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
