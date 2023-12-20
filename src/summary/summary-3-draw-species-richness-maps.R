# Create species richness maps of current & forecast insect species
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2021-06-06

# Develop for all insects based on draw-species-richness-maps-glm.R

require(terra)
require(dplyr)
require(stringr)
require(ggplot2)
require(tidyterra) # for richness maps of SpatRasters
source(file = "load_functions.R")

# Logical indicating whether to replace an overlap raster/map if one already 
# exists
replace <- TRUE

# Logical indicating whether to save overlap maps (i.e., images) to file
save_maps <- TRUE
file_ext <- "png"

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
  insects <- c("Papilio rumiko", "Papilio cresphontes")
}

# Remove insects from list that have an insufficient number of filtered 
# occurrence records and therefore should not be included in analyses. 
# (Technically, they shouldn't have distribution rasters, but some obsolete 
# files may still remain in the output/distributions folder)
exclude <- species_info$species[species_info$pa_csv == "no"]
insects <- insects[!insects %in% exclude]

# Make vector of compute-friendly insect names
nice_names <- insects %>%
  str_replace(pattern = " ", replacement = "_") %>%
  tolower()

# Loop over all climate models (including current)
# Read in all insect rasters
# Convert rasters to 0/1
# Extend all rasters to same extent
# Sum all rasters together
# Save raster to output/maps
# If save_maps is TRUE, create ggplot map and save to output/maps

# Will need to update raster values:
#   {0, 3}: 0
#   {4, 5}: 1
# Reclassification matrix, 
rcl <- matrix(data = c(0, 3, 0, 
                       3.1, 5, 1), # Bit of a cludge to get classify to work
              nrow = 2,
              byrow = TRUE)

# Iterate over each climate model, creating a Papilio species richness raster 
# (and map if appropriate) for each model (5 total)
for (clim_model in climate_models$name) {
  cat("Climate model: ", clim_model, "\n")
  output_basename <- paste0("output/richness/",
                            clim_model, 
                            "-richness")
  richness_ras_filename <- paste0(output_basename, ".rds")
  richness_map_filename <- paste0(output_basename, ".", file_ext)
  
  # Check to see if files exist and only replace if necessary
  if (!file.exists(richness_ras_filename) | replace) {
    overlap_files <- paste0("output/overlaps/",
                            nice_names, 
                            "-overlap-", 
                            clim_model, 
                            ".rds")
    # Vector of logicals to see if any overlap files are missing
    overlap_exists <- file.exists(overlap_files)
    # If some *are* missing, let user know
    if (!all(overlap_exists)) {
      # Notifications
      missing_overlap <- insects[!overlap_exists]
      missing_message <- paste(missing_overlap, sep = ", ")
      message("The following insects did not have overlap files on disk:")
      message(missing_message)
      # Remove any that are missing from the files vector so we do not try to 
      # read them in
      overlap_files <- overlap_files[overlap_exists]
    }
    # All rasters in a list
    raster_list <- lapply(X = overlap_files, FUN = readRDS)
    # Convert to 0/1
    binary_rasters <- lapply(X = raster_list,
                             FUN = terra::classify,
                             rcl = rcl,
                             include.lowest = TRUE)
    # Add them all together, starting with a SpatRasterCollection, which turns 
    # out to be easier to work with (don't have to worry about modifying extents 
    # beforehand)
    binary_coll <- terra::sprc(binary_rasters)
    richness_ras <- terra::mosaic(x = binary_coll, fun = "sum")
    saveRDS(object = richness_ras, file = richness_ras_filename)
    
    # Create map (image file) if necessary
    if (save_maps) {
      rich_map <- richness_map(r = richness_ras,
                                   predictor = clim_model)
      ggsave(filename = richness_map_filename,
             plot = rich_map)
    }
  }
}
