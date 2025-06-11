# Create insect species richness rasters of current & forecast models
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2021-06-06

require(terra)
require(dplyr)
require(stringr)
require(ggplot2)
source(file = "load_functions.R")

# Two versions of rasters will be created:
# 1. Based on overlaps of insect and host (where predicted "suitable" cells 
#    require not only presence of the insect, but also the presence of at least
#    one host plant). Output files will have the suffix "ov".
# 2. Based on insect predicted distribution alone (no plant information). 
#    Output files will have the suffix "io".

# Logical indicating whether to replace an overlap raster if one already exists
replace <- TRUE

# Load insect-host file
ih <- read.csv("data/insect-host.csv")

# Load information about data availability for each species
species_info <- read.csv("data/gbif-pa-summary.csv")

# Load list of climate models
climate_models <- read.csv(file = "data/climate-models.csv")

# Logical indicating whether to include all insects in richness calculations or 
# just a subset of insects
all_insects <- TRUE

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

# Will need to update raster values
# For rasters considering only areas where insect overlaps with at least one 
# plant species (inclusive)
#   [0, 3]: 0
#   [4, 5]: 1
# Reclassification matrix, for overlap rasters
ov_rcl <- matrix(data = c(0, 3, 0,
                          4, 5, 1),
              nrow = 2,
              byrow = TRUE)

# For rasters only considering insects
#  [0, 2]: 0
#  [3, 5]: 1
# Reclassification matrix, for insect-only rasters
io_rcl <- matrix(data = c(0, 2, 0,
                          3, 5, 1),
              nrow = 2,
              byrow = TRUE)

# Loop over all climate models (including current)
# Read in all insect rasters
# Convert rasters to 0/1
# ~Extend all rasters to same extent~ No need - handled by terra.
# Sum all rasters together
# Save raster to output/richness

# Iterate over each climate model, creating a raster of Papilio species 
# richness for each model (5 total)
for (clim_model in climate_models$name) {
  cat("Climate model: ", clim_model, "\n")
  output_basename <- paste0("output/richness/",
                            clim_model, 
                            "-richness")
  # Output files for overlap richness (i.e. those including plants)
  ov_richness_ras_filename <- paste0(output_basename, "-ov.rds")
  # Output files for insect only richness (not considering plants)
  io_richness_ras_filename <- paste0(output_basename, "-io.rds")

  # Check to see if files exist and only replace if necessary
  if (!base::all(file.exists(ov_richness_ras_filename), 
                 file.exists(io_richness_ras_filename)) | 
      replace) {
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
    # Overlap versions
    # Convert to 0/1
    ov_binary_rasters <- lapply(X = raster_list,
                                FUN = terra::classify,
                                rcl = ov_rcl,
                                right = NA)
    # Add them all together, starting with a SpatRasterCollection, which turns 
    # out to be easier to work with (don't have to worry about modifying extents 
    # beforehand)
    ov_binary_coll <- terra::sprc(ov_binary_rasters)
    ov_richness_ras <- terra::mosaic(x = ov_binary_coll, fun = "sum")
    saveRDS(object = ov_richness_ras, file = ov_richness_ras_filename)

    # Insect only version
    # Convert to 0/1
    io_binary_rasters <- lapply(X = raster_list,
                                FUN = terra::classify,
                                rcl = io_rcl,
                                right = NA)
    # Add them all together
    io_binary_coll <- terra::sprc(io_binary_rasters)
    io_richness_ras <- terra::mosaic(x = io_binary_coll, fun = "sum")
    # Write the raster to an rds file
    saveRDS(object = io_richness_ras, file = io_richness_ras_filename)

    # If this is not current climate, do comparison with contemporary richness 
    # to create a delta richness map
    if (clim_model != "current") {
      ov_current_file <- "output/richness/current-richness-ov.rds"
      io_current_file <- "output/richness/current-richness-io.rds"
      # Only proceed if both current richness files are available
      if (all(file.exists(ov_current_file), file.exists(io_current_file))){
        # Destination files
        delta_basename <- paste0("output/richness/",
                                 clim_model, 
                                 "-delta-richness")
        # Output for overlap deltas
        ov_delta_ras_filename <- paste0(delta_basename, "-ov.rds")
        # Output for insect-only deltas
        io_delta_ras_filename <- paste0(delta_basename, "-io.rds")

        # Overlap version
        # Load contemporary richness raster
        ov_current_richness <- readRDS(file = ov_current_file)
        # Create a SpatRasterCollection from raster list, changing the current 
        # raster to negative values so we can use the sum function in 
        # terra::mosaic, i.e. Forecast richness - Current richness.
        ov_delta_coll <- terra::sprc(list((-1) * ov_current_richness, ov_richness_ras))
        # Calculate delta of rasters
        ov_delta_ras <- terra::mosaic(x = ov_delta_coll, fun = "sum")
        # Save delta raster
        saveRDS(object = ov_delta_ras, file = ov_delta_ras_filename)

        # Insect only version
        # Load contemporary richness raster
        io_current_richness <- readRDS(file = io_current_file)
        # Create a SpatRasterCollection from raster list, changing the current 
        # raster to negative values so we can use the sum function in 
        # terra::mosaic, i.e. Forecast richness - Current richness.
        io_delta_coll <- terra::sprc(list((-1) * io_current_richness, io_richness_ras))
        # Calculate delta of rasters
        io_delta_ras <- terra::mosaic(x = io_delta_coll, fun = "sum")
        # Save delta raster
        saveRDS(object = io_delta_ras, file = io_delta_ras_filename)
      } else {
        message("No current richness rasters on disk; deltas not created.")
      }
    } # End conditional for forecast model comparison with current
  } # End conditional for file exists / replace
} # End iteration over all climate models
