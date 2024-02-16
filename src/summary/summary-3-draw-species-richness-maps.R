# Create insect species richness rasters & maps of current & forecast models
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2021-06-06

require(terra)
require(dplyr)
require(stringr)
require(ggplot2)
source(file = "load_functions.R")

# Two versions of rasters (and maps) will be created:
# 1. Based on overlaps of insect and host (where predicted "suitable" cells 
#    require not only presence of the insect, but also the presence of at least
#    one host plant). Output files will have the suffix "ov".
# 2. Based on insect predicted distribution alone (no plant information). 
#    Output files will have the suffix "io".

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
# For maps considering only areas where insect overlaps with at least one plant
# species (inclusive)
#   [0, 3]: 0
#   [4, 5]: 1
# Reclassification matrix, for overlap maps
ov_rcl <- matrix(data = c(0, 3, 0,
                          4, 5, 1),
              nrow = 2,
              byrow = TRUE)

# For maps only considering insects
#  [0, 2]: 0
#  [3, 5]: 1
# Reclassification matrix, for insect-only maps
io_rcl <- matrix(data = c(0, 2, 0,
                          3, 5, 1),
              nrow = 2,
              byrow = TRUE)

# Loop over all climate models (including current)
# Read in all insect rasters
# Convert rasters to 0/1
# ~Extend all rasters to same extent~ No need - handled by terra.
# Sum all rasters together
# Save raster to output/maps
# If save_maps is TRUE, create ggplot map and save to output/maps

# Iterate over each climate model, creating a raster of Papilio species 
# richness (and map if appropriate) for each model (5 total)
for (clim_model in climate_models$name) {
  cat("Climate model: ", clim_model, "\n")
  output_basename <- paste0("output/richness/",
                            clim_model, 
                            "-richness")
  # Output files for overlap richness (i.e. those including plants)
  ov_richness_ras_filename <- paste0(output_basename, "-ov.rds")
  ov_richness_map_filename <- paste0(output_basename, "-ov.", file_ext)
  # Output files for insect only richness (not considering plants)
  io_richness_ras_filename <- paste0(output_basename, "-io.rds")
  io_richness_map_filename <- paste0(output_basename, "-io.", file_ext)
  
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
    saveRDS(object = io_richness_ras, file = io_richness_ras_filename)

    # Create map (image file) if necessary
    if (save_maps) {
      # Base title for plots
      if (clim_model == "current") {
        base_title <- "Current"
      } else {
        # Want to make human-readable version of the scenario + year
        # ensemble_ssp245_2041
        clim_model_split <- unlist(strsplit(x = clim_model, split = "_"))
        ssp <- substr(x = clim_model_split[2], start = 4, stop = 4)
        # Ugly, but transformation to numeric drops ".0" from integer values
        # e.g. SSP3-7.0 becomes SSP3-7
        rcp <- paste0(substr(x = clim_model_split[2], start = 5, stop = 5),
                      ".",
                      substr(x = clim_model_split[2], start = 6, stop = 6))
        scenario <- paste0("SSP", ssp, "-", rcp)
        yr <- clim_model_split[3]
        base_title <- paste0(scenario, ", ", yr)
      }
      
      # Overlap version
      ov_rich_map <- richness_map(r = ov_richness_ras,
                                  predictor = clim_model,
                                  direction = -1,
                                  plot_title = paste0(base_title,
                                                      ", overlaps with hosts"))
      ggsave(filename = ov_richness_map_filename,
             plot = ov_rich_map)
      # Insect-only version
      io_rich_map <- richness_map(r = io_richness_ras,
                                  predictor = clim_model,
                                  direction = -1,
                                  plot_title = paste0(base_title,
                                                      ", hosts not considered"))
      ggsave(filename = io_richness_map_filename,
             plot = io_rich_map)
    }

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
        ov_delta_map_filename <- paste0(delta_basename, "-ov.", file_ext)
        # Output for insect-only deltas
        io_delta_ras_filename <- paste0(delta_basename, "-io.rds")
        io_delta_map_filename <- paste0(delta_basename, "-io.", file_ext)
        
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
        
        # If appropriate, save delta maps
        if (save_maps) {
          # Overlap
          ov_delta_map <- richness_map(r = ov_delta_ras,
                                    predictor = clim_model,
                                    palette = "purple",
                                    plot_title = paste0(base_title,
                                                        ", overlaps with hosts"),
                                    legend = "Richness change")
          ggsave(filename = ov_delta_map_filename,
                 plot = ov_delta_map)
          # Insect-only
          io_delta_map <- richness_map(r = io_delta_ras,
                                       predictor = clim_model,
                                       palette = "purple",
                                       plot_title = paste0(base_title,
                                                           ", hosts not considered"),
                                       legend = "Richness change")
          ggsave(filename = io_delta_map_filename,
                 plot = io_delta_map)
        }
      } else {
        message("No current richness rasters on disk; deltas not created.")
      }
    } # End conditional for forecast model comparison with current
  } # End conditional for file exists / replace
} # End iteration over all climate models
