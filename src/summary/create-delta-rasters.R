# Create rasters showing difference in predicted distributions from current
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2023-05-03

require(terra)
require(dplyr)
require(stringr)

# Create a raster for each insect, with the following classification:
# 0 = Area unsuitable* in current and forecast climate
# 1 = Area suitable in current climate only (= loss)
# 2 = Area suitable in forecast climate only (= gain)
# 3 = Area suitable in current and forecast climate (= stable)
# * Where a cell is considered suitable iff the insect was predicted as present 
# in the cell AND at least one host plant is predicted present in the same cell

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

# Logical indicating whether to create delta rasters for all species or just a 
# subset of insects
all_insects <- TRUE

# Extract species names
if (all_insects) {
  insects <- unique(ih$insect)
} else {
  # If not all insects, identify which insects to include
  insects <- c("Papilio cresphontes", "Papilio glaucus", "Papilio rumiko",
               "Papilio rutulus")
}

# Remove insects from list that have an insufficient number of filtered 
# occurrence records and therefore should not be included in analyses. 
# (Technically, they shouldn't have overlap rasters, but some obsolete 
# files may still remain in the output/overlaps folder)
exclude <- species_info$species[species_info$pa_csv == "no"]
insects <- insects[!insects %in% exclude]

# Loop through insect species to create a delta raster for each forecast 
# climate model
for (i in 1:length(insects)) {
  
  # Find consensus rasters for the insect
  insect <- insects[i]
  insect_nice_name <- insect %>%
    str_replace(pattern = " ", replacement = "_") %>%
    tolower()
  
  insect_forecast_files <- list.files("output/overlaps",
                                      pattern = paste0(insect_nice_name, 
                                                       "-overlap-ensemble"),
                                      full.names = TRUE)

  insect_current_file <- paste0("output/overlaps/", insect_nice_name,
                                "-overlap-current.rds")
  if (!file.exists(insect_current_file)) {
    message("No contemporary overlap file found for ", insect, 
            ", no deltas will be created.")
  } else {
    if (length(insect_forecast_files) == 0) {
      message("No forecast overlap files found for ", insect,
              ", no deltas will be created.")
    } else {
      current_overlap <- readRDS(file = insect_current_file)
      # Update the raster values so we can do a stack and make calculations
      # See src/summary/create-overlap-rasters.R for key to raster cell values
      # For the deltas, we want only to indicate suitable or unsuitable (as 
      # defined above), so we re-assign cells:
      # Old  New
      #  0 -> 0 (Insect and hosts absent) Insect and all host plants predicted absent
      #  1 -> 0 (1 host only) Insect predicted absent, only 1 host predicted present
      #  2 -> 0 (2 or more hosts) Insect predicted absent, >= 2 hosts predicted present
      #  3 -> 0 (Insect, no hosts) Insect predicted present, all hosts predicted absent
      #  4 -> 1 (Insect, only 1 host) Insect and only 1 host predicted present
      #  5 -> 1 (Insect, 2 or more hosts) Insect and >= 2 hosts predicted present
      current_overlap[current_overlap < 4] <- 0
      current_overlap[current_overlap >= 4] <- 1

      # Iterate over each forecast file and compare with the contemporary 
      # overlap
      for (forecast_file in insect_forecast_files) {
        # Pull out the name of the forecast, will need for output file
        file_basename <- tools::file_path_sans_ext(basename(forecast_file))
        # Remove prefix with species name and "overlap"
        clim_model <- gsub(x = file_basename,
                           pattern = paste0(insect_nice_name, "-overlap-"),
                           replacement = "")
        delta_filename <- paste0("output/deltas/", insect_nice_name,
                                 "-delta-", clim_model, ".rds")
        forecast_overlap <- readRDS(forecast_file)
        # Update forecast overlap raster values as suitable / unsuitable as 
        # was done for current overlap raster, above BUT here we score suitable
        # cells as 2, for easier math when stacking rasters
        forecast_overlap[forecast_overlap < 4] <- 0
        forecast_overlap[forecast_overlap >= 4] <- 2
        
        # Now stack the rasters, current first, forecast second to ensure the 
        # math lines up
        delta_raster <- stack_rasters(r = list(current_overlap, 
                                               forecast_overlap))
        
        # Save delta raster to file
        saveRDS(object = delta_raster, 
                file = delta_filename)
        
        # Save map to file
        if (save_maps) {
          message("Saving delta map for ", insect, ", ", clim_model, ".")

          map_object <- delta_map(species_name = insect,
                                  delta_raster = delta_raster,
                                  clim_model = clim_model,
                                  include_legend = TRUE,
                                  horizontal_legend = FALSE)
                    
          map_file <- paste0("output/maps/",
                             insect_nice_name, 
                             "-delta-",
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
