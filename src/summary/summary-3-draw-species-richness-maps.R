# Create species richness maps of current & forecast insect species
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2021-06-06

# TODO: Placeholder script
# Develop for all insects based on draw-species-richness-maps-glm.R

require(terra)
require(dplyr)
require(stringr)

#TODO: Should we use output/maps as destination, or create output/richness

# Logical indicating whether to replace an overlap raster if one already exists
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

# Loop over all climate models (including current)
# Read in all insect rasters
# Convert rasters to 0/1
# Extend all rasters to same extent
# Sum all rasters together
# Save raster to output/maps
# If save_maps is TRUE, create ggplot map and save to output/maps

nice_names <- insects %>%
  str_replace(pattern = " ", replacement = "_") %>%
  tolower()

# Iterate over each climate model, creating a Papilio species richness raster 
# (and map if appropriate) for each model (5 total)
for (clim_model in climate_models$name) {
  cat("Climate model: ", clim_model, "\n")
  overlap_files <- paste0("output/overlaps/",
                          nice_names, 
                          "-overlap-", 
                          clim_model, 
                          ".rds")
}
