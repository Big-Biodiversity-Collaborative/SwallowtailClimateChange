# Calculate proportion of species' distributions that are in protected areas
# Erin Zylstra
# ezylstra@arizona.edu
# 2024-03-06

library(dplyr)
library(terra)
library(stringr)
library(tidyr)

# TODO: Troubleshoot, since R crashes.....

# Protected areas database for North America is huge. It was obtained here:
# http://www.cec.org/north-american-environmental-atlas/north-american-protected-areas-2021/
# Right now, it's downloaded to a zip file on Google Drive. To make this script 
# run, will need to unzip that folder and put the shapefiles somewhere. Identify
# that path below.
# TODO: move these files somewhere easy that we can all access.

# Identify location of Protected Area shapefiles:
shpfile_path <- "C:/Users/erin/OneDrive/PrudicLab/Swallowtails/CEC_NA_2021_terrestrial_IUCN_categories/CEC_NA_2021_terrestrial_IUCN_categories.shp"

# Logical indicating whether to summary table if it already exists
replace <- TRUE

# Load insect-host file
ih <- read.csv("data/insect-host.csv")

# Load information about data availability for each species
species_info <- read.csv("data/gbif-pa-summary.csv")

# Load list of climate models
climate_models <- read.csv(file = "data/climate-models.csv")
climate_names_short <- climate_models$name %>%
  str_remove(pattern = "ensemble_")

# Logical indicating whether to summarize protected area coverage for all 
# insects or just a subset of them
all_insects <- FALSE

# Extract species names
if (all_insects) {
  insects <- unique(ih$insect)
} else {
  # If not all insects, identify which insects to include
  insects <- c("Papilio appalachiensis", "Papilio rumiko")
}

# Remove insects from list that have an insufficient number of filtered 
# occurrence records and therefore should not be included in analyses. 
# (Technically, they shouldn't have overlap rasters, but some obsolete 
# files may still remain in the output/overlaps folder)
exclude <- species_info$species[species_info$pa_csv == "no"]
insects <- insects[!insects %in% exclude]

# Make vector of compute-friendly insect names
nice_names <- insects %>%
  str_replace(pattern = " ", replacement = "_") %>%
  tolower()

# Will need to update raster values in insects' overlap rasters
# For maps considering only areas where insect overlaps with at least one plant
#   [0, 3]: NA
#   [4, 5]: 1
# Reclassification matrix, for distribution = "insect + host"
ov_rcl <- matrix(data = c(0, 3, NA,
                          4, 5, 1),
                 nrow = 2,
                 byrow = TRUE)
# For maps only considering insects, regardless of suitable areas for plants
#  [0, 2]: NA
#  [3, 5]: 1
# Reclassification matrix, for distribution = "total insect"
io_rcl <- matrix(data = c(0, 2, NA,
                          3, 5, 1),
                 nrow = 2,
                 byrow = TRUE)

# Read in protected areas file
pa <- vect(shpfile_path)
  # 62,272 polygons 
  # Projected CRS = Sphere_ARC_INFO_Lambert_Azimuthal_Equal_Area
# count(data.frame(pa), COUNTRY) 
  # 8,377 CAN; 531 MEX; 53,364 USA
# count(data.frame(pa), IUCN_CAT, IUCN_DES)
  # Ia (Strict Nature Reserve): 1,301
  # Ib (Wilderness Area): 2,314
  # II (National Park): 1,808
  # III (National Monument or Feature): 2,192
  # IV (Habitat/Species Management Area): 5,514
  # V (Protected Landscape/Seascape): 45,482
  # VI (Protected Area w/Sustainable Use of Nat Resources): 3,661

# Need to crop the protected areas layer before reprojecting (else R aborts)
# Using a xmin value of -4000000 works (cuts off parts of AK that are far west)

# First, get rid of areas in US outside of 50 states, then crop and reproject
pa <- pa %>%
  terra::subset(!pa$STATE_PROV %in% c("US-AS", "US-FM", "US-GU", "US-MH", 
                                      "US-N/A","US-PR", "US-PW", "US-UM", 
                                      "US-VI"))
pa_ext <- ext(pa)
pa_ext[1] <- -4000000
pa_crop <- terra::crop(pa, pa_ext)
pa <- terra::project(pa_crop, "epsg:4326")

# Create table to hold summary values
distributions <- c("total insect", "insect + host")
stats <- as.data.frame(expand_grid(insect = insects, 
                                   distribution = distributions,
                                   climate = climate_names_short)) %>%
  mutate(area_sqkm = NA,
         area_protected_sqkm = NA)

# General workflow:
# Loop over species
# Load overlap maps for all climate models (current, future)
# Crop protected areas layer to future species extent
# For each climate model:
  # Reclassify overlap raster (so we have species distribution that does/doesn't account for plants)
  # Create raster with cell values = land area
  # Calculate the fraction of each cell that falls in protected areas
  # Calculate the proportion of area in species distribution that's protected

for (i in 1:length(insects)) {
  
  overlap_filenames <- paste0("output/overlaps/", nice_names[i], "-overlap-",
                              climate_models$name, ".rds")
  
  if (length(overlap_filenames) < nrow(climate_models)) {
    
    message("*** Missing one or more overlap rasters for ", insect[i], 
            ". Did not calculate protected area summaries.")
  
  } else {
    
    # Load one of the future overlap rasters to crop protected areas layer
    overlap <- readRDS(overlap_filenames[2])
    
    # Crop protected areas to make things easier
    pa_c <- terra::crop(pa, ext(overlap))
    
    # Loop over climate models and do calculations
    for (j in 1:length(overlap_filenames)) {
      overlap <- readRDS(overlap_filenames[j])
      
      # Reclassify, for distribution of insect + hosts
      r_ov <- terra::classify(overlap, ov_rcl, right = NA)
      # Reclassify, for distribution of insect
      r_io <- terra::classify(overlap, io_rcl, right = NA)
      
      # Create rasters with cell areas, in sq km
      size_ov <- terra::cellSize(r_ov, mask = TRUE, unit = "km")
      size_io <- terra::cellSize(r_io, mask = TRUE, unit = "km")
      
      # Calculate the fraction of each cell that falls in protected areas
      in_pa_ov <- terra::extract(size_ov, pa_c, exact = TRUE)
      in_pa_io <- terra::extract(size_io, pa_c, exact = TRUE)
      # This results in a data.frame with 3 columns: 
      # ID (polygon), area (raster cell value), fraction (proportion of each 
      # cell contained within polygon).
      
      # Remove cells outside of spp distribution that fall in protected areas
      dist_in_pa_ov <- dplyr::filter(in_pa_ov, !is.na(area))
      dist_in_pa_io <- dplyr::filter(in_pa_io, !is.na(area))
      
      # Calculate area of spp distribution
      spp_area_ov <- terra::global(size_ov, "sum", na.rm = TRUE)
      spp_area_io <- terra::global(size_io, "sum", na.rm = TRUE)
      
      # Calculate area within spp distribution that's also within a protected area
      dist_in_pa_ov <- dist_in_pa_ov %>%
        mutate(area_in = area * fraction)
      dist_in_pa_io <- dist_in_pa_io %>%
        mutate(area_in = area * fraction)
      spp_area_prot_ov <- sum(dist_in_pa_ov$area_in)
      spp_area_prot_io <- sum(dist_in_pa_io$area_in)
      
      # Add to summary table
      row_index_ov <- which(stats$insect == insects[i],
                            stats$climate == climate_names_short[j],
                            stats$distribution == "insect + host")
      row_index_io <- which(stats$insect == insects[i],
                            stats$climate == climate_names_short[j],
                            stats$distribution == "total insect")
      stats$area_sqkm[row_index_ov] <- spp_area_ov
      stats$area_sqkm[row_index_io] <- spp_area_io
      stats$area_protected_sqkm[row_index_ov] <- spp_area_prot_ov
      stats$area_protected_sqkm[row_index_io] <- spp_area_prot_io
    }
  }
}

# Calculate proportion of species' distributions that are protected
stats$proportion_protected <- stats$area_protected_sqkm / stats$area_sqkm

# Write to file
if (all_insects) {
  spp <- "allspp"
} else {
  spp <- paste0(length(insects), "spp")
}

summary_filename <- paste0("output/summary-stats/protected-areas-",
                           spp, ".csv")

if (!(file.exists(summary_filename) & replace == FALSE)) {
  write.csv(x = stats,
            file = summary_filename, 
            row.names = FALSE)
}



