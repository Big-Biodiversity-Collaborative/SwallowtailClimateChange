# Calculate proportion of species' distributions that are in protected areas
# Erin Zylstra & Jeff Oliver
# ezylstra@arizona.edu, jcoliver@arizona.edu
# 2024-08-28

require(dplyr)
require(terra)
require(stringr)
require(tidyr)
require(exactextractr)
require(sf)

# Identify where cropped, projected shapefile with protected areas in North
# America lives:
# shpfile_path <- "C:/Users/erin/Desktop/PAs/protected-areas.shp"
shpfile_path <- "data/protected-areas/protected-areas-categorized.shp"

# Protected areas database for North America is huge (tons of small polygons). 
# It was obtained here:
# http://www.cec.org/north-american-environmental-atlas/north-american-protected-areas-2021/
# The original file was processed to categorize each polygon as "National", 
# "State", "Local", or "Private" in terms of management, then polygons in each 
# category were merged to create a SpatVector with four layers, one for each 
# management level.

# Logical indicating whether to replace summary table if it already exists
replace <- TRUE

# Load insect-host file
ih <- read.csv("data/insect-host.csv")

# Load information about data availability for each species
species_info <- read.csv("data/gbif-pa-summary.csv")

# Load list of climate models
climate_models <- read.csv(file = "data/climate-models.csv")
climate_names_short <- climate_models$name %>%
  stringr::str_remove(pattern = "ensemble_")

# Logical indicating whether to summarize protected area coverage for all 
# insects or just a subset of them
all_insects <- TRUE

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
  stringr::str_replace(pattern = " ", replacement = "_") %>%
  tolower()

# Will need to update raster values in insects' overlap raster
# When considering only areas where insect overlaps with at least one plant
#   [0, 3]: NA
#   [4, 5]: 1
# Reclassification matrix, for distribution = "insect + host"
ih_rcl <- matrix(data = c(0, 3, NA,
                          4, 5, 1),
                 nrow = 2,
                 byrow = TRUE)
# When only considering insects, regardless of suitable areas for plants
#  [0, 2]: NA
#  [3, 5]: 1
# Reclassification matrix, for distribution = "total insect"
io_rcl <- matrix(data = c(0, 2, NA,
                          3, 5, 1),
                 nrow = 2,
                 byrow = TRUE)
# Put reclassification matrices in a list
rcls <- list(io_rcl, ih_rcl) 

# Create table to hold summary values
distributions <- c("total insect", "insect + host")
stats <- as.data.frame(expand_grid(insect = insects, 
                                   distribution = distributions,
                                   climate = climate_names_short)) %>%
  mutate(area_sqkm = NA,
         area_prot_sqkm_national = NA,
         area_prot_sqkm_state = NA,
         area_prot_sqkm_local = NA,
         area_prot_sqkm_private = NA)

# Read in protected areas file (may take ~3 minutes)
pa <- terra::vect(shpfile_path)

# For each climate model and distribution type:
  # Reclassify overlap raster (species distribution = 1, everything else NA)
  # Create raster with cell values = land area
  # Calculate the fraction of each cell that falls in a protected area polygon
  #     for each of four protected area categories
  # Calculate the proportion of area in species distribution that's protected
  #     for each of four protected area categories

for (i in 1:length(insects)) {

  overlap_filenames <- paste0("output/overlaps/", nice_names[i], "-overlap-",
                              climate_models$name, ".rds")
  
  if (length(overlap_filenames) < nrow(climate_models)) {
    
    message("*** Missing one or more overlap rasters for ", insect[i], 
            ". Did not calculate protected area summaries.")
  
  } else {
    
    # Load one of the future overlap rasters to crop protected areas layer
      # overlap <- readRDS(overlap_filenames[2])
      # ov_ext <- ext(overlap)
      # pa_ext <- ext(pa)
      # ov_ext[1] <- ifelse(ov_ext[1] < pa_ext[1], pa_ext[1], ov_ext[1])
      # ov_ext[2] <- ifelse(ov_ext[2] > pa_ext[2], pa_ext[2], ov_ext[2])    
      # ov_ext[3] <- ifelse(ov_ext[3] < pa_ext[3], pa_ext[3], ov_ext[3]) 
      # ov_ext[4] <- ifelse(ov_ext[4] > pa_ext[4], pa_ext[4], ov_ext[4])  
      # pa <- st_crop(pa, new_ext)
    # No matter how I tried to do this (with terra or sf package) R crashes.
    # Skipping this step for now.
    
    # Loop over climate models and distribution types
    for (j in 1:length(overlap_filenames)) {
      for (k in 1:2) {
        
        # Print status
        cat(paste0("Calculating protected area for ", insects[i], ", ", 
                   climate_names_short[j], ", distribution = ", 
                   distributions[k], "\n"))
        
        # Read in overlap raster
        r <- readRDS(overlap_filenames[j])
        
        # Reclassify
        r <- terra::classify(r, rcls[[k]], right = NA)
        
        # Create raster with cell areas, in sq km
        r <- terra::cellSize(r, mask = TRUE, unit = "km")
        
        # Calculate the fraction of each cell that falls in protected areas
          # Tried to do this with terra::extract, but R crashes (except for 
          # species with the smallest ranges)
          # in_pa <- terra::extract(r, pa, exact = TRUE, na.rm = TRUE)
              # This results in a data.frame with 3 columns: 
              # ID (polygon), area (raster cell value), fraction (proportion of each 
              # cell contained within polygon).
        # Using exactextractr package instead
        
        # pa is a 4-layer SpatVector
        in_pa <- exactextractr::exact_extract(x = r, 
                                              y = sf::st_as_sf(pa),
                                              progress = FALSE)
        # Resulting in_pa is a 4-element, unnamed list. Get the names of the 
        # elements from the pa SpatVector
        names(in_pa) <- data.frame(pa)$AGNCY_SHOR

        # Transform list into data frame, calculate areas, then sum for each 
        # category
        in_pa_df <- in_pa %>%
          bind_rows(.id = "AGNCY_SHOR") %>%
          dplyr::filter(!is.na(value)) %>%
          mutate(area_in = value * coverage_fraction) %>%
          group_by(AGNCY_SHOR) %>%
          summarize(sum_area_in = sum(area_in))
        
        # Calculate area of species' distribution
        spp_area <- terra::global(r, "sum", na.rm = TRUE)$sum
        spp_area <- ifelse(is.na(spp_area), 0, spp_area)

        # Just a little memory leak paranoia here
        rm(in_pa, r)
        gc()

        # Put summary stats in table
        row_index <- which(stats$insect == insects[i] &
                             stats$climate == climate_names_short[j] &
                             stats$distribution == distributions[k])
        stats$area_sqkm[row_index] <- spp_area
        
        # Blech, some lazy base R subsetting from me today (also, if_else is 
        # too picky)
        stats$area_prot_sqkm_national[row_index] = ifelse(length(in_pa_df$sum_area_in[in_pa_df$AGNCY_SHOR == "National"]) == 0, 0, in_pa_df$sum_area_in[in_pa_df$AGNCY_SHOR == "National"])
        stats$area_prot_sqkm_state[row_index] = ifelse(length(in_pa_df$sum_area_in[in_pa_df$AGNCY_SHOR == "State"]) == 0, 0, in_pa_df$sum_area_in[in_pa_df$AGNCY_SHOR == "State"])
        stats$area_prot_sqkm_local[row_index] = ifelse(length(in_pa_df$sum_area_in[in_pa_df$AGNCY_SHOR == "Local"]) == 0, 0, in_pa_df$sum_area_in[in_pa_df$AGNCY_SHOR == "Local"])
        stats$area_prot_sqkm_private[row_index] = ifelse(length(in_pa_df$sum_area_in[in_pa_df$AGNCY_SHOR == "Private"]) == 0, 0, in_pa_df$sum_area_in[in_pa_df$AGNCY_SHOR == "Private"])
      }
    }  
  }
}

# Calculate proportion of species' distributions that are protected
stats$proportion_national <- ifelse(stats$area_sqkm == 0, NA,
                                    stats$area_prot_sqkm_national / stats$area_sqkm)
stats$proportion_state <- ifelse(stats$area_sqkm == 0, NA,
                                 stats$area_prot_sqkm_state / stats$area_sqkm)
stats$proportion_local <- ifelse(stats$area_sqkm == 0, NA,
                                 stats$area_prot_sqkm_local / stats$area_sqkm)
stats$proportion_private <- ifelse(stats$area_sqkm == 0, NA,
                                   stats$area_prot_sqkm_private / stats$area_sqkm)

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
