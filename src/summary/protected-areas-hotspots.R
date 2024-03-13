# Calculate proportion of areas with high swallowtail diversity that are protected
# Erin Zylstra
# ezylstra@arizona.edu
# 2024-03-12

require(dplyr)
require(terra)
require(stringr)
require(tidyr)
require(exactextractr)
require(sf)

# Identify where cropped, projected shapefile with protected areas in North
# America will/does lives (shapefile was created in protected-areas.R):
shpfile_path <- "C:/Users/erin/Desktop/PAs/protected-areas.shp"
# TODO: move shapefile to a site with public access

# Logical indicating whether to replace summary table if it already exists
replace <- TRUE

# Load list of climate models
climate_models <- read.csv(file = "data/climate-models.csv")
climate_names_short <- climate_models$name %>%
  str_remove(pattern = "ensemble_")

# Define a "species rich" area (ie, hotspot) by identifying the minimum number 
# of species (can do calculations for more than one minimum)
spp_min <- 3:4

# Create table to hold summary values
distributions <- c("total insect", "insect + host")
stats <- as.data.frame(expand_grid(min_num_spp = spp_min, 
                                   distribution = distributions,
                                   climate = climate_names_short)) %>%
  mutate(area_sqkm = NA,
         area_protected_sqkm = NA)

# Read in protected areas file
pa <- vect("C:/Users/erin/Desktop/PAs/protected-areas.shp")

# For each minimum number of species, climate model, and distribution type:
  # Reclassify richness raster (richness >= minimum = 1, NA everywhere else)
  # Create raster with cell values = land area
  # Calculate the fraction of each cell that falls in a protected area polygon
  # Calculate the proportion of species-rich area that's protected

for (distrib in distributions) {
  
  distrib_short <- ifelse(distrib == "total insect", "io", "ov")
  
  for (i in 1:nrow(climate_models)) {
    
    # Read in richness raster
    richness_file <- paste0("output/richness/", climate_models$name[i], 
                            "-richness-", distrib_short, ".rds")
    richness <- readRDS(richness_file)
    
    for (j in spp_min) {
      
      # Print status
      cat(paste0("Calculating protected area for ", j, " or more species, ", 
                 climate_names_short[i], ", distribution = ", distrib, "\n"))
      
      # Reclassify richness (call something new)
      rcl <- matrix(data = c(0, j - 1, NA,
                             j, minmax(richness)[2], 1),
                       nrow = 2,
                       byrow = TRUE)
      r <- terra::classify(richness, rcl, right = NA)
    
      # Create raster with cell areas, in sq km
      r <- terra::cellSize(r, mask = TRUE, unit = "km")
      
      # Calculate the fraction of each cell that falls in protected areas
      in_pa <- exactextractr::exact_extract(x = r, 
                                            y = sf::st_as_sf(pa),
                                            progress = FALSE)
      
      # Calculate the area that's protected
      in_pa <- bind_rows(in_pa) %>%
        dplyr::filter(!is.na(value)) %>%
        mutate(area_in = value * coverage_fraction)
      area_prot <- sum(in_pa$area_in)
      
      # Calculate area of richness hotspot
      area <- terra::global(r, "sum", na.rm = TRUE)$sum
      area <- ifelse(is.na(area), 0, area)
      
      # Put summary stats in table
      row_index <- which(stats$min_num_spp == j &
                           stats$climate == climate_names_short[i] &
                           stats$distribution == distrib)
      stats$area_sqkm[row_index] <- area
      stats$area_protected_sqkm[row_index] <- area_prot
    }  
  }
}

# Calculate proportion of hotspots that are protected
stats$proportion_protected <- ifelse(stats$area_sqkm == 0, NA,
                                     stats$area_protected_sqkm / stats$area_sqkm)

# Write to file
summary_filename <- paste0("output/summary-stats/protected-areas-hotspots.csv")

if (!(file.exists(summary_filename) & replace == FALSE)) {
  write.csv(x = stats,
            file = summary_filename, 
            row.names = FALSE)
}
