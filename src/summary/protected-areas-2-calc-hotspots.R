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
# America lives:
# shpfile_path <- "C:/Users/erin/Desktop/PAs/protected-areas.shp"
shpfile_path <- "data/protected-areas/protected-areas-categorized.shp"

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
         area_prot_sqkm_national = NA,
         area_prot_sqkm_state = NA,
         area_prot_sqkm_local = NA,
         area_prot_sqkm_private = NA)

# Read in protected areas file (may take ~3 minutes)
pa <- terra::vect(shpfile_path)

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
      
      # Perhaps unnecessary
      rm(in_pa, r)
      gc()
      
      # Put summary stats in table
      row_index <- which(stats$min_num_spp == j &
                           stats$climate == climate_names_short[i] &
                           stats$distribution == distrib)
      stats$area_sqkm[row_index] <- spp_area
      
      stats$area_prot_sqkm_national[row_index] = ifelse(length(in_pa_df$sum_area_in[in_pa_df$AGNCY_SHOR == "National"]) == 0, 0, in_pa_df$sum_area_in[in_pa_df$AGNCY_SHOR == "National"])
      stats$area_prot_sqkm_state[row_index] = ifelse(length(in_pa_df$sum_area_in[in_pa_df$AGNCY_SHOR == "State"]) == 0, 0, in_pa_df$sum_area_in[in_pa_df$AGNCY_SHOR == "State"])
      stats$area_prot_sqkm_local[row_index] = ifelse(length(in_pa_df$sum_area_in[in_pa_df$AGNCY_SHOR == "Local"]) == 0, 0, in_pa_df$sum_area_in[in_pa_df$AGNCY_SHOR == "Local"])
      stats$area_prot_sqkm_private[row_index] = ifelse(length(in_pa_df$sum_area_in[in_pa_df$AGNCY_SHOR == "Private"]) == 0, 0, in_pa_df$sum_area_in[in_pa_df$AGNCY_SHOR == "Private"])
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
summary_filename <- paste0("output/summary-stats/protected-areas-hotspots.csv")

if (!(file.exists(summary_filename) & replace == FALSE)) {
  write.csv(x = stats,
            file = summary_filename, 
            row.names = FALSE)
}
