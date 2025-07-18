# Summarize rasters delineating geographic overlap between the predicted 
# distributions of each insect species and its host plants, and save rasters for
# each future period indicating areas of a species' distribution that were lost, 
# gained, or retained since the current time period (delta raster)
# Erin Zylstra
# ezylstra@arizona.edu
# 2023-12-13

require(terra)
require(tidyr)
require(dplyr)
require(stringr)

# Logicals indicating whether to replace table with summary statistics and
# delta rasters if they already exist
replace_table <- TRUE
replace_rasters <- TRUE

## Current set of summary stats ################################################
# Summary stats that are calculated for each time period (and distribution type)
  # area: total area (sqkm)
  # lat_max: median latitude of 10 northernmost cells
  # lat_min: median latitude of 10 southernmost cells
  # lon_max: median longitude of 10 easternmost cells
  # lon_min: median longitude of 10 westernmost cells
  # lat_max_bands: median lat among the northernmost cells in each long band
  # lat_min_bands: median lat among the southernmost cells in each long band
  # lon_max_bands: median long among the easternmost cells in each lat band
  # lon_min_bands: median long among the westernmost cells in each lat band
  # lat_max_05: median latitude of 5% northernmost cells
  # lat_min_05: median latitude of 5% southernmost cells
  # lon_max_05: median longitude of 5% easternmost cells
  # lon_min_05: median longitude of 5% westernmost cells

# Summary stats that are calculated for each time period (across distribution types)
  # pinsect_withhost: % of insect range that overlaps with >= 1 host plant; 
  #   only calculated for rows where distribution == "total insect"
  # pinsecthost_1host: % of insect + host range where only 1 host plant occurs; 
  #   only calculated for rows where distribution == "insect + host" 
  # phost_insect: % of host range that overlaps with insect (i.e. the percent
  #   of host plant range that is actually suitable for the insect); only 
  #   calculated for rows where distribution == "total insect". NOT CALCULATED.

# Summary stats that are calculated between future and current time periods (for 
# each distribution type)
  # area_gained: area (sqkm) predicted suitable in future that wasn't in current range
  # area_lost: area (sqkm) predicted unsuitable in future that was in current range
  # area_retained: area (sqkm) predicted suitable in future that was in current range
  # lat_max_shift: median value of shifts (km) along northern edge in each 
  #  longitudinal band (positive values = northward shift; negative values = 
  #   southward shift)
  # lat_min_shift: median value of shifts (km) along southern edge in each 
  #   longitudinal band (pos values = northward; neg values = southward)
  # lon_max_shift: median value of shifts (km) along eastern edge in each 
  #   latitudinal band (positive values = eastward shift; negative values = 
  #   westward shift)
  # lon_min_shift: median value of shifts (km) along western edge in each 
  #   latitudinal band (pos values = eastward; neg values = westward)
  # Note for these band shift calculations: only included bands with >= 1 cell 
  #   considered suitable in both time periods
  # lat_max_05_shift: shift (deg) in median values of northernmost 5% of cells
  #   (positive values = northward shift; negative values = southward shift)
  # lat_min_05_shift: shift (deg) in median values of southernmost 5% of cells 
  #   (pos values = northward; neg values = southward)
  # lon_max_05_shift: shift (deg) in median values of easternmost 5% of cells
  #   (positive values = eastward shift; negative values = westward shift)
  # lon_min_05_shift: shift (deg) in median values of westernmost 5% of cells
  #   (pos values = eastward; neg values = westward)

# Note: Can calculate % of current range that's still suitable in future as 
# area_retained/(area retained + area lost)

# Also creates delta raster for each insect, with the following classification:
# 0 = Area unsuitable* in current and forecast climate
# 1 = Area suitable in current climate only (= lost)
# 2 = Area suitable in forecast climate only (= gained)
# 3 = Area suitable in current and forecast climate (= retained)
# * Where a cell is considered suitable iff the insect was predicted as present 
# in the cell AND at least one host plant is predicted present in the same cell

# Load insect-host file
ih <- read.csv("data/insect-host.csv")

# Load information about data availability for each species
species_info <- read.csv("data/gbif-pa-summary.csv")

# Load list of climate models
climate_models <- read.csv(file = "data/climate-models.csv")
climate_names_short <- climate_models$name %>%
  str_remove(pattern = "ensemble_")

# Logical indicating whether to summarize overlap rasters for all species or 
# just a subset of insects
all_insects <- TRUE

# Extract species names
if (all_insects) {
  insects <- unique(ih$insect)
} else {
  # If not all insects, identify which insects to include
  insects <- c("Papilio rumiko")
}

# Remove insects from list that have an insufficient number of filtered 
# occurrence records and therefore should not be included in analyses. 
# (Technically, they shouldn't have overlap rasters, but some obsolete 
# files may still remain in the output/overlaps folder)
exclude <- species_info$species[species_info$pa_csv == "no"]
insects <- insects[!insects %in% exclude]

# Interested in summarizing values for (1) all areas predicted to be suitable 
# for the insect (overlap raster values 3, 4, and 5), and (2) areas predicted to 
# be suitable for the insect and one or more of its host plants (overlap = 4-5).
distributions <- c("total insect", "insect + host")

################################################################################

# Create table to hold summary statistics
stats <- as.data.frame(expand_grid(insect = insects, 
                                   distribution = distributions,
                                   climate = climate_names_short)) %>%
  mutate(area = NA,
         lat_max = NA,
         lat_min = NA,
         lon_max = NA,
         lon_min = NA,
         lat_max_bands = NA,
         lat_min_bands = NA,
         lon_max_bands = NA,
         lon_min_bands = NA,
         lat_max_05 = NA,
         lat_min_05 = NA,
         lon_max_05 = NA,
         lon_min_05 = NA,
         area_gained = NA,
         area_lost = NA,
         area_retained = NA,
         lat_max_shift = NA,
         lat_min_shift = NA,
         lon_max_shift = NA,
         lon_min_shift = NA,
         lat_max_05_shift = NA,
         lat_min_05_shift = NA,
         lon_max_05_shift = NA,
         lon_min_05_shift = NA,
         pinsect_withhost = NA,
         pinsecthost_1host = NA,
         phost_insect = NA)

# Loop through insect species
for (i in 1:length(insects)) {

  nice_name <- insects[i] %>%
    str_replace(pattern = " ", replacement = "_") %>%
    tolower()
  short_name <- insects[i] %>%
    str_replace(pattern = "Papilio", replacement = "P.")
  
  cat(paste0("Summarizing overlap rasters for ", short_name, ".\n"))
  
  # Grab overlap rasters for insect
  overlap_files <- list.files(path = "output/overlaps", 
                              pattern = nice_name, 
                              full.names = TRUE)
  
  # Create list of rasters with all areas predicted suitable for insect under
  # all climate scenarios (current + 4 future)
  allinsect_list <- list()
  # Create list of rasters with areas predicted suitable for insect and at 
  # least one host plant under all climate scenarios (current + 4 future)
  insecthost_list <- list()
  
  for (j in 1:length(overlap_files)) {

    allinsect_list[[j]] <- readRDS(overlap_files[j])
    insecthost_list[[j]] <- readRDS(overlap_files[j])

    # Calculate areas in each overlap category
    ih_areas <- round(terra::expanse(allinsect_list[[j]], 
                                     unit = "km",
                                     byValue = TRUE))
    cats <- ih_areas[, "value"]

    # Calculate the percent of insect range that overlaps with one or more host
    # plants (if possible)
    row_index1 <- which(stats$insect == insects[i] & 
                          stats$distribution == "total insect" &
                          stats$climate == climate_names_short[j])
    if (max(cats) > 3) {
      # Total area estimated to be suitable for insect
      insect_area <- sum(ih_areas[ih_areas[, "value"] %in% 3:5, "area"])
      # Total area of suitable insect areas that are also suitable for one or 
      # more hosts
      ih_area <- sum(ih_areas[ih_areas[, "value"] %in% 4:5, "area"])
      stats$pinsect_withhost[row_index1] <- round(ih_area / insect_area * 100, 4)
      # Total area of areas suitable for hosts
      # host_area <- sum(ih_areas[ih_areas[, "value"] %in% c(1:2, 4:5), "area"])
      # stats$phost_insect[row_index1] <- round(ih_area / host_area * 100, 4)
    } else if (max(cats) == 3) {
      stats$pinsect_withhost[row_index1] <- 0
      # stats$phost_insect[row_index1] <- 0
    } else {
      stats$pinsect_withhost[row_index1] <- NA
      # stats$phost_insect[row_index1] <- NA
    }
    
    # Calculate the percent of insect + host range where only 1 host occurs
    row_index2 <- which(stats$insect == insects[i] & 
                          stats$distribution == "insect + host" &
                          stats$climate == climate_names_short[j])
    if (all(4:5 %in% cats)) {
      ih_area <- sum(ih_areas[ih_areas[, "value"] %in% 4:5, "area"])
      ih1_area <- sum(ih_areas[ih_areas[, "value"] == 4, "area"])
      stats$pinsecthost_1host[row_index2] <- round(ih1_area / ih_area * 100, 4)
    } else {
      if (4 %in% cats & !5 %in% cats) {
        stats$pinsecthost_1host[row_index2] <- 100
      } else if (5 %in% cats & !4 %in% cats) {
        stats$pinsecthost_1host[row_index2] <- 0
      } else {
        stats$pinsecthost_1host[row_index2] <- NA 
      }
    }

    # For all areas predicted suitable for the insect: reclassify cells in 
    # overlap rasters that are >= 3 as 1 (NA everywhere else)
    allinsect_list[[j]] <- terra::classify(x = allinsect_list[[j]],
                                           rcl = matrix(c(3, Inf, 1), nrow = 1),
                                           right = FALSE,
                                           others = NA)
    
    # For all areas predicted suitable for the insect and one or more host
    # plants: reclassify cells in overlap rasters that are >= 4 as 1 
    # (NA everywhere else)
    insecthost_list[[j]] <- terra::classify(x = insecthost_list[[j]],
                                            rcl = matrix(c(4, Inf, 1), nrow = 1),
                                            right = FALSE,
                                            others = NA)
  }

  # Loop through distribution types
  for (distribution in distributions) {
    if (distribution == "total insect") {
      raster_list <- allinsect_list
      message_spp <- short_name
      distrib_short <- "totalinsect"
    } else {
      raster_list <- insecthost_list
      message_spp <- paste0(short_name, " + hostplants")
      distrib_short <- "insecthost"
    }
    
    # Do calculations for current time period (max value should always be 3 or greater #########)
    row_index_c <- which(stats$insect == insects[i] & 
                           stats$distribution == distribution &
                           stats$climate == "current")
    # Grab 0/1 raster and convert to a NA/1 raster
    current <- raster_list[[1]]

    # Calculate land area (in sq km) and add to summary
    ca <- terra::expanse(current, unit = "km")
    stats$area[row_index_c] <- round(ca[, "area"])
    
    # Create a data frame with lat/long for all non-NA cells
    current_df <- terra::as.data.frame(current, xy = TRUE, na.rm = TRUE)
    
    # Calculate min/max values (median of 10 most extreme cells). Similar to
    # Grewe et al. 2013.
    stats$lat_max[row_index_c] <- round(median(tail(sort(current_df$y), 10)), 2)
    stats$lat_min[row_index_c] <- round(median(head(sort(current_df$y), 10)), 2)
    stats$lon_max[row_index_c] <- round(median(tail(sort(current_df$x), 10)), 2)
    stats$lon_min[row_index_c] <- round(median(head(sort(current_df$x), 10)), 2)
    
    # Calculate 5% at extremes (median of highest/lowest 5% of cells)
    n_cells_05_c <- round(0.05 * nrow(current_df), 0)
    stats$lat_max_05[row_index_c] <- round(median(tail(sort(current_df$y), n_cells_05_c)), 2)
    stats$lat_min_05[row_index_c] <- round(median(head(sort(current_df$y), n_cells_05_c)), 2)
    stats$lon_max_05[row_index_c] <- round(median(tail(sort(current_df$x), n_cells_05_c)), 2)
    stats$lon_min_05[row_index_c] <- round(median(head(sort(current_df$x), n_cells_05_c)), 2)

    # Calculate max/min latitude for each longitudinal band
    current_lats <- current_df %>%
      group_by(x) %>%
      summarize(max_current = max(y),
                min_current = min(y)) %>%
      data.frame()

    # Calculate median latitude along northern boundary
    stats$lat_max_bands[row_index_c] <- round(median(current_lats$max_current), 2)
    # Calculate median latitude along southern boundary
    stats$lat_min_bands[row_index_c] <- round(median(current_lats$min_current), 2)
    
    # Calculate max/min longitude for each latitudinal band
    current_lons <- current_df %>%
      group_by(y) %>%
      summarize(max_current = max(x),
                min_current = min(x)) %>%
      data.frame()
    
    # Calculate median longitude along eastern boundary
    stats$lon_max_bands[row_index_c] <- round(median(current_lons$max_current), 2)
    # Calculate median latitude along southern boundary
    stats$lon_min_bands[row_index_c] <- round(median(current_lons$min_current), 2)
    
    # Loop through future climate scenarios
    for (j in 2:nrow(climate_models)) {
      clim_model <- climate_names_short[j]
      row_index <- which(stats$insect == insects[i] & 
                           stats$distribution == distribution &
                           stats$climate == clim_model)
      # Grab 0/1 raster and convert to a NA/1 raster
      future <- raster_list[[j]]
      
      # Calculate land area (in sq km) and add to summary
      fa <- terra::expanse(future, unit = "km")
      stats$area[row_index] <- round(fa[, "area"])  
      
      if (stats$area[row_index] == 0) {
        message("No areas predicted suitable for ", message_spp, ", ",
                clim_model,".")
      } else {
        # Create a data frame with lat/long for all non-NA cells
        future_df <- terra::as.data.frame(future, xy = TRUE, na.rm = TRUE)
        
        # Calculate min/max values (median of 10 most extreme cells). 
        stats$lat_max[row_index] <- round(median(tail(sort(future_df$y), 10)), 2)
        stats$lat_min[row_index] <- round(median(head(sort(future_df$y), 10)), 2)
        stats$lon_max[row_index] <- round(median(tail(sort(future_df$x), 10)), 2)
        stats$lon_min[row_index] <- round(median(head(sort(future_df$x), 10)), 2)
        
        # Calculate 5% at extremes (median of highest/lowest 5% of cells)
        n_cells_05 <- round(0.05 * nrow(current_df), 0)
        stats$lat_max_05[row_index] <- round(median(tail(sort(future_df$y), n_cells_05)), 2)
        stats$lat_min_05[row_index] <- round(median(head(sort(future_df$y), n_cells_05)), 2)
        stats$lon_max_05[row_index] <- round(median(tail(sort(future_df$x), n_cells_05)), 2)
        stats$lon_min_05[row_index] <- round(median(head(sort(future_df$x), n_cells_05)), 2)
        
        # Calculate the shift in this 5% median value (future - current)
        stats$lat_max_05_shift[row_index] <- round(stats$lat_max_05[row_index] - stats$lat_max_05[row_index_c], 2)
        stats$lat_min_05_shift[row_index] <- round(stats$lat_min_05[row_index] - stats$lat_min_05[row_index_c], 2)
        stats$lon_max_05_shift[row_index] <- round(stats$lon_max_05[row_index] - stats$lon_max_05[row_index_c], 2)
        stats$lon_min_05_shift[row_index] <- round(stats$lon_min_05[row_index] - stats$lon_min_05[row_index_c], 2)
        
        # Calculate max/min latitude for each longitudinal band
        future_lats <- future_df %>%
          group_by(x) %>%
          summarize(max_future = max(y),
                    min_future = min(y)) %>%
          data.frame()
        
        # Calculate median latitude along northern boundary
        stats$lat_max_bands[row_index] <- round(median(future_lats$max_future), 2)
        # Calculate median latitude along southern boundary
        stats$lat_min_bands[row_index] <- round(median(future_lats$min_future), 2)
        
        # Calculate max/min longitude for each latitudinal band
        future_lons <- future_df %>%
          group_by(y) %>%
          summarize(max_future = max(x),
                    min_future = min(x)) %>%
          data.frame()
        
        # Calculate median longitude along eastern boundary
        stats$lon_max_bands[row_index] <- round(median(future_lons$max_future), 2)
        # Calculate median latitude along southern boundary
        stats$lon_min_bands[row_index] <- round(median(future_lons$min_future), 2)
        
        # Join future and current latitudinal data (only including longitudinal 
        # bands that have suitable areas in both time periods)
        # Rounding longitudes first to ensure there are matching values
        current_lats$x <- round(current_lats$x, 4)
        future_lats$x <- round(future_lats$x, 4)
        future_lats <- inner_join(current_lats, future_lats, by = "x")
        
        if (nrow(future_lats) == 0) {
          message("None of the same longitudinal bands are in the current and ",
                  clim_model, " distributions of ", message_spp, ".")
        } else {
          # Calculating shift along northern edge, in km, in each longitudinal band
          future_n <- as.matrix(future_lats[, c("x", "max_future")])
          current_n <- as.matrix(future_lats[, c("x", "max_current")])
          # Distances in meters
          dists_n <- terra::distance(future_n, current_n, 
                                     lonlat = TRUE, pairwise = TRUE)
          # Calculate shift (positive values = north) and convert to km
          shift_n_km <- ifelse(future_n[, "max_future"] > current_n[, "max_current"], 
                             1 * dists_n, -1 * dists_n) / 1000
          stats$lat_max_shift[row_index] <- round(median(shift_n_km))

          # Calculating shift along southern edge, in km, in each longitudinal band
          future_s <- as.matrix(future_lats[, c("x", "min_future")])
          current_s <- as.matrix(future_lats[, c("x", "min_current")])
          # Distances in meters
          dists_s <- terra::distance(future_s, current_s, 
                                     lonlat = TRUE, pairwise = TRUE)
          # Calculate shift (positive values = north) and convert to km
          shift_s_km <- ifelse(future_s[, "min_future"] > current_s[, "min_current"], 
                               1 * dists_s, -1 * dists_s) / 1000
          stats$lat_min_shift[row_index] <- round(median(shift_s_km))
        }  
          
        # Join future and current latitudinal data (only including longitudinal 
        # bands that have suitable areas in both time periods)
        # Rounding longitudes first to ensure there are matching values
        current_lons$y <- round(current_lons$y, 4)
        future_lons$y <- round(future_lons$y, 4)
        future_lons <- inner_join(current_lons, future_lons, by = "y")    
        
        if (nrow(future_lons) == 0) {
          message("None of the same latitudinal bands are in the current and ",
                  clim_model, " distributions of ", message_spp, ".")
        } else {
          # Calculating shift along eastern edge, in km, in each latitudinal band
          future_e <- as.matrix(future_lons[, c("max_future", "y")])
          current_e <- as.matrix(future_lons[, c("max_current", "y")])
          # Distances in meters
          dists_e <- terra::distance(future_e, current_e, 
                                     lonlat = TRUE, pairwise = TRUE)
          # Calculate shift (positive values = east) and convert to km
          shift_e_km <- ifelse(future_e[, "max_future"] > current_e[, "max_current"], 
                               1 * dists_e, -1 * dists_e) / 1000
          stats$lon_max_shift[row_index] <- round(median(shift_e_km))
          
          # Calculating shift along western edge, in km, in each latitudinal band
          future_w <- as.matrix(future_lons[, c("min_future", "y")])
          current_w <- as.matrix(future_lons[, c("min_current", "y")])
          # Distances in meters
          dists_w <- terra::distance(future_w, current_w, 
                                     lonlat = TRUE, pairwise = TRUE)
          # Calculate shift (positive values = east) and convert to km
          shift_w_km <- ifelse(future_w[, "min_future"] > current_w[, "min_current"], 
                               1 * dists_w, -1 * dists_w) / 1000
          stats$lon_min_shift[row_index] <- round(median(shift_w_km))
        }
      }
      
      # Combine current and future rasters to identify areas lost/gained
      # (1 = lost; 2 = gained; 3 = retained)
      current_lg <- terra::extend(current, ext(future))
      future_lg <- terra::classify(x = future, 
                                   rcl = matrix(c(1, 2), nrow = 1),
                                   other = NA)
      lg <- terra::rast(list(current_lg, future_lg))
      lg <- sum(lg, na.rm = TRUE)
      
      # Grab values for area lost, gained, and retained
      areas <- round(terra::expanse(lg, unit = "km", byValue = TRUE))
      if (1 %in% areas[, "value"]) {
        stats$area_lost[row_index] <- areas[areas[, "value"] == 1, "area"]
      } else {
        stats$area_lost[row_index] <- 0
      }
      if (2 %in% areas[, "value"]) {
        stats$area_gained[row_index] <- areas[areas[, "value"] == 2, "area"]
      } else {
        stats$area_gained[row_index] <- 0
      }
      if (3 %in% areas[, "value"]) {
        stats$area_retained[row_index] <- areas[areas[, "value"] == 3, "area"]
      } else {
        stats$area_retained[row_index] <- 0
      }
      # Create delta raster with 0 values:
      # Read in original overlap raster (to identify land areas with proper ext)
      full_overlap <- readRDS(overlap_files[j])
      # Reclassify, so all non-NA cells have a value of 0
      full_0 <- terra::classify(x = full_overlap,
                                rcl = matrix(c(0, Inf, 0), nrow = 1),
                                right = FALSE,
                                others = NA)
      delta <- terra::mosaic(full_0, lg, fun = "sum")
      # Save delta raster (maps created elsewhere)
      delta_file <- paste0("output/deltas/", nice_name, "-delta-",
                           distrib_short, "-", clim_model, ".rds")
      if (!(file.exists(delta_file) & replace_rasters == FALSE)) {
        saveRDS(object = delta,
                file = delta_file)
      }
    } # future scenario
  } # distribution type
} # species

# Write summary table to file
if (all_insects) {
  spp <- "allspp"
} else {
  spp <- paste0(length(insects), "spp")
}

filename <- paste0("output/summary-stats/overlap-summary-", 
                   spp, ".csv")

if (!(file.exists(filename) & replace_table == FALSE)) {
  write.csv(x = stats,
            file = filename, 
            row.names = FALSE)
}
