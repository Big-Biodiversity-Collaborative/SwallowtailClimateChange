# Summarize rasters delineating geographic overlap between the predicted 
# distributions of each insect species and its host plants
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-12-28

require(terra)
require(tidyr)
require(dplyr)
require(stringr)

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
  insects <- c("Papilio rumiko", "Papilio cresphontes")
}

# Remove insects from list that have an insufficient number of filtered 
# occurrence records and therefore should not be included in analyses. 
# (Technically, they shouldn't have overlap rasters, but some obsolete 
# files may still remain in the output/overlaps folder)
exclude <- species_info$species[species_info$pa_csv == "no"]
insects <- insects[!insects %in% exclude]

# Interested in summarizing values for (1) all areas predicted to be suitable 
# for the insect (overlap raster values 1 and 3), and (2) areas predicted to be 
# suitable for the insect and at least one of its host plants (overlap = 3).
distributions <- c("total insect", "insect + host")

# Logical indicating whether to use a t-test evaluating evidence of a northern 
# shift in an insect's distribution 
t_test <- FALSE

# Create table to hold summary statistics
summary <- as.data.frame(expand_grid(insect = insects, 
                                       distribution = distributions,
                                       climate = climate_names_short)) %>%
  mutate(area_sqkm = NA,
         percent_current = NA,
         lat_max = NA,
         Nlat_med = NA,  
         Nshift_mn_deg = NA,
         Nshift_md_deg = NA,
         Nshift_mn_km = NA,
         Nshift_md_km = NA)
if (t_test) {
  summary$pvalue_Nshift = NA
}

# area = total land area (sq km) predicted to be suitable
# percent_current = percent of area predicted suitable in current time period 
  # that's predicted to be suitable in the future
# lat_max = Northern extent of predicted range
# Nlat_med = median latitude among the northernmost cells in each 
  # longitudinal band
# Nshift_mn_deg = mean northward shift (degrees) in each longitudinal band 
  # (only for bands that have >=1 cell considered suitable in both time periods)
# Nshift_md_deg  = median northward shift (degrees) in each longitudinal band 
  # (only for bands that have >=1 cell considered suitable in both time periods)
# Nshift_mn_km = mean northward shift (km) in each longitudinal band 
  # (only for bands that have >=1 cell considered suitable in both time periods)
# Nshift_md_km  = median northward shift (km) in each longitudinal band 
  # (only for bands that have >=1 cell considered suitable in both time periods)
# pvalue_Nshift = p-value from one-sided t-test evaluating evidence of a northern 
  # shift

# Loop through insect species
for (i in 1:length(insects)) {
  nice_name <- insects[i] %>%
    str_replace(pattern = " ", replacement = "_") %>%
    tolower()
  short_name <- insects[i] %>%
    str_replace(pattern = "Papilio", replacement = "P.")
  
  cat(paste0("Summarizing overlap rasters for ", short_name, ".\n"))
  
  # Grab overlap rasters for insect
  overlap_files <- list.files("output/overlaps", 
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
    
    # For all areas predicted suitable for the insect: reclassify cells in 
    # overlap rasters that were equal to 1 or 3 as 1 (NA everywhere else)
    allinsect_list[[j]] <- terra::classify(x = allinsect_list[[j]],
                                           rcl = rbind(c(1, 1), c(3, 1)),
                                           others = NA)
    
    # For all areas predicted suitable for the insect and one or more host
    # plants: reclassify cells in overlap rasters that were equal to 3 as 1 
    # (NA everywhere else)
    insecthost_list[[j]] <- terra::classify(x = insecthost_list[[j]],
                                            rcl = matrix(c(3, 1), nrow = 1),
                                            others = NA)
  }

  # Loop through distribution types
  for (distribution in distributions) {
    
    if (distribution == "total insect") {
      raster_list <- allinsect_list
      message_spp <- short_name
    } else {
      raster_list <- insecthost_list
      message_spp <- paste0(short_name, " + hostplants")
    }

    # Do calculations for current time period
      row_index_c <- which(summary$insect == insects[i] & 
                             summary$distribution == distribution &
                             summary$climate == "current")
      current <- raster_list[[1]]
      
      # Calculate land area (in sq km) and add to summary
      summary$area_sqkm[row_index_c] <- round(terra::expanse(current, 
                                                             unit = "km"))
      
      # Create a data frame with lat/long for all non-NA cells
      current_df <- terra::as.data.frame(current, xy = TRUE, na.rm = TRUE)

      # Calculate maximum latitude for each longitudinal band
      current_lats <- current_df %>%
        group_by(x) %>%
        summarize(max_current = max(y)) %>%
        data.frame()
      
      # Calculate maximum latitude (across entire predicted range) and add to 
      # summary
      summary$lat_max[row_index_c] <- round(max(current_lats$max_current), 2)
      
      # Calculate median latitude along northern boundary and add to summary
      summary$Nlat_med[row_index_c] <- round(median(current_lats$max_current), 2)
      
    # Loop through future climate scenarios
    for (j in 2:nrow(climate_models)) {
      
      clim_model <- climate_names_short[j]
      row_index <- which(summary$insect == insects[i] & 
                           summary$distribution == distribution &
                           summary$climate == clim_model)
      future <- raster_list[[j]]

      # Calculate land area (in sq km) and add to summary
      summary$area_sqkm[row_index] <- round(terra::expanse(future, unit = "km"))      

      if (summary$area[row_index] == 0) {
        message("No areas predicted suitable for ", message_spp, ", ", 
                clim_model,".")
      } else {
        # Calculate maximum latitude for each longitudinal band
        future_df <- terra::as.data.frame(future, xy = TRUE, na.rm = TRUE)
        future_lats <- future_df %>%
          group_by(x) %>%
          summarize(max = max(y)) %>%
          data.frame()
        
        # Calculate maximum latitude and add to summary
        summary$lat_max[row_index] <- round(max(future_lats$max), 2)
        
        # Calculate median latitude along northern boundary and add to summary
        summary$Nlat_med[row_index] <- round(median(future_lats$max), 2)
        
        # Calculate percent of current distribution that's suitable in future
        future <- terra::crop(future, current)
        overlay <- terra::expanse(current + future, unit = "km")
        perc_current <- overlay/summary$area[row_index_c] * 100
        summary$percent_current[row_index] <- round(perc_current, 2)
        
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
          # Calculating northward shift, in degrees, in each longitudinal band
          future_lats$shift <- future_lats$max - future_lats$max_current
          summary$Nshift_mn_deg[row_index] <- round(mean(future_lats$shift), 2)
          summary$Nshift_md_deg[row_index] <- round(median(future_lats$shift), 2)
          
          # Calculating northward shift, in km, in each longitudinal band
          x <- as.matrix(future_lats[, c("x", "max")])
          y <- as.matrix(future_lats[, c("x", "max_current")])
          # Distances in meters
          dists <- terra::distance(x, y, lonlat = TRUE, pairwise = TRUE)
          # Calculate northward shift and convert to km
          shift_km <- ifelse(x[, "max"] > y[, "max_current"], 
                             1 * dists, -1 * dists) / 1000
          summary$Nshift_mn_km[row_index] <- round(mean(shift_km))
          summary$Nshift_md_km[row_index] <- round(median(shift_km))
          
          # Paired t-test to see if edge has shifted north
          if (t_test) {
            if (nrow(future_lats) == 1) {
              message("Cannot perform t-test for ", message_spp, ", ", 
                      clim_model, ". Only one longitudinal band.")
            } else {
              ttest_p <- t.test(x = future_lats$max,
                                y = future_lats$max_current,
                                paired = TRUE,
                                alternative = "greater")$p.value
              summary$pvalue_Nshift[row_index] <- round(ttest_p, 3)
            }
          }
        }
      }
    }
  }
}  

# Write summary table to file
datestamp <- Sys.Date()
datestamp <- str_remove_all(datestamp, "-")

if (all_insects) {
  spp <- "allspp"
} else {
  spp <- paste0(length(insects), "spp")
}

filename <- paste0("output/summary-stats/overlap-summary-", 
                   spp, "-", datestamp, ".csv")

write.csv(x = summary,
          file = filename, 
          row.names = FALSE)
