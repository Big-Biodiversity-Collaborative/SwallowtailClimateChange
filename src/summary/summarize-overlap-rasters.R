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

# Load list of climate models
climate_models <- read.csv(file = "data/climate-models.csv")
climate_names_short <- climate_models$name %>%
  str_remove(pattern = "ensemble_")

# Logical indicating whether to summarize overlap rasters for all species or 
# just a subset of insects
all_insects <- FALSE

# Extract species names
if (all_insects) {
  insects <- unique(ih$insect)
} else {
  # If not all insects, identify which insects to include
  insects <- c("Papilio rumiko", "Papilio cresphontes")
}

# Interested in summarizing values for (1) all areas predicted to be suitable 
# for the insect (overlap raster values 1 and 3), and (2) areas predicted to be 
# suitable for the insect and at least one of its host plants (overlap = 3).
distributions <- c("total insect", "insect + host")

# Create table to hold summary statistics
summaries <- as.data.frame(expand_grid(insect = insects, 
                                       distribution = distributions,
                                       climate = climate_names_short)) %>%
  mutate(area = NA,
         percent_current = NA,
         max_Nlat = NA,
         median_Nlat = NA,  
         mean_Nshift = NA,
         median_Nshift = NA,
         pvalue_Nshift = NA)

# area = total land area (sq km) predicted to be suitable
# percent_current = percent of area predicted suitable in current time period 
  # that's predicted to be suitable in the future
# max_Nlat = Northern extent of predicted range
# median_Nlat = median latitude among the northernmost cells in each 
  # longitudinal band
# mean_Nshift = mean northward shift (degrees) in each longitudinal band (only 
  # for bands that have >=1 cell considered suitable in both time periods)
# median_Nshift  = median northward shift (degrees) in each longitudinal band 
  # (only for bands that have >=1 cell considered suitable in both time periods)
# pvalue_Nshift = p-value from one-sided t-test evaluating evidence of a northern 
  # shift

# Loop through insect species
for (i in 1:length(insects)) {
  
  nice_name <- insects[i] %>%
    str_replace(pattern = " ", replacement = "_") %>%
    tolower()
  
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
    } else {
      raster_list <- insecthost_list
    }

    # Do calculations for current time period
      row_index_c <- which(summaries$insect == insects[i] & 
                             summaries$distribution == distribution &
                             summaries$climate == "current")
      current <- raster_list[[1]]
      
      # Calculate land area (in sq km) and add to summaries
      summaries$area[row_index_c] <- round(terra::expanse(current, unit = "km"))
      
      # Create a data frame with lat/long for all non-NA cells
      current_df <- terra::as.data.frame(current, xy = TRUE, na.rm = TRUE)

      # Calculate maximum latitude for each longitudinal band
      current_lats <- current_df %>%
        group_by(x) %>%
        summarize(max_current = max(y)) %>%
        data.frame()
      
      # Calculate maximum latitude (across entire predicted range) and add to 
      # summaries
      summaries$max_Nlat[row_index_c] <- max(current_lats$max_current)
      
      # Calculate median latitude along northern boundary and add to summaries
      summaries$median_Nlat[row_index_c] <- median(current_lats$max_current)
      
    # Loop through future climate scenarios
    for (j in 2:nrow(climate_models)) {
      
      clim_model <- climate_names_short[j]
      row_index <- which(summaries$insect == insects[i] & 
                           summaries$distribution == distribution &
                           summaries$climate == clim_model)
      future <- raster_list[[j]]

      # Calculate land area (in sq km) and add to summaries
      summaries$area[row_index] <- round(terra::expanse(future, unit = "km"))      

      # Calculate maximum latitude for each longitudinal band
      future_df <- terra::as.data.frame(future, xy = TRUE, na.rm = TRUE)
      future_lats <- future_df %>%
        group_by(x) %>%
        summarize(max = max(y)) %>%
        data.frame()
      
      # Calculate maximum latitude and add to summaries
      summaries$max_Nlat[row_index] <- max(future_lats$max)
      
      # Calculate median latitude along northern boundary and add to summaries
      summaries$median_Nlat[row_index] <- median(future_lats$max)
      
      # Calculate percent of current distribution that's suitable in future
      future <- terra::crop(future, current)
      overlay <- terra::expanse(current + future, unit = "km")
      perc_current <- overlay/summaries$area[row_index_c] * 100
      summaries$percent_current[row_index] <- round(perc_current, 2)
      
      # Join future and current latitudinal data (only including longitudinal 
      # bands that have suitable areas in both time periods)
        # Rounding longitudes first to ensure there are matching values
        current_lats$x <- round(current_lats$x, 4)
        future_lats$x <- round(future_lats$x, 4)
        future_lats <- inner_join(current_lats, future_lats, by = "x")
        
      if (nrow(future_lats) == 0) {
        message("None of the same longitudinal bands are in the current and future distributions for ",
                insects[i], ", ", clim_model, ".")
      } else {
        
        # Calculating shift in each longitudinal band
        future_lats$shift <- future_lats$max - future_lats$max_current
        summaries$mean_Nshift[row_index] <- round(mean(future_lats$shift), 2)
        summaries$median_Nshift[row_index] <- round(median(future_lats$shift), 2)
        # Paired t-test to see if edge has shifted north
        ttest_p <- t.test(x = future_lats$max,
                          y = future_lats$max_current,
                          paired = TRUE,
                          alternative = "greater")$p.value
        summaries$pvalue_Nshift[row_index] <- round(ttest_p, 3)
      }
    }
  }
}  

# Write summary table to file
# TODO: figure out where and how we want to save this information. Leaving 
# this commented out for now.

# write.csv(x = summaries,
#           file = "output/overlap_summaries.csv",
#           row.names = FALSE)
