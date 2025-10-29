# Investigation of changes underlying gain/loss of area in forecasts
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-10-27

# Want to know if gain/loss of suitable area for an insect is due to gain/loss
# of suitable area for the insect, gain/loss of suitable area for at least one 
# host plant, or gain/loss for both.

require(dplyr)
require(tidyr)
require(terra)

# Get list of insects
ih <- read.csv(file = "data/insect-host.csv")
insects <- unique(ih$insect)

# Get list of climate models
climate_models <- read.csv(file = "data/climate-models.csv")
# For our purposes, we do not need the current climate model
climate_models <- climate_models %>%
  filter(name != "current")

# data frame to hold values
areas_df <- tidyr::crossing(insect = insects, 
                            climate = climate_models$name) %>%
  mutate(area_current = NA_real_,
         area_lost_both = NA_real_,
         area_lost_insect = NA_real_,
         area_lost_plant = NA_real_,
         area_gained_both = NA_real_,
         area_gained_insect = NA_real_,
         area_gained_plant = NA_real_)

for (insect in insects) {
  nice_name <- tolower(gsub(pattern = " ",
                            replacement = "_",
                            x = insect))
  
  # Incoming rasters have the following values:
  # 0 = (Insect and hosts absent) Insect and all host plants predicted absent
  # 1 = (1 host only) Insect predicted absent, only 1 host predicted present
  # 2 = (2 or more hosts) Insect predicted absent, >= 2 hosts predicted present
  # 3 = (Insect, no hosts) Insect predicted present, all hosts predicted absent
  # 4 = (Insect, only 1 host) Insect and only 1 host predicted present
  # 5 = (Insect, 2 or more hosts) Insect and >= 2 hosts predicted present
  current_file <- paste0("output/overlaps/", nice_name,
                         "-overlap-current.rds")
  if (file.exists(current_file)) {
    message("Calculating change dynamics for ", insect, "\n")
    current <- readRDS(current_file)
    
    # Start by reclassifying them, current:
    # {0} -> 0 (suitable for neither)
    # {1,2} -> 1 (suitable for host, but not insect)
    # {3} -> 2 (suitable for insect, but not hosts)
    # {4,5} -> 4 (suitable for both insect and host)
    rcl_mat_current <- matrix(data = c(0, 1, 2, 3, 4, 5,
                                       0, 1, 1, 2, 4, 4),
                              nrow = 6, byrow = FALSE)
    current <- terra::classify(x = current,
                               rcl = rcl_mat_current)
    
    # While we are here, we can calculate current area suitable with at least one 
    # host
    current_areas <- round(terra::expanse(current, unit = "km", byValue = TRUE))
    area_current <- 0
    if (4 %in% current_areas$value) {
      area_current <- current_areas$area[current_areas$value == 4]
    }
    # And update rows for this species (repeated values, but fine)
    areas_df$area_current[areas_df$insect == insect] <- area_current
    
    for (climate_model in climate_models$name) {
      
      row_index <- which(areas_df$insect == insect &
                           areas_df$climate == climate_model)
      
      future_file <- paste0("output/overlaps/", nice_name,
                            "-overlap-", climate_model, ".rds")
      if (file.exists(future_file)) {
      future <- readRDS(future_file)
      
      # Forecast:
      # {0} -> 0 (suitable for neither)
      # {1,2} -> 8 (suitable for host, but not insect)
      # {3} -> 16 (suitable for insect, but not hosts)
      # {4,5} -> 32 (suitable for both insect and host)
      rcl_mat_future <- matrix(data = c(0, 1, 2, 3, 4, 5,
                                        0, 8, 8, 16, 32, 32),
                               nrow = 6, byrow = FALSE)
      future <- terra::classify(x = future,
                                rcl = rcl_mat_future)
      
      # Expand current to get right extent
      current <- extend(current, future)
      
      # Add current and forecast rasters together to get new raster with combined 
      # informative values. We care about cells with only certain values. Those 
      # dealing with loss of area marked with (LOSS), gain of area marked with (GAIN)
      # Current | Forecast
      #       0 |        0 = 0
      #       1 |        0 = 1
      #       2 |        0 = 2
      #       4 |        0 = 4 LOSS: both -> area_lost_both
      #       0 |        8 = 8
      #       1 |        8 = 9
      #       2 |        8 = 10
      #       4 |        8 = 12 LOSS: insect only -> area_lost_insect
      #       0 |        16 = 16
      #       1 |        16 = 17
      #       2 |        16 = 18
      #       4 |        16 = 20 LOSS: plant only -> area_lost_plant
      #       0 |        32 = 32 GAIN: both -> area_gained_both
      #       1 |        32 = 33 GAIN: insect only -> area_gained_insect
      #       2 |        32 = 34 GAIN: plant only -> area_gained_plant
      #       4 |        32 = 36
      raster_sum <- sum(terra::rast(list(current, future)), 
                        na.rm = TRUE)
      
      # Make things simpler for us by only keeping those values of interest and give 
      # those things names to make it easier
      
      area_names <- data.frame(id = c(4, 12, 20, 32, 33, 34),
                               name = c("area_lost_both",
                                        "area_lost_insect",
                                        "area_lost_plant",
                                        "area_gained_both",
                                        "area_gained_insect",
                                        "area_gained_plant"))
      
      raster_sum[!(raster_sum %in% area_names$id)] <- NA
      
      # Calculate area for each of the above categories
      areas <- round(terra::expanse(raster_sum, unit = "km", byValue = TRUE))
      
      # Add area values with the names and transform to wide
      areas <- areas %>%
        full_join(area_names, by = join_by(value == id)) %>%
        select(name, area) %>%
        pivot_wider(names_from = name, values_from = area)
      
      # Add those values to the larger data frame. This is so ugly.
      areas_df$area_lost_both[row_index] <- areas$area_lost_both
      areas_df$area_lost_insect[row_index] <- areas$area_lost_insect
      areas_df$area_lost_plant[row_index] <- areas$area_lost_plant
      areas_df$area_gained_both[row_index] <- areas$area_gained_both
      areas_df$area_gained_insect[row_index] <- areas$area_gained_insect
      areas_df$area_gained_plant[row_index] <- areas$area_gained_plant

      } # End conditional for future climate mdoel raster exists for this sp.
      else {
        message("No ", climate_model, " raster on file for ", insect, "\n")
      }
    } # end iterating over climate models
    
  } # End conditional for current raster exists for this species
  else {
    message("No current climate raster on file for ", insect, "\n")
  }
} # end iterating over insects

# Before sending to file, let's remove "emsemble_" from all those climate names
areas_df <- areas_df %>%
  mutate(climate = gsub(pattern = "ensemble_",
                        replacement = "",
                        x = climate))

write.csv(file = "output/summary-stats/overlap-changes.csv",
          x = areas_df,
          row.names = FALSE)
