# Calculate percent of contemporary range predicted to be suitable in forecasts
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-10-19

library(raster)
library(dplyr)

# Load up the functions from the functions folder, using stack_raster for 
# raster math
source(file = "load_functions.R")

# Calculate the percent of current (predicted) range that is predicted to be 
# occupied (present) for under each of the forecast climate models. 
# Differs from calculate-range-sizes and calculate-range-changes in that it 
# does not just calculate total area for each time period / climate model and 
# compare area values, but rather uses contemporary predicted range as 
# reference group and calculates how much of that area is predicted to be 
# occupied under different forecast conditions.

# Should read in rasters from output/overlaps/ and base and do comparisons for 
# (1) total insect area (values of 1 and 3 in raster) and (2) area in which 
# insect overlaps with at least on host (value of 3 in raster) (see 
# documentation in functions/overlap_raster.R for explanation of raster values)

sdm_method <- "glm"

# Load insects and get vector of unique names
insects <- read.csv(file = "data/insect-host.csv")
insects <- unique(insects$insect)

# Load climate model data frame
climate_models <- read.csv(file = "data/climate-models.csv")
# The current (contemporary) climate model is used as reference, so we will not 
# need to include that model in iterations
climate_models <- climate_models[climate_models$name != "current", ]

# Make data frame with one column for insect names, one column for shortened 
# forecast climate model names, one column which will hold the value name (such 
# as "total", "with_plant"), and one column for the values called "proportion"
results <- data.frame(insect = character(0),
                      gcm = character(0),
                      name = character(0),
                      proportion = numeric(0))
# TODO: how to generate this data frame before hand, where the only thing 
# missing is the proportion? Could be a tad more efficient than row binding, 
# but is it worth it?

# Iterate over insects
for (species_name in insects) {
  # TODO: For testing only (P. rumiko)
  # species_name <- insects[13]
  message("Calculating range proportions for ", species_name)
  
  # Make nice names that are used in file names
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species_name))

  # Check to see if predicted values for the current climate is there; if so 
  # proceed with comparisons
  current_raster_file <- paste0("output/overlaps/",
                                nice_name,
                                "-overlap-",
                                sdm_method,
                                "-current.rds")
  if (file.exists(current_raster_file)) {
    # Load current climate model overlap file for this insect
    current_raster <- readRDS(file = current_raster_file)
    
    # Convert raster values to:
    # 0, 2 -> 0  None present or only plant present
    # 1 -> 1     Insect present, plant absent
    # 3 -> 2     Insect and plant present
    current_raster[current_raster %in% c(0, 2)] <- 0
    current_raster[current_raster == 3] <- 2
    
    # Calculate cell areas
    current_cell_areas <- tapply(X = raster::area(current_raster),
                                 INDEX = current_raster[],
                                 FUN = sum)

    # Total area of cells with a 1 (insect range in the absence of plants) or a 
    # 2 (insect range in presence of plants)
    insect_total_current <- current_cell_areas["1"] + current_cell_areas["2"]
    # Total area of cells with a 2 (insect range that overlaps with plants)
    insect_plant_current <- current_cell_areas["2"]
  
    # Need to explicitly deal with case when there are zero cells
    if (length(insect_total_current) == 0) {
      insect_total_current <- 0
    }
    if (length(insect_plant_current) == 0) {
      insect_plant_current <- 0
    }

    # Iterate over all forecast climate models
    for (gcm_i in 1:nrow(climate_models)) {
      forecast_model_name <- climate_models$name[gcm_i]
      forecast_raster_file <- paste0("output/overlaps/",
                                    nice_name,
                                    "-overlap-",
                                    sdm_method, "-",
                                    forecast_model_name,
                                    ".rds")
      
      if (file.exists(forecast_raster_file)) {
        # Load forecast climate model overlap file
        forecast_raster <- readRDS(file = forecast_raster_file)
          
        # Convert raster values to:
        # 0, 2 -> 0  None present or only plant present
        # 1 -> 4     Insect present, plant absent
        # 3 -> 8     Insect and plant present
        forecast_raster[forecast_raster %in% c(0, 2)] <- 0
        forecast_raster[forecast_raster == 1] <- 4
        forecast_raster[forecast_raster == 3] <- 8

        # Add rasters together
        both_rasters <- stack_rasters(r = list(current_raster, forecast_raster),
                                      out = "total")
          
        # On the summed raster, calculate
        # (1) total area of current range of insect (including areas with and 
        #     without plants) occupied in forecast by insect (including areas 
        #     with and without plants). 
        #     These are values: (1 + 4) and (2 + 8), where parentheses each 
        #     have a current raster value (the first number) and a forecast 
        #     raster value (the second number). So this value will be the total 
        #     area of the cells with values of 5 and 10
        # (2) total area of current range of insect + plant occupied in 
        #     forecast by insect + plant. These are values (2 + 8) = 10 in the 
        #     summed raster.
        both_cell_areas <- tapply(X = raster::area(both_rasters),
                                  INDEX = both_rasters[],
                                  FUN = sum)
        
        insect_total_both <- both_cell_areas["5"] + both_cell_areas["10"]
        insect_plant_both <- both_cell_areas["10"]
        
        # Need to explicitly deal with case when there are zero cells
        if (length(insect_total_both) == 0) {
          insect_total_both <- 0
        }
        if (length(insect_plant_both) == 0) {
          insect_plant_both <- 0
        }

        # Do proportion calculations 
        proportion_total <- insect_total_both / insect_total_current
        proportion_plant <- insect_plant_both / insect_plant_current
        
        # Store these proportions in results data frame for this insect
        # insect, gcm, name, proportion
        results <- results %>%
          dplyr::bind_rows(data.frame(insect = rep(species_name, times = 2),
                                      gcm = rep(forecast_model_name, times = 2),
                                      name = c("total", "with_plant"),
                                      proportion = c(proportion_total, 
                                                     proportion_plant)))
      } else {
        warning("No overlap file for ", climate_models$description[gcm_i], 
                " model for ", species_name)
      }
    } # end iterating over forecast climate models
  } else {
    # No predictions for insect under current climate model, message and move 
    # on to next insect
    warning("No current climate model overlap file found for ", species_name)
  }
}

# Write results file to output/areas
write.csv(x = results,
          file = paste0("output/areas/delta-contemporary-",
                        sdm_method,
                        ".csv"),
          row.names = FALSE)
