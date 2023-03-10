# A template for predicting probability of occurrence and the distribution of a 
# single species from a LASSO regression model
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-10-12

require(raster)
require(Matrix)
require(terra)

# Load up the functions from the functions folder
source(file = "load_functions.R")

genus <- "GENUS"
species <- "SPECIES"

sdm_method <- "lasso"

# Logical to indicate whether or not rasters with probabilities should be saved
save_probs <- FALSE

# Name for reporting
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

message("Predicting presence / absence based on ", sdm_method, " for ", 
        species_name)

# Grab the appropriate sdm model object from disk
sdm_file <- paste0("output/SDMs/", nice_name, "-", sdm_method, ".rds")
if (!file.exists(sdm_file)) {
  warning("No model file for ", species_name, " on disk; no predictions made.")
} else { # Only proceed if we can get the SDM file
  sdm_model <- readRDS(file = sdm_file)
  
  ########################################
  # Iterate over all climate models listed in data/climate-models.csv
  climate_models <- read.csv(file = "data/climate-models.csv")
  
  # apply over all rows in climate_models data frame
  success <- apply(X = climate_models, # each row passed one by one to climate_model
                   MARGIN = 1,
                   FUN = function(climate_model, sdm_model, nice_name, sdm_method, save_probs) {
                     # Use this as return value, toggling to TRUE iff 
                     # presence/absence raster for this climate model is 
                     # created
                     pa_success <- FALSE
                     model_name <- climate_model["name"]
                     # model_yr <- climate_model["yr"]
                     model_ssp <- climate_model["ssp"]
                     message("\tRunning ", model_name)
                     preds <- predict_sdm(nice_name = nice_name,
                                          model = sdm_model$model,
                                          sdm_method = sdm_method,
                                          yr = as.character(climate_model["yr"]),
                                          ssp = as.character(model_ssp),
                                          stand_obj = sdm_model$standardize_objects,
                                          quad = sdm_model$quad)
                     
                     # Save raster with probabilities (only for current and ssp370 scenarios)
                     if (save_probs & !is.null(preds) & (is.na(model_ssp) | model_ssp == "370")) {
                       preds_file <- paste0("output/predicted-probabilities/", nice_name,
                                            "-pred-probs-",
                                            sdm_method, "-", 
                                            model_name, ".rds")
                       saveRDS(object = preds,
                               file = preds_file)
                     }
                     
                     # Make a raster of presence / absence values
                     pa <- preds > sdm_model$thresh
                     
                     if (!is.null(pa)) {
                       # Save presence / absence raster
                       pa_file <- paste0("output/distributions/", nice_name,
                                         "-distribution-",
                                         sdm_method, "-", 
                                         model_name, ".rds")
                       saveRDS(object = pa,
                               file = pa_file)
                       pa_success <- TRUE
                       rm(pa)
                     } else {
                       warning("No raster produced for ", species_name, " and ",
                               model_name, " model.")
                     }
                     rm(preds)
                     invisible(gc())
                     return(pa_success)
                   }, 
                   # additional arguments passed to anonymous function
                   sdm_model = sdm_model, 
                   nice_name = nice_name,
                   sdm_method = sdm_method,
                   save_probs = save_probs)
  
  ################################################################################
  # Reporting
  completion_message <- paste0("Prediction process based on ", sdm_method, 
                               " for ", species_name, " complete.")
  if (!all(success)) {
    completion_message <- paste0(completion_message, 
                                 " But one or more rasters were null; check for warnings.")
  }
  message(completion_message)
}
