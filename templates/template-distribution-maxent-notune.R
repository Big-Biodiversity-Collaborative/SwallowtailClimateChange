# A template for building predicted distributions for a single species from a Maxent model (no tuning)
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-08-05

require(raster)

# Load up the functions from the functions folder
source(file = "load_functions.R")

genus <- "GENUS"
species <- "SPECIES"

sdm_method <- "maxent-notune"

# Name for reporting
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

message("Predicting presence / absence based on ", toupper(sdm_method), " for ", 
        species_name)

# Grab the appropriate SDM model object from disk
sdm_file <- paste0("output/SDMs/", nice_name, "-", sdm_method, ".rds")
if (!file.exists(sdm_file)) {
  warning("No model file for ", species_name, " on disk; no predictions made.")
} else { # Only proceed if we can get the SDM file
  sdm_model <- readRDS(file = sdm_file)
  
  ########################################
  # Iterate over all climate models listed in data/climate-models.csv
  climate_models <- read.csv(file = "data/climate-models.csv")
  
  # To keep track if any returned presence / absence rasters were NULL
  success <- rep(x = FALSE, times = nrow(climate_models))

  for (i in 1:nrow(climate_models)) {
    model_name <- climate_models$name[i]
    model_yr <- climate_models$yr[i]
    model_ssp <- climate_models$ssp[i]
    message("\tRunning ", model_name)
    preds <- predict_sdm(nice_name = nice_name,
                         model = sdm_model$model,
                         sdm_method = sdm_method,
                         yr = as.character(model_yr),
                         ssp = as.character(model_ssp))
  
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
      success[i] <- TRUE
    } else {
      warning("No raster produced for ", species_name, " and ",
                     model_name, " model.")
    }
  } # end iterating over all global climate models
  
  ################################################################################
  # Reporting
  completion_message <- paste0("Prediction process based on ", 
                               toupper(sdm_method), 
                               " for ", species_name, " complete.")
  if (!all(success)) {
    completion_message <- paste0(completion_message, 
                                 " But one or more rasters were null; check for warnings.")
  }
  message(completion_message)
}
