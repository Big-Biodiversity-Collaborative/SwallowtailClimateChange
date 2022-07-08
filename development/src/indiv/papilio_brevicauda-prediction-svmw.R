# Generate predicted distributions for Angelica lucida from a weighted SVM
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-07-05

require(raster)

# Load up the functions from the functions folder
source(file = "load_functions.R")
source(file = "development/functions/predict_sdm.R")

genus <- "Papilio"
species <- "brevicauda"

model <- "svmw"

# Name for reporting
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

message(paste0("Predicting presence / absence based on ", toupper(model), 
               " for ", species_name))
# To keep track if any returned presence / absence rasters were NULL
success <- TRUE 

# Load the SDM model, which includes the threshold for determining 
# presence / absence
model_file <- paste0("development/output/SDMs/", nice_name,
                     "-sdm-", model,".rds")
if (!file.exists(model_file)) {
  warning(paste0("No model file found for ", species_name, "; ", toupper(model), 
                 " predictions cannot be made"))
}

sdm_model <- readRDS(file = model_file)

################################################################################
# Start by doing prediction for current conditions (worldclim data)
current_predictors <- raster::stack(list.files(path = "data/wc2-1",
                                               pattern = ".tif$",
                                               full.names = TRUE))

# Using predict_sdm to return predictions from SDM model
  # Note that this is different than predict_pa() function, which returned a  
  # presence-absence raster using the spec_sens threshold
current_preds <- predict_sdm(nice_name = nice_name,
                             model = sdm_model$model,
                             predictors = current_predictors)

# Map model predictions
# plot(current_preds)
# Note: I'm not sure what these values are/mean for the weighted SVM

# Make a raster of presence / absence values
current_pa <- current_preds > sdm_model$thresh

if (!is.null(current_pa)) {
  # Save presence / absence raster
  current_pa_file <- paste0("development/output/distributions/", 
                            nice_name,
                            "-distribution-",
                            model, "-current.rds")
  saveRDS(object = current_pa,
          file = current_pa_file)
} else {
  success <- FALSE
}

################################################################################
# Predict values under future conditions (2 emissions scenarios, 2 time periods)

ssps <- c("ssp245", "ssp370")
yrs <- c("2041", "2071")

for (ssp in ssps) {
  for (yr in yrs) {
    predictor_filepath <- paste0("data/ensemble/",
                                 ssp, "/",
                                 yr)
    predictors <- raster::stack(list.files(path = predictor_filepath,
                                           pattern = ".tif$",
                                           full.names = TRUE))  
    
    preds <- predict_sdm(nice_name = nice_name,
                         model = sdm_model$model,
                         predictors = predictors)
    
    # Make a raster of presence / absence values
    future_pa <- preds > sdm_model$thresh
    
    if (!is.null(future_pa)) {
      # Save presence / absence raster
      future_pa_file <- paste0("development/output/distributions/", 
                                nice_name,
                                "-distribution-",
                                model, "-",
                                ssp, "-",
                                yr, ".rds")
      saveRDS(object = future_pa,
              file = future_pa_file)
    } else {
      success <- FALSE
    }    
  }
}
