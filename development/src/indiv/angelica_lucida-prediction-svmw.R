# Generate predicted distributions for Angelica lucida from a weighted SVM
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-07-05

require(raster)

# Load up the functions from the functions folder
source(file = "load_functions.R")

genus <- "Angelica"
species <- "lucida"

model <- "svmw"

# Name for reporting
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

message(paste0("Predicting presence / absence based on ", toupper(model), 
               " for ", species_name))
# To keep track if any returned presence / absence rasters were NULL
success <- TRUE 

################################################################################
# Start by doing prediction for current conditions (worldclim data)
current_predictors <- raster::stack(list.files(path = "data/wc2-1",
                                               pattern = ".tif$",
                                               full.names = TRUE))


current_preds <- predict_pa(nice_name = nice_name,
                            model = model,
                            predictors = current_predictors)

# Need to figure out whether we're returning a pa raster (0/1)
# or a raster with predicted values.  If the latter, we need to load 
# the model so the threshold value is available...


  # Plot predictions
  plot(current_preds)
  
  # Make a raster of presence / absence values
  pa_raster <- current_preds > sdm_model$thresh
   # Can't do this without loading model list 
   # Should we do that in this script instead of 

if (!is.null(current_pa)) {
  # Save presence / absence raster
  current_pa_file <- paste0("output/distributions/", nice_name,
                            "-distribution-",
                            model, "-current.rds")
  saveRDS(object = current_pa,
          file = current_pa_file)
} else {
  success <- FALSE
}

