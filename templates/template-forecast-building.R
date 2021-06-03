# A template for building predicted distributions for a single species
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-02

require(raster)
require(kernlab)  # because we're loading & predicting with an svm model

# Load up the functions from the functions folder
function_files <- list.files(path = "./functions", 
                             pattern = ".R$", 
                             full.names = TRUE)
for(fun_file in function_files) {
  source(file = fun_file)
}

genus <- "GENUS"
species <- "SPECIES"

# Name for reporting and looking up info in files
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

# For the paranoid
message(paste0("Predicting distribution of ", species_name, 
               " based on GFDL-ESM4_RCP45 forecast data."))

# Do forecast model, with GFDL-ESM4_RCP45 as forecast data
# Load forecast data, download if it does not exist
# For now, want to suppress warning messages about Discarded datum unknown...
suppressWarnings({
  gfdl_data <- raster::getData(name = "CMIP5",
                               var = "bio",
                               res = 2.5,
                               rcp = 45,
                               model = "GD",
                               year = 70,
                               path = "data/")
})

# Need to rename variables in forecast climate data so our predictions work 
# (these are the same names as the bioclim data, used for the creation of our 
# species distribution model)
names(gfdl_data) <- paste0("bio", 1:19)

# Check for the model; stop processing if it isn't there
model_file <- paste0("output/models/", nice_name, "-model-svm-current.rds")

# Also check for data, since we'll need to restrict predictions to extent of 
# organism in question
obs_file <- paste0("data/", nice_name, "-gbif.csv")

if (file.exists(model_file) & file.exists(obs_file)) {
  # Load the model in from file
  svm_model <- readRDS(file = model_file)
  
  # Want to restrict predictions to extent of beast
  obs_extent <- get_extent(data = read.csv(file = obs_file))
  
  # Predict probabilities with model and forecast climate data
  forecast_probs <- predict(gfdl_data,
                            svm_model$model,
                            ext = obs_extent)
  
  # Estimate presence / absence with threshold
  forecast_pa <- forecast_probs > svm_model$thresh

  # Write as a raster to file  
  forecast_pa_file <- paste0("output/distributions/",
                             nice_name,
                             "-distribution-svm-GFDL-ESM4_RCP45.rds")
  saveRDS(object = forecast_pa,
          file = forecast_pa_file)
  
  message(paste0("SVM model forecast predictions for ", species_name, 
                 " complete; saved to ", forecast_pa_file))
} else {
  message(paste0("One or more required files (model, observations) missing for ",
                 species_name, ". Skipping forecast."))
  if (!file.exists(obs_file)) {
    message(paste0("Could not find observations file ", obs_file))
  }
  if (!file.exists(model_file)) {
    message(paste0("Could not find model file ", model_file))
  }
}