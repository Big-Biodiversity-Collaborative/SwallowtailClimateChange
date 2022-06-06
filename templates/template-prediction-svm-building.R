# A template for building predicted distributions for a single species from SVM
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-02

require(raster)

# Load up the functions from the functions folder
source(file = "load_functions.R")

genus <- "GENUS"
species <- "SPECIES"

method <- "svm"

# Name for reporting
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

message(paste0("Predicting presence / absence based on ", toupper(method), 
               " for ", species_name))

################################################################################
# Iterate over all climate models listed in data/climate-models.csv
climate_models <- read.csv(file = "data/climate-models.csv")

# To keep track if any returned presence / absence rasters were NULL
success <- rep(x = FALSE, times = nrow(climate_models))

for (i in 1:nrow(climate_models)) {
  model_name <- climate_models$name[i]
  model_directory <- paste0("data/", climate_models$directory[i])
  message(paste0("Running ", model_name, " from ", model_directory))
  predictors <- raster::stack(list.files(path = model_directory,
                                         pattern = ".tif$",
                                         full.names = TRUE))
  pa <- predict_pa(nice_name = nice_name,
                   model = method,
                   predictors = predictors)
  if (!is.null(pa)) {
    # Save presence / absence raster
    pa_file <- paste0("output/distributions/", nice_name,
                      "-distribution-",
                      method, "-", 
                      model_name, ".rds")
    saveRDS(object = pa,
            file = pa_file)
    success[i] <- TRUE
  } else {
    warning(paste0("No raster produced for ", species_name, " and ",
                   model_name, " model."))
  }
}

################################################################################
# Reporting
completion_message <- paste0("Prediction process based on ", toupper(method), 
                             " for ", species_name, " complete.")
if (!all(success)) {
  completion_message <- paste0(completion_message, 
                               " But one or more rasters were null; check for warnings.")
}
message(completion_message)
