# Test run of Maxent model using dismo package, no parameter tuning
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-07-11

require(raster)
require(dplyr)

# Load functions from the functions folder
source(file = "load_functions.R")
source("development/functions/run_maxent_notune.R")

genus <- "Papilio"
species <- "rumiko"

set.seed(20220614)

# Name for reporting and looking up info in files
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

# Load in presence-absence dataset
pa_file <- paste0("development/data/presence-absence/",
                  nice_name,
                  "-pa.csv")
# if (!file.exists(pa_file)) {
#   unzip(zipfile = "development/data/pa-datasets.zip")
# }
pa_data <- read.csv(file = pa_file)

# Only need geo coordinates, so extract those (in x, y order)
pa_locs <- pa_data %>%
  dplyr::select(x, y)

# Grab worldclim data to use as predictors
predictors <- raster::stack(list.files(path = "data/wc2-1",
                                       pattern = ".tif$",
                                       full.names = TRUE))

# Extract climate values
pa_climate <- raster::extract(x = predictors, y = pa_locs) 

# Combine pa and fold data with climate values
full_data <- cbind(pa_data, pa_climate)

# Run support vector machine model
maxent_model <- run_maxent_notune(full_data = full_data,
                                  verbose = TRUE)

# Save the model to file in output/models/
model_file <- paste0("development/output/SDMs/", nice_name,
                     "-sdm-maxent.rds")
saveRDS(object = maxent_model,
        file = model_file)

message(paste0("Maxent model for ", species_name, 
               " complete; saved to ", model_file))
