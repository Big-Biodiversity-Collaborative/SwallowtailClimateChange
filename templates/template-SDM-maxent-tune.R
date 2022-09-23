# A template for building Maxent species distribution models (with tuning) for a single species
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-09-23

require(raster)
require(terra)
require(dplyr)

# Load up the functions from the functions folder
source(file = "load_functions.R")

genus <- "GENUS"
species <- "SPECIES"

set.seed(20220916)

# Name for reporting and looking up info in files
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

# Load in presence/absence data
pa_file <- paste0("data/gbif/presence-absence/",
                  nice_name,
                  "-pa.csv")
# If dataset isn't in presence-absence folder, unzip gbif-pa
if (!file.exists(pa_file)) {
  unzip(zipfile = "data/gbif-pa.zip")
}
pa_data <- read.csv(file = pa_file)

# Grab worldclim data to use as predictors
predictors <- terra::rast(list.files(path = "data/wc2-1",
                                     pattern = ".tif$",
                                     full.names = TRUE))

# A note to let folks know you are alive
n_obs <- nrow(pa_data %>% dplyr::filter(pa == 1))
message("
**** Running Maxent SDM on ", n_obs, " observations of ", 
        species_name, " ****")

# Run and evaluate Maxent models with different tuning parameters; select
# the best one with optimal parameter values; evaluate.
# Note: this can take a while to run (eg, 3-5 min for P. rumiko)
maxent_model <- run_maxent_tune(pa_data = pa_data,
                                predictors = predictors, 
                                criteria = "AICc",
                                verbose = FALSE)

# Save the model to file in output/models/
model_file <- paste0("output/SDMs/", nice_name,
                     "-maxent-tune.rds")
saveRDS(object = maxent_model,
        file = model_file)

message(paste0("Maxent model for ", species_name, 
               " complete; saved to ", model_file))
