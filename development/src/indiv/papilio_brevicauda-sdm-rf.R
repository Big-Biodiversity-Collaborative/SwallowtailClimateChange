# Test run of Maxent model using dismo package, no parameter tuning
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-07-11

require(dplyr)

# Load functions from the functions folder
source(file = "load_functions.R")
source("development/functions/run_rf.R")

genus <- "Papilio"
species <- "brevicauda"

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
full_data <- read.csv(file = pa_file)

# Run support vector machine model
# TODO update with rf function
rf_model <- run_rf(full_data = full_data,
                                  verbose = FALSE)

# Save the model to file in output/models/
model_file <- paste0("development/output/SDMs/", nice_name,
                     "-sdm-rf.rds")
saveRDS(object = rf_model,
        file = model_file)

message(paste0("Random forest model for ", species_name, 
               " complete; saved to ", model_file))
