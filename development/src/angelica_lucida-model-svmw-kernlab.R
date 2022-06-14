# Test run of weighted SVM model using the kernlab package
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-06-14

require(dplyr)

# Load up the functions from the functions folder
source(file = "load_functions.R")
source("development/functions/run_svmw_kernlab.R")

genus <- "Angelica"
species <- "lucida"

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

# For now (until changes are made to data-gbif-qa.R), remove records with NAs
full_data <- full_data %>%
  dplyr::filter(!is.na(bio1))

# Run support vector machine model
svmwk_model <- run_svmw_kernlab(full_data = full_data,
                                verbose = TRUE)

# Save the model to file in output/models/
model_file <- paste0("output/models/", nice_name,
                     "-model-svmwk-current.rds")
saveRDS(object = svmwk_model,
        file = model_file)

message(paste0("SVM model for ", species_name, 
               " complete; saved to ", model_file))
