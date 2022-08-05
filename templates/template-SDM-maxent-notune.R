# A template for building MaxEnt species distribution models (no tuning) for 
# a single species
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-08-05

require(raster)
require(terra)
require(dplyr)

# Load up the functions from the functions folder
source(file = "load_functions.R")

# genus <- "GENUS"
# species <- "SPECIES"
genus <- "Papilio"
species <- "rumiko"

set.seed(20220805)

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
full_data <- read.csv(file = pa_file)

# A note to let folks know you are alive
n_obs <- nrow(full_data %>% dplyr::filter(pa == 1))
message("\n**** Running MaxEnt SDM on ", n_obs, " observations of ", 
        species_name, " ****")

# Grab worldclim data to use as predictors
predictors <- terra::rast(list.files(path = "data/wc2-1",
                                     pattern = ".tif$",
                                     full.names = TRUE))

# Extract bioclim data for presence/absence data; can take a moment
predictors <- terra::extract(x = predictors, 
                             y = full_data[, c("x", "y")], 
                             xy = TRUE) %>%
  select(-ID)

# Join bioclim data with original full_data (which has pa and fold info)
# Specifying join columns isn't necessary, but keeps things quiet
# Need to include distinct since there are often multiple gbif observations at 
# one location (leading to duplicated rows in predictors)
full_data <- full_data %>%
  dplyr::left_join(dplyr::distinct(predictors),
                   by = c("x" = "x", "y" = "y"))

# Arrange predictor columns in full_data (so they appear in order)
# use all_of to ensure all all bioclim variables are there
# We can drop x, y columns at this point
full_data <- full_data %>%
  dplyr::select(c("pa", "fold", all_of(paste0("bio", 1:19))))

# Run MaxEnt model
maxent_model <- run_maxent_notune(full_data = full_data,
                                  verbose = FALSE)

# Save the model to file in output/models/
model_file <- paste0("output/SDMs/", nice_name,
                     "-maxent-notune.rds")
saveRDS(object = maxent_model,
        file = model_file)

message(paste0("MaxEnt model for ", species_name, 
               " complete; saved to ", model_file))
