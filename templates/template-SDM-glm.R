# A template for building GLM species distribution models for a single species
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-02

require(raster)
require(terra)
require(dplyr)

# Load up the functions from the functions folder
source(file = "load_functions.R")

genus <- "GENUS"
species <- "SPECIES"

set.seed(20210603)

# Name for reporting and looking up info in files
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

# Load in presence/absence data
pa_file <- paste0("data/gbif/presence-absence/",
                  nice_name,
                  "-pa.csv")
# TODO: Need to check to see if file exists and what to do if not
full_data <- read.csv(file = pa_file)

# A note to let folks know you are alive
n_obs <- nrow(full_data %>% dplyr::filter(pa == 1))
message("\n**** Running GLM SDM on ", n_obs, " observations of ", 
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
full_data <- full_data %>%
  dplyr::left_join(predictors, by = c("x" = "x", "y" = "y"))

# Arrange predictor columns in full_data (so they appear in order)
# use all_of to ensure all all bioclim variables are there
# We can drop x, y columns at this point
full_data <- full_data %>%
  dplyr::select(c("pa", "fold", all_of(paste0("bio", 1:19))))

# Run generalized linear model
glm_model <- run_glm(full_data = full_data,
                     verbose = FALSE)

# Save the model to file in output/models/
model_file <- paste0("output/SDMs/", nice_name,
                     "-glm.rds")
saveRDS(object = glm_model,
        file = model_file)

message(paste0("GLM model for ", species_name, 
               " complete; saved to ", model_file))
