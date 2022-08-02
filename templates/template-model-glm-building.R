# A template for building GLM species distribution models for a single species
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-02

require(raster)
require(dplyr)  # load *after* raster for easier use of select
require(dismo)  # background point sampling

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
n_obs <- nrow(full_data %>% filter(pa == 1))
message(paste0("\n**** Running GLM SDM on ", n_obs, " observations of ", 
               species_name, " ****"))

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
