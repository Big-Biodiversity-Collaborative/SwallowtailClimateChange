# A template for building GLM species distribution models for a single species
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-02

require(raster)
require(dplyr)  # load *after* raster for easier use of select
require(dismo)  # background point sampling

# Load up the functions from the functions folder
function_files <- list.files(path = "./functions", 
                             pattern = ".R$", 
                             full.names = TRUE)
for(fun_file in function_files) {
  source(file = fun_file)
}

genus <- "GENUS"
species <- "SPECIES"

set.seed(20210603)

# Name for reporting and looking up info in files
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

# Load in observation data
obs_file <- paste0("data/gbif/",
                   nice_name,
                   "-gbif.csv")
obs <- read.csv(file = obs_file)

# A note to let folks know you are alive
n_obs <- nrow(obs)
message(paste0("\n**** Running GLM SDM on ", n_obs, " observations of ", 
               species_name, " ****"))

# Get the geographic extent of this beast
obs_extent <- get_extent(data = obs)

# Will use bioclim data files to create masks for sampling resolution of 
# background points. Check for bioclim data and download if it isn't there
if (!file.exists("data/wc2-5/bio1.bil")) {
  bioclim_data <- raster::getData(name = "worldclim",
                                  var = "bio",
                                  res = 2.5,
                                  path = "data/")
  # Will load in the predictor variables as a RasterStack later, so removing 
  # to free up memory
  rm(bioclim_data)
}

# Using the first bil file to create a raster to use as mask for sampling 
# background points
bil_file <- list.files(path = "data/wc2-5", 
                       pattern = ".bil$", 
                       full.names = TRUE)[1]
mask <- raster(bil_file)
# Do not need this anymore
rm(bil_file)

# Use random sampling to generate pseudo-absence points
# mask:  Provides resolution of sampling points
# n:     Number of random points
# ext:   Spatially restricts sampling
# extf:  Expands sampling a little bit
background_points <- dismo::randomPoints(mask = mask,  
                                         n = nrow(obs),
                                         ext = obs_extent, 
                                         extf = 1.25)

# Will want to use this with dplyr::bind_rows, so convert to data frame
background_points <- as.data.frame(background_points)

# Make background points conform to column names of observed data
background_points <- background_points %>%
  dplyr::rename(longitude = x,
                latitude = y)

# Grab worldclim data to use as predictors
predictors <- raster::stack(list.files(path = "data/wc2-5",
                                       pattern = ".bil$",
                                       full.names = TRUE))

# Run generalized linear model
glm_model <- run_glm(obs = obs,
                     absence = background_points,
                     predictors = predictors,
                     verbose = FALSE)

# Save the model to file in output/models/
model_file <- paste0("output/models/", nice_name,
                     "-model-glm-current.rds")
saveRDS(object = glm_model,
        file = model_file)

message(paste0("GLM model for ", species_name, 
               " complete; saved to ", model_file))