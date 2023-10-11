# A template for running (or for Maxent, just saving) SDMs using all available data 
# Erin Zylstra
# ezylstra@arizona.edu
# 2023-10-03

require(stringr)
require(ENMeval)
require(raster)
require(terra)
require(dplyr)
require(flexsdm)
require(ecospat)
require(glmnet)
require(mgcv)
require(randomForest)
require(dismo)
require(gbm)

# Load up the functions from the functions folder
source(file = "load_functions.R")

genus <- "GENUS"
species <- "SPECIES"

# Name for reporting and looking up info in files
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

# Load evaluation metrics from CV models
eval_file <- paste0("output/eval-metrics/", nice_name, "-CVevals.csv")
evals <- read.csv(eval_file, header = TRUE)

# Load in presence/pseudo-absence data
pa_file <- paste0("data/gbif/presence-absence/", nice_name, "-pa.csv")
# Check to see if file exists and what to do if not
if (!file.exists(pa_file)) {
  unzip(zipfile = "data/gbif-pa.zip")
}
pa_data <- read.csv(file = pa_file)

# Get shapefile for geographic extent (to crop predictor rasters)
shapefile_name <- paste0("data/gbif/shapefiles/",
                         nice_name, 
                         "-buffered-mcp.shp")
# If species' shapefile isn't in shapefiles folder, unzip gbif-shapefiles
if (!file.exists(shapefile_name)) {
  unzip(zipfile = "data/gbif-shapefiles.zip")
}
buffered_mcp <- vect(shapefile_name)

# Grab worldclim data to use as predictors
predictors <- terra::rast(list.files(path = "data/wc2-1",
                                     pattern = ".tif$",
                                     full.names = TRUE))

# Extract a subset of climate variables
all_climate_vars <- read.csv("data/climate-variables.csv", header = TRUE)
climate_vars <- all_climate_vars$variable[all_climate_vars$include == TRUE]
predictors <- terra::subset(predictors, climate_vars)

# Crop and mask predictor rasters
pred_mask <- terra::crop(predictors, buffered_mcp, snap = "out")
pred_mask <- terra::mask(pred_mask, buffered_mcp)  

# Extract value of predictors at each presence/background location (needed for 
# all SDMs except MAXENT). 
predictors_df <- terra::extract(x = pred_mask,
                                y = pa_data[, c("x", "y")],
                                xy = FALSE) %>%
  dplyr::select(-ID)
pa_data <- cbind(pa_data, predictors_df)

# Calculate means, SDs for standardizing covariates
stand_obj <- save_means_sds(pa_data, cols = climate_vars, verbose = FALSE)

# Run BRT
  sdm <- "brt"
  message("Running ", toupper(sdm), " model for ", species_name)

  # Extract settings from CV models
  brt_settings <- evals %>%
    filter(sdm == "BRT" & fold == 1) %>%
    select(tune.args) %>%
    str_split_fixed(pattern = "_", n = 3) %>%
    as.vector()
  ntrees <- as.numeric(str_remove(brt_settings[1], "tree."))
  complexity <- as.numeric(str_remove(brt_settings[2], "node."))
  lr <- as.numeric(str_remove(brt_settings[3], "lr."))
  
  # Run model
  brt_fit <- run_brt(full_data = pa_data, step = FALSE, ntrees = ntrees,
                     complexity = complexity, learning.rate = lr,
                     verbose = FALSE)

  # Bind everything into a list and save to file
  brt_results <- list(model = brt_fit,
                      sdm = sdm,
                      stand_obj = NA,
                      quad = NA,
                      climate_vars = climate_vars)
  brt_file <- paste0("output/SDMs/", nice_name, "-", sdm, ".rds")
  saveRDS(brt_results, brt_file)
  
# Run GAM
  sdm <- "gam"
  message("Running ", toupper(sdm), " model for ", species_name)
  quad <- FALSE

  # Run model
  gam_fit <- run_gam(full_data = pa_data, 
                     stand_obj = stand_obj,
                     quad = quad)
  
  # Bind everything into a list and save to file
  gam_results <- list(model = gam_fit,
                      sdm = sdm,
                      stand_obj = stand_obj,
                      quad = quad,
                      climate_vars = climate_vars)
  gam_file <- paste0("output/SDMs/", nice_name, "-", sdm, ".rds")
  saveRDS(gam_results, gam_file)  
  
# Run LASSO
  sdm <- "lasso"
  message("Running ", toupper(sdm), " model for ", species_name)
  quad <- TRUE
  
  # Run model
  lasso_fit <- run_lasso(full_data = pa_data, 
                         stand_obj = stand_obj,
                         quad = quad)
  
  # Bind everything into a list and save to file
  lasso_results <- list(model = lasso_fit,
                        sdm = sdm,
                        stand_obj = stand_obj,
                        quad = quad,
                        climate_vars = climate_vars)
  lasso_file <- paste0("output/SDMs/", nice_name, "-", sdm, ".rds")
  saveRDS(lasso_results, lasso_file)   
  
# Run RF
  sdm <- "rf"
  message("Running ", toupper(sdm), " model for ", species_name)

  # Run model
  rf_fit <- run_rf(full_data = pa_data,
                   ntree = 1000)
  
  # Bind everything into a list and save to file
  rf_results <- list(model = rf_fit,
                     sdm = sdm,
                     stand_obj = NA,
                     quad = NA,
                     climate_vars = climate_vars)
  rf_file <- paste0("output/SDMs/", nice_name, "-", sdm, ".rds")
  saveRDS(rf_results, rf_file) 
  
# Load Maxent model and save as list (if not done already)
  sdm <- "maxent"
  message("Saving ", toupper(sdm), " model for ", species_name)
  
  # Load model
  max_file <- paste0("output/SDMs/", nice_name, "-", sdm, ".rds")
  max_model <- readRDS(max_file)
  
  # Check if it's already in desired list format. If not, do that and save.
  if (!(length(max_model) == 5 & names(max_model)[2] == "sdm")) {
    max_results <- list(model = max_model,
                        sdm = sdm,
                        stand_obj = NA,
                        quad = NA,
                        climate_vars = climate_vars)
    saveRDS(max_results, max_file)
  }
