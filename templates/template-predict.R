# A template to predict suitability values and distributions under each
# climate scenario for a given species
# Erin Zylstra
# ezylstra@arizona.edu
# 2023-10-04

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

# Logical indicating whether or not to save predicted suitabilities values from
# individual SDMs for the current time period
sdm_raster_save <- TRUE

# genus <- "GENUS"
# species <- "SPECIES"
genus <- "Papilio"
species <- "machaon"

# Name for reporting and looking up info in files
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

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

# Load evaluation metrics from CV models
eval_file <- paste0("output/eval-metrics/", nice_name, "-CVevals.csv")
evals <- read.csv(eval_file, header = TRUE)
evals_avg <- evals %>%
  group_by(sdm, tune.args) %>%
  summarize(tss = mean(TSS),
            .groups = "keep") %>%
  data.frame()

# Load SDM objects (each SDM based on all available data)
sdm_base <- paste0("output/SDMs/", nice_name)
brt_mod <- readRDS(paste0(sdm_base, "-brt.rds"))
gam_mod <- readRDS(paste0(sdm_base, "-gam.rds"))
lasso_mod <- readRDS(paste0(sdm_base, "-lasso.rds"))
maxent_mod <- readRDS(paste0(sdm_base, "-maxent.rds"))
rf_mod <- readRDS(paste0(sdm_base, "-rf.rds"))

# Get list of climate variables that were considered for the SDM
all_climate_vars <- read.csv("data/climate-variables.csv")
climate_vars <- all_climate_vars$variable[all_climate_vars$include == TRUE]

# Load list of climate models
climate_models <- read.csv(file = "data/climate-models.csv")

# Loop through climate scenarios
for (i in 1:nrow(climate_models)) {
  clim_name <- climate_models$name[i]
  clim_yr <- climate_models$yr[i]
  clim_ssp <- climate_models$ssp[i]

  # Grab predictors
  gcm_directory <- dplyr::if_else(clim_yr == "current",
                                  "data/wc2-1",
                                  paste0("data/ensemble/ssp", clim_ssp, "/", clim_yr))
  
  predictors <- terra::rast(list.files(path = gcm_directory,
                                       pattern = ".tif$",
                                       full.names = TRUE))
  
  # Extract only those layers associated with climate variables in the model
  predictors <- terra::subset(predictors, climate_vars)
  
  # If necessary, adjust buffered MCP as appropriate - allowing larger buffers
  # for more distant time periods
  if (clim_yr %in% c("2041", "2071")) {
    dist_mult <- dplyr::if_else(clim_yr == "2041",
                                true = 350,  # 350 km for 2041
                                false = 900) # 900 km for 2071
    
    buffered_mcp <- terra::buffer(buffered_mcp, width = dist_mult * 1000)
  }
  
  # Cut off areas that fall outside the geographic extent of climate rasters
  # (could occur in any time period)
  buffered_mcp <- terra::crop(buffered_mcp, ext(predictors))  
  
  # Crop and mask as appropriate
  pred_mask <- terra::crop(predictors, buffered_mcp, snap = "in")
  pred_mask <- terra::mask(pred_mask, buffered_mcp)
  
  # Predict suitability for each SDM using predict_sdm (in a loop?)
  # Save rasters for current time period (if sdm_raster_save == TRUE)
  
  # Create raster with weighted average of suitability values
  # Save raster
  
  # Calculate threshold value
  
  # Create raster with predicted distribution
  # Save raster
  
}
