# A template to predict suitability values and distributions under each
# climate scenario for a given species
# Erin Zylstra
# ezylstra@arizona.edu
# 2023-10-09

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

# Logical indicating whether or not to save predicted suitability values from
# individual SDMs for the current time period
sdm_raster_save <- TRUE

genus <- "GENUS"
species <- "SPECIES"

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

# Load evaluation metrics from CV models
eval_file <- paste0("output/eval-metrics/", nice_name, "-CVevals.csv")
evals <- read.csv(eval_file, header = TRUE)
evals_avg <- evals %>%
  group_by(sdm, tune.args) %>%
  summarize(tss = mean(TSS),
            .groups = "keep") %>%
  data.frame()
# Calculate SDM weights based on mean TSS
evals_avg$tss_wt <- evals_avg$tss / sum(evals_avg$tss)

# Grab list of SDMs
sdms <- tolower(evals_avg$sdm)

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
  
  # Get shapefile for geographic extent (to crop predictor rasters)
  shapefile_name <- paste0("data/gbif/shapefiles/",
                           nice_name, 
                           "-buffered-mcp.shp")
  # If species' shapefile isn't in shapefiles folder, unzip gbif-shapefiles
  if (!file.exists(shapefile_name)) {
    unzip(zipfile = "data/gbif-shapefiles.zip")
  }
  buffered_mcp <- vect(shapefile_name)
  
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
  pred_mask <- terra::crop(predictors, buffered_mcp, snap = "out")
  pred_mask <- terra::mask(pred_mask, buffered_mcp)

  for (sdm in sdms) {
    model_list <- get(paste0(sdm, "_mod"))
    model <- model_list$model
    stand_obj <- model_list$stand_obj
    quad <- model_list$quad
    sdm_suit <- predict_sdm(nice_name = nice_name,
                            sdm_method = sdm,
                            model = model,
                            stand_obj = stand_obj,
                            quad = quad,
                            predictors = pred_mask)
    if (sdm_raster_save == TRUE & clim_yr == "current") {
      rast_file <- paste0("output/suitabilities/", nice_name, "-", sdm, "-",
                         clim_yr, ".rds")
      saveRDS(sdm_suit, rast_file)
    }
    assign(paste0(sdm, "_suit"), sdm_suit)
  }
  
  # Create and save raster with weighted mean values (ie, mean of suitability 
  # values across different SDMs, weighted by mean TSS values from CV models)
  wtmn <- app(rast(mget(paste0(tolower(sdms), "_suit"))),
              function(x) sum(x * evals_avg$tss_wt))
  wtmn_file <- paste0("output/suitabilities/", nice_name, "-",
                      clim_name, ".rds")
  saveRDS(wtmn, wtmn_file)
  
  # Extract predicted suitability values for all occurrence and bg points
  preds_pa <- terra::extract(wtmn, pa_data[, c("x", "y")], ID = FALSE)
  p <- preds_pa[pa_data$pa == 1, 1]
  a <- preds_pa[pa_data$pa == 0, 1]
  
  # Calculate threshold value to convert suitability values to binary values
  ev <- dismo::evaluate(p = p, a = a)
  thr <- dismo::threshold(ev, stat = "spec_sens")
  
  # Create and save raster with predicted distribution (suitability value > thr)
  distrib <- wtmn > thr
  dist_file <- paste0("output/distributions/", nice_name, "-distribution-",
                      clim_name, ".rds")
  saveRDS(distrib, dist_file)
}
