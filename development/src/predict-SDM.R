# Predict suitability for current/future time periods
# Erin Zylstra
# ezylstra@arizona.edu
# 2023-09-07

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

# Identify which species to include (just insects for now) 
spp_data <- read.csv("data/gbif-pa-summary.csv", header = TRUE)
insect_data <- spp_data %>%
  filter(str_detect(species, "Papilio") & pa_csv == "yes") %>%
  select(species, n_filtered)
nice_names <- insect_data %>%
  select(species) %>%
  unlist() %>%
  str_replace(pattern = " ", replacement = "_") %>%
  tolower()

# Grab worldclim data to use as predictors
predictors <- terra::rast(list.files(path = "data/wc2-1",
                                     pattern = ".tif$",
                                     full.names = TRUE))

# For now, will use only 9 of the worldclim variables (this includes the 6 vars
# that Low et al. 2020 used, plus a few variables that assess seasonal or daily
# variation in temp/precip)
climate_vars <- paste0("bio", c(1, 2, 4:6, 12:15))

# Subset climate variables
predictors <- terra::subset(predictors, climate_vars)

# Load evaluation metrics from CV models
evals <- read.csv("development/output/evals-CV-insect.csv", header = TRUE)

# Make predictions, looping through each species
for (i in 1:nrow(insect_data)) {
  nice_name <- nice_names[i]
  insect <- insect_data$species[i]
  cat(paste0("Predicting suitability for ", insect, ".\n"))

  # Load SDM models
  file_base <- paste0("development/output/SDMs/", nice_name, "-sdm")
  brt_mod <- readRDS(paste0(file_base, "-brt-9var.rds"))
  gam_mod <- readRDS(paste0(file_base, "-gam-9var.rds"))
  lasso_mod <- readRDS(paste0(file_base, "-lasso-9var.rds"))
  maxent_mod <- readRDS(paste0(file_base, "-maxent-9var.rds"))
  rf_mod <- readRDS(paste0(file_base, "-rf-9var.rds"))

  # Get shapefile for geographic extent (to crop predictor rasters)
  shapefile_name <- paste0("data/gbif/shapefiles/",
                           nice_name, 
                           "-buffered-mcp.shp")
  # If species' shapefile isn't in shapefiles folder, unzip gbif-shapefiles
  if (!file.exists(shapefile_name)) {
    unzip(zipfile = "data/gbif-shapefiles.zip")
  }
  buffered_mcp <- vect(shapefile_name)
  
  # Crop and mask predictor rasters for current time period
  pred_mask <- terra::crop(predictors, buffered_mcp)
  pred_mask <- terra::mask(pred_mask, buffered_mcp) 
  # Create raster stack for MAXENT and RF predictions
  pred_rs <- raster::stack(pred_mask)
  
  # Make and save predictions for each model, current time period
    file_start <- "development/output/predicted-probabilities/"
    
    brt_current <- predict_sdm(nice_name = nice_name,
                               model = brt_mod$model,
                               sdm_method = "brt",
                               yr = "current")
    saveRDS(brt_current, 
            paste0(file_start, nice_name, "-pred-probs-brt-current.rds"))
    
    gam_current <- predict_sdm(nice_name = nice_name,
                               model = gam_mod$model,
                               sdm_method = "gam",
                               yr = "current",
                               stand_obj = gam_mod$standardize_objects,
                               quad = FALSE)
    saveRDS(gam_current, 
            paste0(file_start, nice_name, "-pred-probs-gam-current.rds"))
    
    lasso_current <- predict_sdm(nice_name = nice_name,
                                 model = lasso_mod$model,
                                 sdm_method = "lasso",
                                 yr = "current",
                                 stand_obj = lasso_mod$standardize_objects,
                                 quad = TRUE)
    saveRDS(lasso_current, 
            paste0(file_start, nice_name, "-pred-probs-lasso-current.rds"))
    
    # Can't use predict for maxnet models. See ?ENMevaluate or 
    # https://github.com/jamiemkass/ENMeval/issues/112
    max_current <- enm.maxnet@predict(maxent_mod$model, pred_rs, 
                                      list(pred.type ="cloglog", doClamp = FALSE))
    max_current <- terra::rast(max_current)
    saveRDS(max_current, 
            paste0(file_start, nice_name, "-pred-probs-max-current.rds"))
    
    # Need to modify predict_sdm() to work with classification RF
    rf_current <- raster::predict(object = pred_rs,
                                  model = rf_mod$model, 
                                  type = "prob",
                                  index = 2)
    rf_current <- terra::rast(rf_current)
    saveRDS(rf_current, 
            paste0(file_start, nice_name, "-pred-probs-rf-current.rds"))
  
  # Make and save predictions for each model, future time period (ssp245-2041)
    brt_fut <- predict_sdm(nice_name = nice_name,
                           model = brt_mod$model,
                           sdm_method = "brt",
                           yr = "2041",
                           ssp = "245")
    saveRDS(brt_fut, 
            paste0(file_start, nice_name, "-pred-probs-brt-ensemble_ssp245_2041.rds"))
    
    gam_fut <- predict_sdm(nice_name = nice_name,
                           model = gam_mod$model,
                           sdm_method = "gam",
                           yr = "2041",
                           ssp = "245",
                           stand_obj = gam_mod$standardize_objects,
                           quad = FALSE)
    saveRDS(gam_fut, 
            paste0(file_start, nice_name, "-pred-probs-gam-ensemble_ssp245_2041.rds"))
    
    lasso_fut <- predict_sdm(nice_name = nice_name,
                             model = lasso_mod$model,
                             sdm_method = "lasso",
                             yr = "2041",
                             ssp = "245",
                             stand_obj = lasso_mod$standardize_objects,
                             quad = TRUE)
    saveRDS(lasso_fut, 
            paste0(file_start, nice_name, "-pred-probs-lasso-ensemble_ssp245_2041.rds"))
    
    # Get future climate rasters
    gcm_directory <- "data/ensemble/ssp245/2041"
    predictors <- terra::rast(list.files(path = gcm_directory,
                                         pattern = ".tif$",
                                         full.names = TRUE))
    # Extract only those layers associated with climate variables in the model
    predictors <- terra::subset(predictors, climate_vars)
    
    # Extend buffer
    dist_mult <- 350
    buffered_mcp <- terra::buffer(buffered_mcp, width = dist_mult * 1000)
    terra::crs(buffered_mcp) <- "EPSG:4326"
    
    # Crop and mask predictor rasters for future time period
    pred_mask <- terra::crop(predictors, buffered_mcp)
    pred_mask <- terra::mask(pred_mask, buffered_mcp) 
    # Create raster stack for MAXENT and RF predictions
    pred_rs <- raster::stack(pred_mask)
    
    max_fut <- enm.maxnet@predict(maxent_mod$model, pred_rs, 
                                  list(pred.type ="cloglog", doClamp = FALSE))
    max_fut <- terra::rast(max_fut)
    saveRDS(max_fut, 
            paste0(file_start, nice_name, "-pred-probs-max-ensemble_ssp245_2041.rds"))
    
    rf_fut <- raster::predict(object = pred_rs,
                              model = rf_mod$model, 
                              type = "prob",
                              index = 2)
    rf_fut <- terra::rast(rf_fut)
    saveRDS(rf_fut, 
            paste0(file_start, nice_name, "-pred-probs-rf-ensemble_ssp245_2041.rds"))
}
  