#' Predict suitability values and distributions under each climate scenario for 
#' a single species
#' 
#' @param species_name character scientific name of species to run, e.g. 
#' "Papilio rumiko"
#' @param rerun logical indicating whether or not to re-run analyses if results
#' are already disk; operates at the level of individual SDM methods - that is,
#' if \code{rerun} is \code{FALSE}, this function will only estimate models 
#' which do not already exist on disk
#' @param sdm_raster_save logical indicating whether or not to save predicted 
#' suitability values from individual SDMs for the current time period
#' 
#' @return Character vector with information about model evaluation, reporting 
#' completion, any problems, or if evaluation process was skipped because file
#' is already on disk and \code{rerun} was set to \code{FALSE}.
run_one_predict <- function(species_name, rerun = TRUE, sdm_raster_save = TRUE) {
  # List necessary packages
  requirements <- c("dismo", "dplyr", "ENMeval", "gbm", "glmnet", 
                    "mgcv", "randomForest", "raster", "stringr", "terra")
  # Attempt to load required packages
  reqs_met <- sapply(X = requirements, 
                     FUN = require, 
                     character.only = TRUE, # require needs this
                     quietly = TRUE) # Will handle this better
  # Throw error if any packages are missing
  if (any(!reqs_met)) {
    # Extract the name of this function for reporting
    function_name <- as.character(match.call())[1]
    # Pull out missing packages
    missing_pkgs <- requirements[!reqs_met]
    stop(function_name, " requires the following missing packages: ",
         paste0(missing_pkgs, collapse = ", "))
  }
  # Check to see if functions folder has already been sourced
  if (length(lsf.str(pattern = "prep_predictors")) == 0) {
    # prep_predictors() function not in memory, so functions likely hasn't been 
    # sourced; do so now.
    source(file = "load_functions.R")
  }
  # A more compute-friendly name
  nice_name <- tolower(gsub(pattern = " ",
                            replacement = "_",
                            x = species_name))
  
  # Character vector for reporting out
  status_message <- ""
  
  # Start first by making sure all necessary files are on disk (and report out 
  # if not)
  
  # Check to see if presence/absence file exists
  pa_file <- paste0("data/gbif/presence-absence/", nice_name, "-pa.csv")
  # If not, first try to unzip archive
  if (!file.exists(pa_file)) {
    unzip(zipfile = "data/gbif-pa.zip")
  }
  # File with evaluation metrics for each model
  eval_file <- paste0("output/eval-metrics/", nice_name, "-CVevals.csv")
  # Shapefile for geographic extent (to crop predictor rasters)
  shapefile_name <- paste0("data/gbif/shapefiles/",
                           nice_name, 
                           "-buffered-mcp.shp")
  # If species' shapefile isn't in shapefiles folder, unzip gbif-shapefiles
  if (!file.exists(shapefile_name)) {
    unzip(zipfile = "data/gbif-shapefiles.zip")
  }
  # Check to see if all three files exist
  files_exist <- file.exists(c(pa_file, eval_file, shapefile_name))
  names(files_exist) <- c("Presence absence file", "Evaluation metrics file",
                          "Shapefile")
  if (!all(files_exist)) {
    status_message <- paste0("One or more files for ", species_name, 
                             " missing; no predictions made. Missing files: ", 
                             paste0(names(files_exist)[!files_exist], 
                                    collapse = ", "))
  } else {
    # Read in presence/absence data
    pa_data <- read.csv(file = pa_file)
    # Read evaluation metrics from CV models
    evals <- read.csv(eval_file, header = TRUE)
    # Read shapefile for cropping prediction rasters
    mcp <- terra::vect(shapefile_name)
    
    # Get average TSS for each model / tuning parameter set
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

    # Grab predictors
    gcm_directory <- "data/wc2-1"
    predictors <- terra::rast(list.files(path = gcm_directory,
                                         pattern = ".tif$",
                                         full.names = TRUE))
    
    # Extract only those layers associated with climate variables in the model
    predictors <- terra::subset(predictors, climate_vars)

    # Cut off areas that fall outside the geographic extent of climate rasters
    # (could occur in any time period)
    mcp <- terra::crop(mcp, ext(predictors))  
    
    # Crop and mask as appropriate
    pred_mask <- terra::crop(predictors, mcp, snap = "out")
    pred_mask <- terra::mask(pred_mask, mcp)

    ########################################
    # Make predictions for current time period
    
    clim_name <- climate_models$name[1]
    clim_yr <- climate_models$yr[1]
    clim_ssp <- climate_models$ssp[1]

    # File to hold weighted mean suitabilities
    wtmn_file <- paste0("output/suitabilities/", nice_name, "-",
                        clim_name, ".rds")
    # File to hold binary presence / absence prediction based on weighted 
    # mean suitabilities
    dist_file <- paste0("output/distributions/", nice_name, "-distribution-",
                        clim_name, ".rds")

    # Future distribution files; we are grabbing these now to see if they 
    # exist. The big issue is that if we are missing *any* of the forecast 
    # distributions, we will need to re-run contemporary predictions in order 
    # to get threshold value for determining presence/absence
    future_dist_files <- paste0("output/distributions/", nice_name, 
                                "-distribution-",
                                climate_models$name[2:nrow(climate_models)], 
                                ".rds")
    
    # Threshold value for determining presence/absence cutoff
    thr <- NULL
    
    # Only proceed if rerun is TRUE or suitability and/or distribution file(s) 
    # missing
    if (rerun | !all(file.exists(c(wtmn_file, dist_file, future_dist_files)))) {
      status_message <- paste0("Making predictions for current climate for ", 
                               species_name, ".")
      # Iterate over all models.
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
        if (sdm_raster_save) {
          rast_file <- paste0("output/suitabilities/", nice_name, "-", sdm, "-",
                              clim_yr, ".rds")
          saveRDS(sdm_suit, rast_file)
        }
        assign(paste0(sdm, "_suit"), sdm_suit)
      } # End iteration over all sdms
      
      # Create and save raster with weighted mean values (ie, mean of suitability 
      # values across different SDMs, weighted by mean TSS values from CV models)
      wtmn <- app(rast(mget(paste0(tolower(sdms), "_suit"))),
                  function(x) sum(x * evals_avg$tss_wt))
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
      saveRDS(distrib, dist_file)
    } else {
      status_message <- paste0("Current climate suitability predictions for ", 
                               species_name, 
                               " already on disk and rerun set to FALSE.")
    } # End conditional for contemporary climate predictions
    
    ########################################
    # Make predictions for forecast climate models
    
    # Loop through future climate scenarios
    for (i in 2:nrow(climate_models)) {
      clim_name <- climate_models$name[i]
      clim_yr <- climate_models$yr[i]
      clim_ssp <- climate_models$ssp[i]
      
      # File to hold weighted mean suitabilities
      wtmn_file <- paste0("output/suitabilities/", nice_name, "-",
                          clim_name, ".rds")
      # File to hold binary presence / absence prediction based on weighted 
      # mean suitabilities
      dist_file <- paste0("output/distributions/", nice_name, "-distribution-",
                          clim_name, ".rds")
      
      # Only proceed if rerun is TRUE or suitability and/or distribution file(s) 
      # missing
      if (rerun | !all(file.exists(c(wtmn_file, dist_file)))) {
        status_message <- c(status_message, paste0("Making predictions for ", 
                                                   clim_name, " for ", 
                                                   species_name, "."))
        
        # Grab predictors
        gcm_directory <- paste0("data/ensemble/ssp", clim_ssp, "/", clim_yr)
        predictors <- terra::rast(list.files(path = gcm_directory,
                                             pattern = ".tif$",
                                             full.names = TRUE))
        
        # Extract only those layers associated with climate variables in the model
        predictors <- terra::subset(predictors, climate_vars)
        
        # Extend buffered MCP for future time periods 
        # (350 km, based on P. cresphontes [Wilson et al. 2021])
        dist_mult <- 350
        buffered_mcp <- terra::buffer(mcp, width = dist_mult * 1000)
        
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
          assign(paste0(sdm, "_suit"), sdm_suit)
        }
        
        # Create and save raster with weighted mean values (ie, mean of suitability 
        # values across different SDMs, weighted by mean TSS values from CV models)
        wtmn <- app(rast(mget(paste0(tolower(sdms), "_suit"))),
                    function(x) sum(x * evals_avg$tss_wt))
        saveRDS(wtmn, wtmn_file)
        
        # Create and save raster with predicted distribution (suitability value > thr)
        if (!is.null(thr)) {
          distrib <- wtmn > thr
          saveRDS(distrib, dist_file)
        } else {
          status_message <- c(status_message, 
                              paste0("Warning: Threshold missing for ", 
                                     clim_name, " for ", species_name, 
                                     "; distribution not available."))
        }
      } else {
        status_message <- paste0(clim_name, " suitability predictions for ", 
                                 species_name, 
                                 " already on disk and rerun set to FALSE.")
      }
    } # End iteration over all future climate scenarios
  } # End else for all files on disk
  return(status_message)
}