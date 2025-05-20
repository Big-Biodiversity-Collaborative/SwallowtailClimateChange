#' Estimate all SDMs on full dataset for a single species
#' 
#' @param species_name character scientific name of species to run, e.g. 
#' "Papilio rumiko"
#' @param rerun logical indicating whether or not to re-run analyses if results
#' are already disk; operates at the level of individual SDM methods - that is,
#' if \code{rerun} is \code{FALSE}, this function will only estimate models 
#' which do not already exist on disk
#' @param num_cores integer number of cores to use when evaluating MaxEnt 
#' model; passed to \code{numCores} argument of \code{ENMevaluate}. Users are 
#' strongly recommended to use default value (2)
#' @param quad_gam logical indicating whether or not quadratics should be 
#' included in gam model (default = FALSE)
#' @param quad_lasso logical indicating whether or not quadratics should be 
#' included in lasso model (default = TRUE)
#' 
#' @return Character vector with information about model evaluation, reporting 
#' completion, any problems, or if evaluation process was skipped because file
#' is already on disk and \code{rerun} was set to \code{FALSE}.
run_one_SDMs_full <- function(species_name, rerun = TRUE, num_cores = 2,
                              quad_gam = FALSE, quad_lasso = TRUE) {
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
  
  # Load evaluation metrics from CV models
  eval_file <- paste0("output/eval-metrics/", nice_name, "-CVevals.csv")
  if (!file.exists(eval_file)) {
    status_message <- paste0("No evaluation metrics available for ",
                             species_name, ", no SDMs estimated.")
  } else {
    evals <- read.csv(eval_file, header = TRUE)
    # Load in presence/pseudo-absence data
    pa_file <- paste0("data/gbif/presence-absence/", nice_name, "-pa.csv")
    # Check to see if file exists and what to do if not
    if (!file.exists(pa_file)) {
      unzip(zipfile = "data/gbif-pa.zip")
    }
    # Only proceed if pa file exists
    if (!file.exists(pa_file)) {
      status_message <- paste0("Presence/absence data for ", species_name, 
                               " not available, no SDMs estimated.")
    } else {
      pa_data <- read.csv(file = pa_file)
      shapefile_name <- paste0("data/gbif/shapefiles/",
                               nice_name, 
                               "-buffered-mcp.shp")
      # If species' shapefile isn't in shapefiles folder, unzip gbif-shapefiles
      if (!file.exists(shapefile_name)) {
        unzip(zipfile = "data/gbif-shapefiles.zip")
      }
      # Only proceed if shapefile exists
      if (!file.exists(shapefile_name)) {
        status_message <- paste0("Shapefile for ", species_name, 
                                 " not available, no SDMs estimated.")
      } else {
        buffered_mcp <- terra::vect(shapefile_name)
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
        
        # Extract value of predictors at each presence/background location 
        # (needed for all SDMs except MAXENT). 
        predictors_df <- terra::extract(x = pred_mask,
                                        y = pa_data[, c("x", "y")],
                                        xy = FALSE) %>%
          dplyr::select(-ID)
        pa_data <- cbind(pa_data, predictors_df)
        
        # Calculate means, SDs for standardizing covariates
        stand_obj <- save_means_sds(pa_data, cols = climate_vars, verbose = FALSE)

        # Setup status message for start of model estimation process
        status_message <- paste0("About to estimate full models for ",
                                 species_name, ".")
        
        ########################################
        # Run BRT
        ########################################
        sdm <- "brt"
        brt_file <- paste0("output/SDMs/", nice_name, "-", sdm, ".rds")
        if (!file.exists(brt_file) | rerun) {
          message("Running ", toupper(sdm), " model for ", species_name)
          status_message <- paste0(status_message,
                                   "\nRunning ", toupper(sdm), 
                                   " model for ", species_name)
          # Extract settings from CV models
          brt_settings <- evals %>%
            dplyr::filter(sdm == "BRT" & fold == 1) %>%
            dplyr::select(tune.args) %>%
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
          saveRDS(brt_results, brt_file)
        } else {
          message("Results for ", sdm, " model for ", species_name, 
                  " already on disk and rerun set to FALSE.")
        }

        ########################################
        # Run GAM
        ########################################
        sdm <- "gam"
        gam_file <- paste0("output/SDMs/", nice_name, "-", sdm, ".rds")
        if (!file.exists(gam_file) | rerun) {
          message("Running ", toupper(sdm), " model for ", species_name)
          status_message <- paste0(status_message,
                                   "\nRunning ", toupper(sdm), 
                                   " model for ", species_name)
          quad <- quad_gam
          
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
          saveRDS(gam_results, gam_file)  
        } else {
          message("Results for ", sdm, " model for ", species_name, 
                  " already on disk and rerun set to FALSE.")
        }
        
        ########################################
        # Run LASSO
        ########################################
        sdm <- "lasso"
        lasso_file <- paste0("output/SDMs/", nice_name, "-", sdm, ".rds")
        if (!file.exists(lasso_file) | rerun) {
          message("Running ", toupper(sdm), " model for ", species_name)
          status_message <- paste0(status_message,
                                   "\nRunning ", toupper(sdm), 
                                   " model for ", species_name)
          quad <- quad_lasso
          
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
          saveRDS(lasso_results, lasso_file)   
        } else {
          message("Results for ", sdm, " model for ", species_name, 
                  " already on disk and rerun set to FALSE.")
        }
        
        ########################################
        # Run RF
        ########################################
        sdm <- "rf"
        rf_file <- paste0("output/SDMs/", nice_name, "-", sdm, ".rds")
        if (!file.exists(rf_file) | rerun) {
          message("Running ", toupper(sdm), " model for ", species_name)
          status_message <- paste0(status_message,
                                   "\nRunning ", toupper(sdm), 
                                   " model for ", species_name)
          
          # Run model
          rf_fit <- run_rf(full_data = pa_data,
                           importance = TRUE,
                           ntree = 1000)
          
          # Bind everything into a list and save to file
          rf_results <- list(model = rf_fit,
                             sdm = sdm,
                             stand_obj = NA,
                             quad = NA,
                             climate_vars = climate_vars)
          saveRDS(rf_results, rf_file) 
        } else {
          message("Results for ", sdm, " model for ", species_name, 
                  " already on disk and rerun set to FALSE.")
        }
        
        ########################################
        # Load Maxent
        ########################################
        
        # Load Maxent model and save as list (if not done already)
        sdm <- "maxent"
        message("Saving ", toupper(sdm), " model for ", species_name)
        status_message <- paste0(status_message,
                                 "\nSaving ", toupper(sdm), 
                                 " model for ", species_name)
        
        # Load model
        max_file <- paste0("output/SDMs/", nice_name, "-", sdm, ".rds")
        max_model <- readRDS(max_file)
        
        # Check if it's already in desired list format. If not, do that and 
        # save.
        if (!(length(max_model) == 5 & names(max_model)[2] == "sdm")) {
          max_results <- list(model = max_model,
                              sdm = sdm,
                              stand_obj = NA,
                              quad = NA,
                              climate_vars = climate_vars)
          saveRDS(max_results, max_file)
        }
        status_message <- paste0(status_message, 
                                 "\nSDM model estimation complete for ", 
                                 species_name)
      }  # End else for shapefile exists
    } # End else for presence/absence file exists
  } # End else for evaluation file exists
  return(status_message)
}
