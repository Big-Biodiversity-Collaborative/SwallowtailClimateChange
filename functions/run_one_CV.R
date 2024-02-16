# IN DEVELOPMENT. Not ready for use.

#' Tune MaxEnt and BRT models and cross-validate all SDMs for a single species
#' 
#' @param species_name character scientific name of species to run, e.g. 
#' "Papilio rumiko"
#' @param rerun logical indicating whether or not to re-run analyses if results
#' are already disk
#' @param max_save logical to indicate whether to save ENMeval object that 
#' contains output from all models (and not just the model with "optimal" 
#' tuning parameters)
#' @param num_cores integer number of cores to use when evaluating MaxEnt 
#' model; passed to \code{numCores} argument of \code{ENMevaluate}. Users are 
#' strongly recommended to use default value (2)
run_one_CV <- function(species_name, rerun = TRUE, max_save = FALSE, 
                       num_cores = 2) {
  # List necessary packages
  requirements <- c("dplyr", "ecospat", "ENMeval", "flexsdm", "gbm", "glmnet", 
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

  # Evaluation file to store results
  eval_file <- paste0("output/eval-metrics/", nice_name, "-CVevals.csv")
  
  # Start by checking to see if we need to proceed (evaluation file is missing
  # or rerun is FALSE)
  if (!file.exists(eval_file) | rerun) {
    
    # Load in presence/pseudo-absence data
    pa_file <- paste0("data/gbif/presence-absence/", nice_name, "-pa.csv")
    # Check to see if file exists and what to do if not
    if (!file.exists(pa_file)) {
      unzip(zipfile = "data/gbif-pa.zip")
    }
    
    # Proceed only if pa file exists; warn if not
    if (file.exists(pa_file)) {
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
      # Create RasterStack (needed for MAXENT model)
      pred_rs <- raster::stack(pred_mask) 
      
      # Extract value of predictors at each presence/background location (needed for 
      # all SDMs except MAXENT). 
      predictors_df <- terra::extract(x = pred_mask,
                                      y = pa_data[, c("x", "y")],
                                      xy = FALSE) %>%
        dplyr::select(-ID)
      pa_data <- cbind(pa_data, predictors_df)
      
      # Create a table to store evaluation metrics for each SDM and fold
      sdms <- c("BRT", "GAM", "LASSO", "MAXENT", "RF")
      nfolds <- max(pa_data$fold)
      folds <- 1:nfolds
      evals <- data.frame(sdm = rep(sdms, each = max(folds)),
                          tune.args = NA,
                          fold = rep(folds, length(sdms)),
                          AUC = NA,
                          CBI = NA,
                          threshold = NA,
                          OR = NA,
                          TSS = NA)
      
      # Run MAXENT models ---------------------------------------------------------#
      
      message("Running Maxent models for ", species_name)
      
      # Dataframes with lat/long (in that order) for presence and background 
      # locations
      occs <- pa_data %>%
        dplyr::filter(pa == 1) %>%
        dplyr::select(x, y)
      bg <- pa_data %>%
        dplyr::filter(pa == 0) %>%
        dplyr::select(x, y)
      
      # ENMeval settings
      feature_classes <- c("L", "LQ", "H", "LQH")
      multipliers <- 1:3
      tune.args <- list(fc = feature_classes, rm = multipliers)
      user.grp <- list(occs.grp = pa_data$fold[pa_data$pa == 1],
                       bg.grp = pa_data$fold[pa_data$pa == 0])
      os <- list(validation.bg = "partition",
                 pred.type = "cloglog")
      
      # Run maxent models (Put this in a try() function because every once in a while
      # hinge features combined with a multiplier >=2 will cause an error. When this 
      # occurs, its easiest to remove "H" from the list of feature classes and re-run)
      max_models <- NULL
      try(
        max_models <- ENMevaluate(occs = occs, 
                                  bg = bg, 
                                  envs = pred_rs,
                                  algorithm = "maxnet",
                                  partitions = "user",
                                  user.grp = user.grp,
                                  tune.args = tune.args,
                                  other.settings = os,
                                  user.eval = em,
                                  parallel = TRUE,
                                  numCores = num_cores) 
      )
      if (is.null(max_models)) {
        feature_classes <-  c("L", "LQ", "LQH")
        tune.args <- list(fc = feature_classes, rm = multipliers) 
        max_models <- ENMevaluate(occs = occs, 
                                  bg = bg, 
                                  envs = pred_rs,
                                  algorithm = "maxnet",
                                  partitions = "user",
                                  user.grp = user.grp,
                                  tune.args = tune.args,
                                  other.settings = os,
                                  user.eval = em,
                                  parallel = TRUE,
                                  numCores = num_cores) 
      }
      
      # Save ENMeval object to file (contains 12 models with different tuning params)
      if (max_save) {
        sdm_file <- paste0("output/SDMs/", nice_name, "-maxent-tuning.rds")
        saveRDS(max_models, sdm_file)
      }
      
      # Identify optimal tuning parameters
      other_summaries <- max_models@results.partitions %>%
        group_by(tune.args) %>%
        dplyr::summarize(n.partitions = length(fold)) %>%
        data.frame()
      # Occasionally, there are no results for one fold (maybe the model didn't
      # run ok?). And occasionally, average CBI value is NA. Should remove these 
      # models from consideration. 
      optimal <- max_models@results %>% 
        left_join(., other_summaries, by = "tune.args") %>%
        dplyr::filter(n.partitions == 4) %>%
        dplyr::filter(!is.na(cbi.val.avg)) %>%
        mutate(fc = factor(fc, levels = c("L", "LQ", "H", "LQH")))
      # If all mean CBIs are negative (which is very rare), pick the model with the 
      # highest mean CBI. If at least one mean CBI is positive, eliminate models with 
      # negative mean CBIs. Of those remaining, use minimum average or.10p. Break any
      # ties by selecting model with the maximum average auc.val
      if (max(optimal$cbi.val.avg) <= 0) {
        optimal <- optimal %>%
          dplyr::filter(cbi.val.avg == max(cbi.val.avg)) %>%
          dplyr::filter(or.10p.avg == min(or.10p.avg)) %>%
          dplyr::filter(auc.val.avg == max(auc.val.avg)) %>%
          dplyr::filter(as.numeric(rm) == max(as.numeric(rm))) %>%
          dplyr::filter(as.numeric(fc) == min(as.numeric(fc)))
      } else {
        optimal <- optimal %>%
          dplyr::filter(cbi.val.avg > 0) %>%
          dplyr::filter(or.10p.avg == min(or.10p.avg)) %>%
          dplyr::filter(auc.val.avg == max(auc.val.avg)) %>%
          dplyr::filter(as.numeric(rm) == max(as.numeric(rm))) %>%
          dplyr::filter(as.numeric(fc) == min(as.numeric(fc)))
      }
      
      # Save best model to file
      max_best <- max_models@models[[optimal$tune.args]]
      max_file <- paste0("output/SDMs/", nice_name, "-maxent.rds")
      saveRDS(max_best, max_file)  
      
      # Extract evaluation metrics for the model with optimal tuning parameters
      partitions <- max_models@results.partitions %>%
        dplyr::filter(tune.args == optimal$tune.args)
      eval_rows <- which(evals$sdm == "MAXENT")
      evals$tune.args[eval_rows] <- as.character(partitions$tune.args)
      evals$AUC[eval_rows] <- partitions$auc.val
      evals$CBI[eval_rows] <- partitions$cbi.val
      evals$threshold[eval_rows] <- partitions$thr
      evals$OR[eval_rows] <- partitions$OR.mss
      evals$TSS[eval_rows] <- partitions$TSS
      
      # Tuning BRT models -----------------------------------------------------------#
      # We're running a BRT model for each set of training data, identifying the 
      # optimal learning rate and number of trees. We'll use the mean of these
      # values when running CV models. 
      
      message("Tuning BRT models for ", species_name)
      
      # Create empty vectors to hold optimal tuning parameters for each fold
      learningrate_cv <-  rep(NA_real_, 4)
      ntrees_cv <- rep(NA_integer_, 4)
      
      # Set tree complexity
      complexity <- 5
      
      for (k in 1:nfolds) {
        sdmtrain <- pa_data %>%
          dplyr::filter(fold != k)
        
        brt_fit <- run_brt(full_data = sdmtrain, step = TRUE, 
                           complexity = complexity, verbose = FALSE)  
        
        if (!is.null(brt_fit)) {
          ntrees_cv[k] <- brt_fit$gbm.call$best.trees
          learningrate_cv[k]<- brt_fit$gbm.call$learning.rate
        }
      }    
      
      # Identify best tuning parameters across folds
      # (Most of the time the learning rate [lr] will be the same across folds. If
      # it's not, then average only over those folds with the smaller lr)
      lr_min <- min(learningrate_cv)
      ntrees_cv <- ntrees_cv[which(learningrate_cv == lr_min)]
      ntrees <- round(mean(ntrees_cv))
      
      # Run CV models for BRT, GAM, LASSO, RF ---------------------------------------#
      
      message("Running CV models for ", species_name)
      
      for (k in 1:nfolds) {
        # Create training and testing datasets for all SDMs
        sdmtrain <- pa_data %>%
          dplyr::filter(fold != k)
        sdmtest <- pa_data %>%
          dplyr::filter(fold == k)  
        
        # Calculate means, SDs for standardizing covariates (for GAM, LASSO)
        stand_obj <- save_means_sds(sdmtrain, cols = climate_vars, verbose = TRUE)
        
        # Run BRT and evaluate with test data
        brt_fit <- run_brt(full_data = sdmtrain, step = FALSE, ntrees = ntrees,
                           complexity = complexity, learning.rate = lr_min,
                           verbose = TRUE)
        
        if (!is.null(brt_fit)) {
          ev <- evaluate_sdm(test_data = sdmtest, 
                             model = brt_fit, 
                             sdm_method = "brt") 
          
          row_index <- which(evals$sdm == "BRT" & evals$fold == k)  
          tune.args <- paste0("tree.", ntrees, "_node.", complexity, "_lr.", lr_min)
          evals$tune.args[row_index] <- tune.args
          evals$AUC[row_index] <- ev$auc
          evals$CBI[row_index] <- ev$cbi
          evals$threshold[row_index] <- ev$threshold
          evals$OR[row_index] <- ev$or
          evals$TSS[row_index] <- ev$tss 
        }
        
        # Run GAM and evaluate with test data
        gam_fit <- run_gam(full_data = sdmtrain, 
                           stand_obj = stand_obj,
                           quad = FALSE)
        
        ev <- evaluate_sdm(test_data = sdmtest, 
                           model = gam_fit, 
                           sdm_method = "gam",
                           stand_obj = stand_obj,
                           quad = FALSE) 
        
        row_index <- which(evals$sdm == "GAM" & evals$fold == k)  
        evals$AUC[row_index] <- ev$auc
        evals$CBI[row_index] <- ev$cbi
        evals$threshold[row_index] <- ev$threshold
        evals$OR[row_index] <- ev$or
        evals$TSS[row_index] <- ev$tss 
        
        # Run LASSO and evaluate with test data
        lasso_fit <- run_lasso(full_data = sdmtrain, 
                               stand_obj = stand_obj,
                               quad = TRUE)
        
        ev <- evaluate_sdm(test_data = sdmtest, 
                           model = lasso_fit, 
                           sdm_method = "lasso",
                           stand_obj = stand_obj,
                           quad = TRUE) 
        
        row_index <- which(evals$sdm == "LASSO" & evals$fold == k)  
        evals$tune.args[row_index] <- "lambda.1se"
        evals$AUC[row_index] <- ev$auc
        evals$CBI[row_index] <- ev$cbi
        evals$threshold[row_index] <- ev$threshold
        evals$OR[row_index] <- ev$or
        evals$TSS[row_index] <- ev$tss 
        
        # Run RF and evaluate with test data
        rf_fit <- run_rf(full_data = sdmtrain,
                         importance = TRUE,
                         ntree = 1000)
        
        ev <- evaluate_sdm(test_data = sdmtest, 
                           model = rf_fit, 
                           sdm_method = "rf") 
        
        row_index <- which(evals$sdm == "RF" & evals$fold == k) 
        tune.args <- paste0("tree.", rf_fit$forest$ntree, "_mtry.3")
        evals$tune.args[row_index] <- tune.args
        evals$AUC[row_index] <- ev$auc
        evals$CBI[row_index] <- ev$cbi
        evals$threshold[row_index] <- ev$threshold
        evals$OR[row_index] <- ev$or
        evals$TSS[row_index] <- ev$tss 
      }
      # Write evals table to file
      write.csv(evals, 
                file = eval_file,
                row.names = FALSE)
      # Final message about process complete
      message("Model evaluation complete for ", species_name)
    } else { # For missing data
      warning("No presence / absence data for ", species_name, 
              " on disk; no evaluations performed.")
    }
  } else {
    message("Evaluations for ", species_name, 
            " already on disk and rerun set to ", rerun, ".")
  }
}

#' Calculate evaluation metrics; passed to \code{user.eval} argument of 
#' \code{ENMeval::ENMevaluate}.
#' 
#' @param vars list including different data that can be used to calculate the 
#' evaluation metric; 
#' @return data.frame with TSS, OR, and threshold value for an SDM
em <- function(vars) {
  perf_metrics <- flexsdm::sdm_eval(p = vars$occs.val.pred, 
                                    a = vars$bg.val.pred,
                                    thr = "max_sens_spec")
  out <- data.frame(TSS = perf_metrics$TSS,
                    OR.mss = perf_metrics$OR,
                    thr = perf_metrics$thr_value)
  return(out)
}
