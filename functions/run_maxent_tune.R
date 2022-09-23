#' Run Maxent species distribution model (with tuning) using ENMeval package
#' 
#' @param pa_data dataframe with presence-absence data (1/0) and fold ID (for 
#' separating testing and training data)
#' @param predictors SpatRaster with climate data
#' @param verbose logical indicating whether or not to print processing messages
#' @param criteria character indicating the criteria that will be used to 
#' evaluate the performance of models with different tuning parameters 
#' 
#' @details Uses \code{ENMeval::ENMevaluate()} for both tuning and running the 
#' Maxent models. Note that the algorithm we're using requires a 
#' maxent.jar file be installed in the dismo package java folder. That is the 
#' folder returned by system.file("java", package = "dismo").
#' 
#' @return a list with the following elements:
#' \describe{
#'   \item{model}{Maxent SDM, after parameter tuning; one of the models returned 
#'   by \code{ENMeval::ENMevaluate()}}
#'   \item{evaluation}{Evaluation of selected model using testing data; the 
#'   output of \code{dismo::evaluate()}}
#'   \item{thresh}{Threshold value of probabilities for determining absence or 
#'   presence; the output of \code{dismo::threshold()} with 
#'   \code{stat = "spec_sens"}}
#'   \item{feature_class}{feature class of the selected Maxent SDM}
#'   \item{multiplier}{regularization multiplier of the selected Maxent SDM}
#' }
run_maxent_tune <- function(pa_data, predictors, verbose = TRUE,
                            criteria = c("AICc", "AUC", "CBI")) {
  # Extract the name of this function for reporting
  function_name <- as.character(match.call())[1]
  
  # Libraries required for this function to work
  method_name <- "maxent-tune"
  dependencies <- c("dplyr", "dismo", "ENMeval")
  if (!all(unlist(lapply(X = dependencies, FUN = require, character.only = TRUE)))) {
    stop("At least one package required by ", function_name, 
         " could not be loaded: ", paste(dependencies, collapse = ", "),
         " are required.")
  }  

  jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep="")
  if(!file.exists(jar)) {
    stop("run_maxent_notune requires maxent.jar file, but it doesn't exist")
  }

  # Make sure presence-absence data are there
  if (!("pa" %in% colnames(pa_data))) {
    stop("run_maxent_notune requires column named 'pa' in pa_data")
  }
  # Make sure fold indicators are there
  if (!("fold" %in% colnames(pa_data))) {
    stop("run_maxent_notune requires column named 'fold' in pa_data")
  }

  criteria <- match.arg(arg = criteria)

  # Create training and testing datasets
  presence_train <- pa_data %>%
    filter(pa == 1) %>%
    filter(fold != 1) %>%
    dplyr::select(x, y)
  absence_train <- pa_data %>%
    filter(pa == 0) %>%
    filter(fold != 1) %>%
    dplyr::select(x, y)
  presence_test <- pa_data %>%
    filter(pa == 1) %>%
    filter(fold == 1) %>%
    dplyr::select(x, y)
  absence_test <- pa_data %>%
    filter(pa == 0) %>%
    filter(fold == 1) %>%
    dplyr::select(x, y)

  # Get shapefile for geographic extent (to crop predictor rasters)
  shapefile_name <- paste0("data/gbif/shapefiles/",
                           nice_name, 
                           "-buffered-mcp.shp")
  # If species' shapefile isn't in shapefiles folder, unzip gbif-shapefiles
  if (!file.exists(shapefile_name)) {
    unzip(zipfile = "data/gbif-shapefiles.zip")
  }
  buffered_mcp <- vect(shapefile_name)
  
  # Crop and mask predictor rasters
  pred_mask <- terra::crop(predictors, buffered_mcp)
  pred_mask <- terra::mask(pred_mask, buffered_mcp) 
  
  # Set parameter values and arguments for ENMevaluate()
  feature_classes <- c("L", "LQ", "H", "LQH")
  multipliers <- 1:3
  tune.args <- list(fc = feature_classes, rm = multipliers)
  
  # Need to use 5-fold cross validation if using AUC, CBI as evaluation criteria
  if (criteria == "AICc") {
    partitions <- "none"
    partition.settings <- NULL
  } else {
    partitions <- "randomkfold"
    partition.settings <- list(kfolds = 5)
  }
  
  # For parallel processing, use two fewer cores than are available
  num_cores <- parallel::detectCores() - 2

  if(verbose) {
    message("Running ", method_name, ".")
  }   
  # Run (and evaluate) Maxent models with each combination of feature class
  # and regularization multiplier. 
  maxent_models <- ENMevaluate(occs = presence_train,
                               envs = pred_mask,
                               bg = absence_train,
                               tune.args = tune.args,
                               algorithm = "maxent.jar",
                               partitions = partitions, 
                               partition.settings = partition.settings,
                               parallel = TRUE,
                               numCores = num_cores) 
  
  # Select the "best" model
  best_index <- ifelse(criteria == "AICc",
                       which(maxent_models@results$delta.AICc == 0),
                ifelse(criteria == "AUC",
                       which.max(maxent_models@results$auc.val.avg),
                       which.max(maxent_models@results$cbi.val.avg)))       
                             
  best_model <- maxent_models@models[[best_index]]
  fc_best <- as.character(eval.tune.settings(maxent_models)[best_index, "fc"])
  rm_best <- as.numeric(eval.tune.settings(maxent_models)[best_index, "rm"])
    # If you wanted to inspect response curves:
    # dismo::response(maxent_models@models[[best_index]])

  # Note: We could use permutation importance of each variable along with 
  # pairwise correlations to reduce the number of covariates in the model 
  # (like Warren et al. 2014 did). If you wanted to extract permutation 
  # importance for each variable: 
  # best_model@results[grep("permutation", rownames(best_model@results)),]
  # Then you could re-run the model with testing data using only a subset of 
  # the climate variables.
  
  if(verbose) {
    message("Model (and tuning) complete. Evaluating selected Maxent model with testing data.")
  }
  
  # Evaluate model performance with testing data
  maxent_eval <- dismo::evaluate(p = presence_test, 
                                 a = absence_test,
                                 model = best_model,
                                 x = pred_mask)
  
  # Calculate threshold so we can make a P/A map later
  # TODO: Maxent predictions should be on cloglog scale. Need to make sure 
  # this threshold value is on the same scale.
  pres_threshold <- dismo::threshold(x = maxent_eval, 
                                     stat = "spec_sens")
  
  # Bind everything together and return as list
  # For Maxent models, including the tuning parameters
  results <- list(model = best_model,
                  evaluation = maxent_eval,
                  thresh = pres_threshold,
                  feature_class = fc_best,
                  multiplier = rm_best)

  return(results)
}    
  
