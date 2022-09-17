#' Run maxent species distribution model (with tuning) using  ENMeval package
#' 
#' @param pa_data dataframe with presence-absence data (1/0) and fold (for 
#' separating testing and training data)
#' @param predictors rasters with climate data
#' @param verbose logical indicating whether or not to print processing messages
#' 
#' @details Uses \code{ENMeval::ENMevaluate()} for both tuning and running the 
#' final maxent model. Note that the algorithm we're using requires that a 
#' maxent.jar file be installed in the dismo package java folder. That is the 
#' folder returned by system.file("java", package = "dismo").
#' 
#' @return a list with the following elements:
#' \describe{
#'   \item{model}{Maxent SDM, after parameter tuning; one of the models in
#'   \code{ENMeval::ENMevaluate}}
#'   \item{evaluation}{Evaluation of selected model using testing data; the 
#'   output of \code{dismo::evaluate}}
#'   \item{thresh}{Threshold value of probabilities for determining absence or 
#'   presence; the output of \code{dismo::threshold} with 
#'   \code{stat = "spec_sens"}}
#'   \item{feature_class}{feature class of the selected Maxent SDM}
#'   \item{multiplier}{regularization multiplier of the selected Maxent SDM}
#' }
run_maxent_tune <- function(pa_data, predictors, verbose = TRUE) {
  if (!require(ENMeval)) {
    stop("run_maxent_tune requires ENMeval package, but it could not be loaded")
  }  
  if (!require(dplyr)) {
    stop("run_maxent_tune requires dplyr package, but it could not be loaded")
  }
  if (!require(dismo)) {
    stop("run_maxent_tune requires dismo package, but it could not be loaded")
  }
  jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep="")
  if(!file.exists(jar)) {
    stop("run_maxent_notune requires maxent.jar file, but it doesn't exist")
  }

  # Make sure presence-absence data are there
  if (!("pa" %in% colnames(pa_data))) {
    stop("run_maxent_notune requires column named 'pa' in full_data")
  }
  # Make sure fold indicators are there
  if (!("fold" %in% colnames(pa_data))) {
    stop("run_maxent_notune requires column named 'fold' in full_data")
  }

  # Set parameter values for ENMevaluate()
  feature_classes <- c("L", "LQ", "H", "LQH")
  multipliers <- 1:3
  tune.args <- list(fc = feature_classes, rm = multipliers)
  
  # For parallel processing, use two fewer cores than are available
  num_cores <- parallel::detectCores() - 2
  
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
  buffered_mcp <- vect(shapefile_name)
  
  # Crop and mask predictor rasters
  pred_mask <- terra::crop(predictors, buffered_mcp)
  pred_mask <- terra::mask(pred_mask, buffered_mcp) 
  
  # Run (and evaluate) MaxEnt models with each combination of feature class
  # and regularization multiplier.  Using random 5-fold cross validation 
  # within the training data to evaluate models. 
  maxent_models <- ENMevaluate(occs = presence_train,
                               envs = pred_mask,
                               bg = absence_train,
                               tune.args = tune.args,
                               algorithm = "maxent.jar",
                               partitions = "randomkfold",
                               partition.settings = list(kfolds = 5),
                               parallel = TRUE,
                               numCores = num_cores) 
  
  # Select the "best" model
  # For now, using minimum AICc as the criterion (see Warren and Seifert 
  # 2011; Warren et al. 2014). There's good reason not to use AUC, but could 
  # use CBI instead... 
  # NOTE: if we stick with AICc, we probably don't need to do the 5-fold CV
  best_index <- which(maxent_models@results$delta.AICc == 0)
  best_model <- maxent_models@models[[best_index]]
  fc_best <- as.character(eval.tune.settings(maxent_models)[best_index, "fc"])
  rm_best <- as.numeric(eval.tune.settings(maxent_models)[best_index, "rm"])
    # If you wanted to look at response curves:
    # dismo::response(maxent_models@models[[best_index]])
    # If you wanted to extract permutation importance for each variable:
    # best@results[grep("permutation", rownames(best@results)),]
  
  # Note: could use permutation importance or contribution of each variable 
  # along with pairwise correlations to reduce the number of covariates in the 
  # model (like Warren et al. 2014 did)
  
  if(verbose) {
    message("Model (and tuning) complete. Evaluating selected Maxent model with testing data.")
  }
  
  # Evaluate model performance with testing data
  maxent_eval <- dismo::evaluate(p = presence_test, 
                                 a = absence_test,
                                 model = best_model,
                                 x = pred_mask)
  
  # Calculate threshold so we can make a P/A map later
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
  
