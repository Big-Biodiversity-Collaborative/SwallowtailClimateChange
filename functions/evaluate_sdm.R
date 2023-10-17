#' Evaluate SDM model performance
#' 
#' @param test_data dataframe with presence-absence data (1/0) and climate data 
#' associated with a test dataset
#' @param model species distribution model object
#' @param sdm_method character indicating the type of SDM ("brt", "gam", 
#' "lasso", "rf")
#' @param stand_obj an object of class save_means_sds (created with the 
#' save_means_sds function) that contains means and SDs for predictor variables 
#' in a training dataset (Optional, except for lasso regression and gam models)
#' @param quad a logical indicating whether or not quadratics were included 
#' in model (Optional, except for lasso regression and gam models)
#' 
#' @details 
#'
#' @return a list with the following elements:
#' \describe{
#'   \item{auc}{AUC value}
#'   \item{cbi}{Continuous Boyce Index}
#'   \item{threshold}{suitability value that maximizes sensitivity + 
#'   specificity, and can be used to estimate a species' distribution}
#'   \item{or}{Omission rate, based on the max(sens + spec) threshold}
#'   \item{tss}{maximum value of the True Skill Statistic (at max(sens +
#'   spec) threshold)}
#' }

evaluate_sdm <- function(test_data, 
                         model, 
                         sdm_method = c("brt", "gam", "lasso", "rf"), 
                         stand_obj = NULL,
                         quad = NULL) {
  
  sdm_method <- match.arg(arg = sdm_method)
  
  # Extract the name of this function for reporting
  function_name <- as.character(match.call())[1]
  
  if (!require(dplyr)) {
    stop(function_name, " requires dplyr package, but it could not be loaded")
  }
  if (!require(dismo)) {
    stop(function_name, " requires dismo package, but it could not be loaded")
  }
  if (!require(ecospat)) {
    stop(function_name, " requires ecospat package, but it could not be loaded")
  }
  if (!require(flexsdm)) {
    stop(function_name, " requires flexsdm package, but it could not be loaded")
  }
  if (sdm_method == "brt") {
    if (!require(gbm)) {
      stop(function_name, " requires gbm package, but it could not be loaded")    
    }
  }
  if (sdm_method == "gam") {
    if (!require(mgcv)) {
      stop(function_name, " requires mgcv package, but it could not be loaded")    
    }
  }
  if (sdm_method == "lasso") {
    if (!require(glmnet)) {
      stop(function_name, " requires glmnet package, but it could not be loaded")       
    }
  }
  if (sdm_method == "rf") {
    if (!require(randomForest)) {
      stop(function_name, " requires randomForest package, but it could not be loaded")       
    }
  }
  if (sdm_method == "gam" | sdm_method == "lasso") {
    if(is.null(stand_obj) | is.null(quad)) {
      stop(function_name, " requires stand_obj and quad arguments")
    }
  }
  
  pa <- test_data$pa
  
  clim_data <- test_data %>%
    dplyr::select(contains("bio"))
  
  if (sdm_method %in% c("gam", "lasso")) {
    clim_data <- prep_predictors(stand_obj, clim_data, quad = quad) 
  }
  
  p <- clim_data[which(pa == 1),]
  a <- clim_data[which(pa == 0),]

  # Use dismo::evaluate to get predicted suitability values for presence and 
  # pseudo-absence locations
  if (sdm_method == "rf") {
    rf_preds <- predict(model, clim_data, type = "prob")[, 2]
    eval <- dismo::evaluate(p = rf_preds[which(pa == 1)],
                            a = rf_preds[which(pa == 0)])
  } else if (sdm_method == "lasso") {
    eval <- evaluate_lasso(p = as.matrix(p),
                           a = as.matrix(a),
                           model = model,
                           s = model$lambda.1se,
                           type = "response")
  } else {
    eval <- dismo::evaluate(p = p, 
                            a = a, 
                            model = model, 
                            type = "response")
  }

  # Use ecospat::ecospat.boyce to get Continuous Boyce Index (CBI)
  boyce <- ecospat::ecospat.boyce(fit = c(eval@presence, eval@absence),
                                  obs = eval@presence,
                                  nclass = 0,
                                  PEplot = FALSE)
  
  # Use flexsdm::sdm_eval to get threshold and all other evaluation metrics
  em <- flexsdm::sdm_eval(p = eval@presence,
                          a = eval@absence,
                          thr = "max_sens_spec")
  
  # Bind everything together and return as list  
  results <- list(auc = em$AUC,
                  cbi = boyce$cor,
                  threshold = em$thr_value,
                  or = em$OR,
                  tss = em$TSS)

  return(results)  
}
