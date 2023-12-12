#' Run generalized additive model (GAM) species distribution model
#'
#' @param full_data dataframe with presence-absence data (1/0), columns with 
#' climate data
#' @param stand_obj an object of class save_means_sds (created with the 
#' save_means_sds function) that contains means and SDs for predictor variables 
#' in a training dataset
#' @param quad a logical indicating whether or not quadratics should be included 
#' in model (default = FALSE)
#' 
#' @details Uses \code{mgcv::gam()} for a generalized additive model
#' 
#' @return An object of class gbm, with parameter estimates for a generalized
#' additive model SDM. Output of \code{mgcv::gam()}.

run_gam <- function(full_data, stand_obj, quad = FALSE) {
  # Extract the name of this function for reporting
  function_name <- as.character(match.call())[1]
  
  if (!require(mgcv)) {
    stop(function_name, " requires mgcv package, but it could not be loaded")
  }
  # Make sure presence-absence data are there
  if (!("pa" %in% colnames(full_data))) {
    stop(function_name, " requires column named 'pa' in full_data")
  }
  # Make sure multiple climate variables are there
  if (sum(grepl("bio", colnames(full_data))) < 2) {
    stop(function_name, " requires multiple climate variables in full_data")
  }
  if(is.null(stand_obj) | is.null(quad)) {
    stop(function_name, " requires stand_obj and quad arguments")
  } 
  
  # Identify climate variables in model
  climate_vars <- stand_obj$names	
  
  # Standardize variables before running model
  dat_z <- prep_predictors(stand_obj, full_data, quad = quad) 
  dat_z <- cbind(pa = full_data$pa, dat_z)	
  
  # Select type of smooth functions
  # Use "s" for a spline-based smooth
  # Use "te" for a tensor product smooth (good when variables have diff units)
  smooth <- "s"
  
  # Create model formula
  model_formula <- paste0(smooth, "(", climate_vars, "_1)")
  model_formula <- paste(model_formula, collapse = " + ")
  model_formula <- as.formula(paste0("pa ~ ", model_formula))
  
  # Model adds a double penalty to remove variables that don't add to the model
  gam_fit <- gam(model_formula,
                 data = dat_z,
                 family = binomial, 
                 method = 'REML', 
                 select = TRUE)
  
  results <- gam_fit
  
  return(results)
}