#' Run lasso regression species distribution model
#' 
#' @param full_data dataframe with presence-absence data (1/0), columns with 
#' climate data
#' @param stand_obj an object of class save_means_sds (created with the 
#' save_means_sds function) that contains means and SDs for predictor variables 
#' in a training dataset
#' @param quad a logical indicating whether or not quadratics should be included 
#' in model (default = TRUE)
#' 
#' @details Uses \code{glmnet::cv.glmnet()} to run lasso regression models and 
#' identify a value for lambda (via k-fold cross validation). 
#' 
#' @return An object of class cv.glmnet, with parameter estimates for a lasso 
#' regression SDM. Output of \code{glmnet::cv.glmnet()}.

run_lasso <- function(full_data, stand_obj, quad = TRUE) {
  # Extract the name of this function for reporting
  function_name <- as.character(match.call())[1]
  
  if (!require(glmnet)) {
    stop(function_name, " requires glmnet package, but it could not be loaded")
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
  
  # Standardize variables before running model
  dat_z <- prep_predictors(stand_obj, full_data, quad = quad) 
  
  # Creating values to downweight points (so total [summed] weight of 
  # background points is equal to the total weight of presence points).
  prNum <- as.numeric(table(full_data$pa)["1"])
  bgNum <- as.numeric(table(full_data$pa)["0"])
  if (prNum <= bgNum) {
    wt <- ifelse(full_data$pa == 1, 1, prNum / bgNum)
  } else {
    wt <- ifelse(full_data$pa == 0, 1, bgNum / prNum)
  }
  
  lasso_fit <- cv.glmnet(x = as.matrix(dat_z),
                         y = full_data$pa,
                         family = "binomial",
                         alpha = 1,
                         weights = wt,
                         standardize = FALSE)
  
  results <- lasso_fit
  
  return(results)
}
