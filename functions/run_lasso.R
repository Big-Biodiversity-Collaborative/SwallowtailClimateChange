#' Run lasso regression species distribution model
#' 
#' @param full_data dataframe with presence-absence data (1/0), fold (for 
#' separating testing and training data), and 19 columns with climate data
#' @param quad logical indicating whether or not to include quadratic terms 
#' for each continuous covariate
#' @param verbose logical indicating whether or not to print processing messages
#' 
#' @details Uses \code{glmnet::cv.glmnet()} to run lasso regression models and 
#' identify a value for lambda (via k-fold cross validation). 
#' 
#' @return a list with the following elements:
#' \describe{
#'   \item{model}{Lasso regression SDM; the output of 
#'   \code{glmnet::cv.glmnet()}}
#'   \item{evaluation}{Evaluation of model using testing data; the output of 
#'   \code{evaluate_lasso()}}
#'   \item{thresh}{Threshold value of probabilities for determining absence or 
#'   presence; the output of \code{dismo::threshold()} with 
#'   \code{stat = "spec_sens"}}
#'   \item{standardize_objects}{an object of class save_means_sds that contains
#'   lists of predictor names with means and SDs based on training data}
#'   \item{quad}{a logical indicating whether or not quadratics were included in
#'   the model}
#' }

run_lasso <- function(full_data, quad = TRUE, verbose = TRUE) {
  # Extract the name of this function for reporting
  function_name <- as.character(match.call())[1]
  
  # Libraries required for this function to work
  method_name <- "lasso"
  dependencies <- c("dplyr", "dismo", "glmnet")
  if (!all(unlist(lapply(X = dependencies, FUN = require, character.only = TRUE)))) {
    stop("At least one package required by ", function_name, 
         " could not be loaded: ", paste(dependencies, collapse = ", "),
         " are required.")
  }
  
  # Make sure presence-absence data are there
  if (!("pa" %in% colnames(full_data))) {
    stop(function_name, " requires column named 'pa' in full_data")
  }
  # Make sure fold indicators are there
  if (!("fold" %in% colnames(full_data))) {
    stop(function_name, " requires column named 'fold' in full_data")
  }
  # Make sure data for all 19 climate variables are there
  if (length(setdiff(paste0("bio", 1:19),colnames(full_data))) > 0) {
    stop(function_name, " requires bio1:bio19 columns in full_data")
  }
  
  predvars <- paste0("bio", 1:19)
  # Create separate data frames for testing and training presence data
  presence_train <- full_data %>%
    filter(pa == 1) %>%
    filter(fold != 1)
  presence_test <- full_data %>%
    filter(pa == 1) %>%
    filter(fold == 1) %>%
    dplyr::select(all_of(predvars))
  # Create separate data frames for testing and training (pseudo)absence data
  absence_train <- full_data %>%
    filter(pa == 0) %>%
    filter(fold != 1)
  absence_test <- full_data %>%
    filter(pa == 0) %>%
    filter(fold == 1) %>%
    dplyr::select(all_of(predvars))
  
  # Add presence and pseudoabsence training data into single data frame
  sdmtrain <- rbind(presence_train, absence_train)
  
  # Calculate (and save) means, SDs for standardizing covariates
  stand_obj <- save_means_sds(sdmtrain, cols = paste0("bio", 1:19), verbose = TRUE)
  # Standardize values in training dataset (to include quadratics, set quad = TRUE)
  sdmtrain_preds <- prep_predictors(stand_obj, sdmtrain, quad = quad) 
  sdmtrain <- cbind(sdmtrain[,1:2], sdmtrain_preds)
  
  # Creating values to downweight background points (so total (summed) 
  # weight of background pts equal to the total weight of presence pts)
  prNum <- sum(sdmtrain$pa == 1)
  bgNum <- sum(sdmtrain$pa == 0)
  wt <- ifelse(sdmtrain$pa == 1, 1, prNum / bgNum)
  
  if(verbose) {
    message("Running ", method_name, ".")
  }  
  # Run model  
  model_fit <- glmnet::cv.glmnet(x = as.matrix(sdmtrain[,3:ncol(sdmtrain)]),
                                 y = sdmtrain$pa,
                                 family = "binomial",
                                 alpha = 1,
                                 weights = wt,
                                 standardize = FALSE)
  
  # Identify the value of lambda that gives the most regularized model such that 
  # the cross-validated error is within one SE of the minimum (recommended)
  lambda_1se <- model_fit$lambda.1se
  # Coefficients at lambda.1se
  # coef(model_fit, s = lambda_1se)
  
  if(verbose) {
    message("Model complete. Evaluating ", method_name, 
            " with testing data.")
  }
  
  # Prep testing dataset
  presence_test <- prep_predictors(stand_obj, presence_test, quad = quad) 
  absence_test <- prep_predictors(stand_obj, absence_test, quad = quad)
  
  # Evaluate model performance with testing data
  model_eval <- evaluate_lasso(p = as.matrix(presence_test), 
                               a = as.matrix(absence_test), 
                               model = model_fit,
                               s = lambda_1se,
                               type = "response")
  
  # Calculate threshold so we can make a P/A map later
  pres_threshold <- dismo::threshold(x = model_eval, 
                                     stat = "spec_sens")
  
  # Bind everything together and return as list
  # Note: including a couple additional items that are needed for predictions
  # standardize_objects: lists of predictors, and lists of the means and SDs
  # used to standardize them
  # quad: logical that indicates whether quadratic terms were included
  results <- list(model = model_fit,
                  evaluation = model_eval,
                  thresh = pres_threshold, 
                  standardize_objects = stand_obj,
                  quad = quad)
  
  return(results)
}
