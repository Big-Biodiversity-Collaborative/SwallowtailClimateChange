#' Run generalized linear model species distribution model
#' 
#' @param full_data dataframe with presence-absence data (1/0), fold (for 
#' separating testing and training data), and 19 columns with climate data
#' @param verbose logical indicating whether or not to print processing messages
#' 
#' @details Uses \code{stats::glm()} for generalized linear model
#' 
#' @return a list with the following elements:
#' \describe{
#'   \item{model}{Generalized linear model SDM; the output of \code{stats::glm}}
#'   with family = "logit"
#'   \item{evaluation}{Evaluation of model using testing data; the output of 
#'   \code{dismo::evaluate}}
#'   \item{thresh}{Threshold value of probabilities for determining absence or 
#'   presence; the output of \code{dismo::threshold} with 
#'   \code{stat = "spec_sens"}}
#'   \item{standardize_objects}{an object of class save_means_sds that contains
#'   lists of predictor names with means and SDs based on training data}
#'   \item{quad}{A logical indicating whether or not quadratics were included in
#'   the model. Will always be set to FALSE for glm models}
#'   \item{climate_vars}{vector with names of all climate variables considered 
#'   in the model}
#' }
run_glm <- function(full_data, verbose = TRUE) {
  # Extract the name of this function for reporting
  function_name <- as.character(match.call())[1]
  
  # Libraries required for this function to work
  method_name <- "generalized linear model"
  dependencies <- c("dplyr", "dismo")
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
  if (length(setdiff(paste0("bio",1:19),colnames(full_data))) > 0) {
    stop(function_name, " requires bio1:bio19 columns in full_data")
  }

  # Get list of climate variables to consider for the SDM
  all_climate_vars <- read.csv("data/climate-variables.csv")
  climate_vars <- all_climate_vars$variable[all_climate_vars$include == TRUE]
  
  # Create separate data frames for testing and training presence data
  presence_train <- full_data %>%
    filter(pa == 1) %>%
    filter(fold != 1) %>%
    dplyr::select(pa, fold, all_of(climate_vars))
  presence_test <- full_data %>%
    filter(pa == 1) %>%
    filter(fold == 1) %>%
    dplyr::select(all_of(climate_vars))
  # Create separate data frames for testing and training (pseudo)absence data
  absence_train <- full_data %>%
    filter(pa == 0) %>%
    filter(fold != 1) %>%
    dplyr::select(pa, fold, all_of(climate_vars))
  absence_test <- full_data %>%
    filter(pa == 0) %>%
    filter(fold == 1) %>%
    dplyr::select(all_of(climate_vars))
  
  # Add presence and pseudoabsence training data into single data frame
  sdmtrain <- rbind(presence_train, absence_train)
  
  # Calculate (and save) means, SDs for standardizing covariates
  stand_obj <- save_means_sds(sdmtrain, cols = climate_vars, verbose = TRUE)
  # Standardize values in training dataset
  sdmtrain_preds <- prep_predictors(stand_obj, sdmtrain, quad = FALSE) 
  sdmtrain <- cbind(sdmtrain[,1:2], sdmtrain_preds)
  
  if(verbose) {
    message("Running ", method_name, ".")
  }  

  # Create model formula
  model_formula <- paste0(climate_vars, "_1")
  model_formula <- paste(model_formula, collapse = " + ")
  model_formula <- as.formula(paste0("pa ~ ", model_formula))
  
  model_fit <- stats::glm(formula = model_formula,
                          data = sdmtrain,
                          family = binomial(link = "logit"))
  
  if(verbose) {
    message("Model complete. Evaluating ", method_name, 
            " with testing data.")
  }
  
  # Prep testing dataset
  presence_test <- prep_predictors(stand_obj, presence_test, quad = FALSE) 
  absence_test <- prep_predictors(stand_obj, absence_test, quad = FALSE)
  
  # Evaluate model performance with testing data
  # The type = "response" is passed to predict.glm so the values are on the 
  # scale of 0 to 1 (probabilities), rather than the log odds. Required to make
  # sure the threshold is on the same scale of output from predict_sdm
  model_eval <- dismo::evaluate(p = presence_test, 
                                a = absence_test, 
                                model = model_fit,
                                type = "response") 
  
  # Calculate threshold so we can make a P/A map later
  pres_threshold <- dismo::threshold(x = model_eval, 
                                     stat = "spec_sens")
  
  # Bind everything together and return as list  
  results <- list(model = model_fit,
                  evaluation = model_eval,
                  thresh = pres_threshold,
                  standardize_objects = stand_obj,
                  quad = FALSE,
                  climate_vars = climate_vars)
  
  return(results)
}