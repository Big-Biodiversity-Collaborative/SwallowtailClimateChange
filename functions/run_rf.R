#' Run random forest species distribution model
#' 
#' @param full_data dataframe with presence-absence data (1/0), fold (for 
#' separating testing and training data), and 19 columns with climate data
#' @param verbose logical indicating whether or not to print processing messages
#' 
#' @details Uses \code{randomForest::randomForest()} for regression-type 
#' random forest model
#' 
#' @return a list with the following elements:
#' \describe{
#'   \item{model}{Random forest model SDM; the output of 
#'   \code{randomForest::randomForest()}}
#'   \item{evaluation}{Evaluation of model using testing data; the output of 
#'   \code{dismo::evaluate}}
#'   \item{thresh}{Threshold value of probabilities for determining absence or 
#'   presence; the output of \code{dismo::threshold} with 
#'   \code{stat = "spec_sens"}}
#' }

run_rf <- function(full_data, verbose = TRUE) {
  # Extract the name of this function for reporting
  function_name <- as.character(match.call())[1]
  
  # Libraries required for this function to work
  method_name <- "random forest"
  dependencies <- c("dplyr", "dismo", "randomForest")
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

  # Calculate sample sizes and weights
  prNum <- sum(sdmtrain$pa == 1)
  bgNum <- sum(sdmtrain$pa == 0)
  wt <- ifelse(sdmtrain$pa == 1, 1, prNum/bgNum)

  if(verbose) {
    message("Running ", method_name, ".")
  } 
  
  # Create model formula
  model_formula <- paste(climate_vars, collapse = " + ")
  model_formula <- as.formula(paste0("pa ~ ", model_formula))
  
  # Run model (set number of trees to 500 for now)
  model_fit <- suppressWarnings(randomForest(formula = model_formula,
                                             data = sdmtrain,
                                             ntree = 500,
                                             weights = wt,
                                             replace = TRUE))
  # Note: wrapping call to randomForest() in suppressWarnings() because
  # R will produce the following, expected warning:
  # In randomForest.default(m, y, ...) : The response has five or fewer unique 
  # values.  Are you sure you want to do regression?
  # See https://rspatial.org/raster/sdm/6_sdm_methods.html#random-forest
  
  if(verbose) {
    message("Model complete. Evaluating ", method_name, 
            " with testing data.")
  }
  
  # Evaluate model performance with testing data
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
                  thresh = pres_threshold)
  
  return(results)
}
