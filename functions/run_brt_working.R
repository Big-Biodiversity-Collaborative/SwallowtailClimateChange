#' Run boosted regression tree (BRT) species distribution model
#' 
#' @param full_data dataframe with presence-absence data (1/0), fold (for 
#' separating testing and training data), and 19 columns with climate data
#' @param verbose logical indicating whether or not to print processing messages
#' 
#' @details Uses \code{dismo::gbm.step()} for boosted regression tree model.
#' 
#' @return a list with the following elements:
#' \describe{
#'   \item{model}{Boosted regression tree SDM; the output of 
#'   \code{dismo::gbm.step()}
#'   \item{evaluation}{Evaluation of model using testing data; the output of 
#'   \code{dismo::evaluate()}}
#'   \item{thresh}{Threshold value of probabilities for determining absence or 
#'   presence; the output of \code{dismo::threshold} with 
#'   \code{stat = "spec_sens"}}
#' }
run_brt <- function(full_data, verbose = TRUE) {
  # Extract the name of this function for reporting
  function_name <- as.character(match.call())[1]
  
  # Libraries required for this function to work
  method_name <- "boosted regression tree"
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
  
  # create weights
  # set parameters (tc, lr, n.tree, max.trees, n.folds, )
  
  
  
  
  if(verbose) {
    message("Running ", method_name, ".")
  }  
  # Run the model
  #### NEED TO PUT IN WHILE LOOP to see if optimal number of trees is > 1000?
  #### AND see whether model_fit == NULL?  (under what circumstances could this happen?)
  model_fit <- gbm.step(data = ,
                        gbm.x = 3:ncol(data),
                        gbm.y = 1,
                        family = "bernoulli")
  
  if(verbose) {
    message("Model complete. Evaluating ", method_name, 
            " with testing data.")
  }
  # Evaluate model performance with testing data
  # The type = "response" is passed to predict.glm so the values are on the 
  # scale of 0 to 1 (probabilities), rather than the log odds. Required to make
  # sure the threshold is on the same scale of output from predict_sdm
  model_eval <- dismo::evaluate(data...
                                model = model_fit,
                                n.trees = model_fit$gmb.call$best.trees,
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