# Run generalized additive model species distribution model
# Rachel Laura
# rlaura@arizona.edu
# 2022-10-25

#' Run generalized additive model species distribution model
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
#'   presence; the output of \code{dismo::threshold} with \code{stat = "spec_sens"}}
#' }
run_gam <- function(full_data, verbose = TRUE) {
  # Extract the name of this function for reporting
  function_name <- as.character(match.call())[1]
  
  # Libraries required for this function to work
  method_name <- "generalized additive model"
  dependencies <- c("dplyr", "dismo", "mgcv")
  
  ###CHECK THAT ALL THESE DEPENDECIES ARE NEEDED HERE
  
  if (!all(unlist(lapply(X = dependencies, FUN = require, character.only = TRUE)))) {
    stop("At least one package required by ", function_name, 
         " could not be loaded: ", paste(dependencies, collapse = ", "),
         " are required.")
  }
  
  # 
  # if (!require(dplyr)) {
  #   stop("run_glm requires dplyr package, but it could not be loaded")
  # }
  # if (!require(dismo)) {
  #   stop("run_glm requires dismo package, but it could not be loaded")
  # }

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
  
  if(verbose) {
    message("Running ", method_name, ".")
  }  
  # Run the model, specifying model with standard formula syntax
  # Exclude bio3 (a function of bio2 & bio7) and bio7 (a function of bio5 and 
  # bio6)
  
  
  # Model below uses tensor product (te) smooths since variables have different
  # units and also uses a double penalty to remove variables that don't
  # add to the model
  
 # model_fit <- gam(pa ~ te(bio1)+te(bio2)+te(bio4)+te(bio5)+te(bio6)+te(bio8)
#                   +te(bio9)+te(bio10)+te(bio11)+te(bio12)+te(bio13)+te(bio14)
 #                  +te(bio15)+te(bio16)+te(bio17)+te(bio18)+te(bio19),
  #                 data = sdmtrain,family = binomial, method = 'REML', 
  #                 select = TRUE)
  
  # Model below uses standard smoothing and adds a double penalty
  
  model_fit <- gam(pa ~ s(bio1)+s(bio2)+s(bio4)+s(bio5)+s(bio6)+s(bio8)
                  +s(bio9)+s(bio10)+s(bio11)+s(bio12)+s(bio13)+s(bio14)
                  +s(bio15)+s(bio16)+s(bio17)+s(bio18)+s(bio19),
                   data = sdmtrain,family = binomial, method = 'REML', select = TRUE)
  
  if(verbose) {
    message("Model complete. Evaluating ", method_name, 
            " with testing data.")
  }
  
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
                  thresh = pres_threshold)
  
  return(results)
}