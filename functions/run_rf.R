#' Run random forest species distribution model
#' 
#' @param full_data dataframe with presence-absence data (1/0), fold (for 
#' separating testing and training data), and 19 columns with climate data
#' @param verbose logical indicating whether or not to print processing messages
#' 
#' @details Uses \code{stats::rf()} for random forest model
#' 
#' @return a list with the following elements:
#' \describe{
#'   \item{model}{Random forest model SDM; the output of \code{stats::rf}}
#'   with family = "logit"
#'   \item{evaluation}{Evaluation of model using testing data; the output of 
#'   \code{dismo::evaluate}}
#'   \item{thresh}{Threshold value of probabilities for determining absence or 
#'   presence; the output of \code{dismo::threshold} with \code{stat = "spec_sens"}}
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
  
  #convert the response to factor for RF model to return probabilities -- 11/8/22 not doing this
  #sdmtrain$pa <- as.factor(sdmtrain$pa)
 
  # calculating the class weights and sample size
  prNum <- sum(sdmtrain$pa == 1)
  bgNum <- sum(sdmtrain$pa == 0)
  wt <- ifelse(sdmtrain$pa == 1, 1, prNum/bgNum)
  
  # calculating the class weights and sample size
  #prNum <- as.numeric(table(sdmtrain$pa)["1"]) # number of presences
  
  #cwt <- c("1" = 1, "0" = prNum / bgNum)
  #samsize <- c("0" = prNum, "1" = prNum)
  
  if(verbose) {
    message("Running ", method_name, ".")
  }  
  # Run the model, specifying model with standard formula syntax
  # Exclude bio3 (a function of bio2 & bio7) and bio7 (a function of bio5 and 
  # bio6)
  # model_fit <- stats::glm(pa ~ bio1 + bio2 + bio4 + bio5 + bio6 +
  #                           bio8 + bio9 + bio10 + bio11 + bio12 +
  #                           bio13 + bio14 + bio15 + bio16 + bio17 + bio18 +
  #                           bio19,
  #                         data = sdmtrain,
  #                         family = binomial(link = "logit"))
  
 model_fit <- randomForest(pa ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 +
                              bio7 + bio8 + bio9 + bio12 +
                              bio13 + bio14 + bio15 + bio18 +
                              bio19,
                            data = sdmtrain,
                            ntree = 100,
                            weights = wt,
                            replace = TRUE)
  
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
