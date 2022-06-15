#' Run support vector machine species distribution model using e1071 package
#' 
#' @param full_data dataframe with presence-absence data (1/0), fold (for 
#' separating testing and training data), and 19 columns with climate data
#' @param verbose logical indicating whether or not to print processing messages
#' 
#' @details Uses \code{e1071::svm()} for support vector machine model
#' 
#' @return a list with the following elements:
#' \describe{
#'   \item{model}{Support vector machine SDM; the output of \code{e1071::svm}}
#'   \item{evaluation}{Evaluation of model using testing data; the output of 
#'   \code{dismo::evaluate}}
#'   \item{probs}{Occurrence probabilities predicted from SVM model}
#'   \item{thresh}{Threshold value of probabilities for determining absence or 
#'   presence; the output of \code{dismo::threshold} with \code{stat = "spec_sens"}}
#' }
run_svmw_e1071 <- function(full_data, verbose = TRUE) {
  if (!require(dplyr)) {
    stop("run_svmw_e1071 requires dplyr package, but it could not be loaded")
  }
  if (!require(dismo)) {
    stop("run_svmw_e1071 requires dismo package, but it could not be loaded")
  }
  if (!require(e1071)) {
    stop("run_svmw_e1071 requires e1071 package, but it could not be loaded")
  }
  # Make sure presence-absence data are there
  if (!("pa" %in% colnames(full_data))) {
    stop("run_svmw_e1071 requires column named 'pa' in full_data")
  }
  # Make sure fold indicators are there
  if (!("fold" %in% colnames(full_data))) {
    stop("run_svmw_kernlab requires column named 'fold' in full_data")
  }
  # Make sure data for all 19 climate variables are there
  if (length(setdiff(paste0("bio",1:19),colnames(full_data))) > 0) {
    stop("run_svmw_e1071 requires bio1:bio19 columns in full_data")
  }

  # Create separate data frames for testing and training presence data
  presence_train <- full_data %>%
    filter(pa == 1) %>%
    filter(fold != 1)
  presence_test <- full_data %>%
    filter(pa == 1) %>%
    filter(fold == 1)
  # Create separate data frames for testing and training (pseudo)absence data
  absence_train <- full_data %>%
    filter(pa == 0) %>%
    filter(fold != 1)
  absence_test <- full_data %>%
    filter(pa == 0) %>%
    filter(fold == 1)
  
  # Add presence and pseudoabsence training data into single data frame
  sdmtrain <- rbind(presence_train, absence_train)

  # Calculate weights for training dataset 
  # (presence and pseudoabsence data equally weighted)
  pNum_train <- nrow(presence_train)
  aNum_train <- nrow(absence_train)
  wt_train <- c("1" = 1, "0" = pNum_train / aNum_train)

  if(verbose) {
    message("Running support vector machine model.")
  }  
  # Run an SVM model, specifying model with standard formula syntax
  # Could exclude bio3 (a function of bio2 & bio7) and bio7 (a function of bio5 
  # and bio6), but for now including them all
  svm_model <- e1071::svm(pa ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 +
                            bio7 + bio8 + bio9 + bio10 + bio11 + bio12 +
                            bio13 + bio14 + bio15 + bio16 + bio17 + bio18 +
                            bio19,
                          kernel = "radial",
                          probability = TRUE,
                          # class.weights = wt_train,
                          data = sdmtrain)
  
  # ERROR: ‘class.weights’ are set to NULL for regression mode
  # Similar error when running code from Valavi et al.
  # Other potential options: kernlab::ksvm or WeightSVM::wsvm
  
  if(verbose) {
    message("Model complete. Evaluating SVM model with testing data.")
  }
  # Evaluate model performance with testing data
  svm_eval <- dismo::evaluate(p = presence_test, 
                              a = absence_test,
                              model = svm_model)
  
  # Calculate threshold so we can make a P/A map later
  pres_threshold <- dismo::threshold(x = svm_eval, 
                                     stat = "spec_sens")
  
  # Bind everything together and return as list  
  results <- list(model = svm_model,
                  evaluation = svm_eval,
                  thresh = pres_threshold)
  
  return(results)
}
