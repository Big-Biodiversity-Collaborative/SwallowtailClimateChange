#' Run maxent species distribution model (no tuning) using dismo package
#' 
#' @param full_data dataframe with presence-absence data (1/0), fold (for 
#' separating testing and training data), and 19 columns with climate data
#' @param verbose logical indicating whether or not to print processing messages
#' 
#' @details Uses \code{dismo::maxent()} for maxent model. Note that the program
#' needs to be downloaded first, and the maxent.jar file needs to be in the
#' 'java' folder of the 'dismo' package. That is the folder returned by 
#' system.file("java", package = "dismo").
#' 
#' @return a list with the following elements:
#' \describe{
#'   \item{model}{Maxent SDM, without parameter tuning; the output of 
#'   \code{dismo::maxent}}
#'   \item{evaluation}{Evaluation of model using testing data; the output of 
#'   \code{dismo::evaluate}}
#'   \item{probs}{Occurrence probabilities predicted from SVM model}
#'   \item{thresh}{Threshold value of probabilities for determining absence or 
#'   presence; the output of \code{dismo::threshold} with 
#'   \code{stat = "spec_sens"}}
#' }
run_maxent_notune <- function(full_data, verbose = TRUE) {
  if (!require(dplyr)) {
    stop("run_maxent_notune requires dplyr package, but it could not be loaded")
  }
  if (!require(dismo)) {
    stop("run_maxent_notune requires dismo package, but it could not be loaded")
  }
  jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep="")
  if(!file.exists(jar)) {
    stop("run_maxent_notune requires maxent.jar file, but it doesn't exist")
  }

  # Make sure presence-absence data are there
  if (!("pa" %in% colnames(full_data))) {
    stop("run_maxent_notune requires column named 'pa' in full_data")
  }
  # Make sure fold indicators are there
  if (!("fold" %in% colnames(full_data))) {
    stop("run_maxent_notune requires column named 'fold' in full_data")
  }
  # Make sure data for all 19 climate variables are there
  if (length(setdiff(paste0("bio",1:19),colnames(full_data))) > 0) {
    stop("run_maxent_notune requires bio1:bio19 columns in full_data")
  }
  
  # Arrange predictor columns in full_data (so they appear in order)
  predvars <- paste0("bio", 1:19)
  full_data <- select(full_data, c("pa","fold",all_of(predvars)))
  
  # Create separate data frames for testing and training presence data
  presence_train <- full_data %>%
    filter(pa == 1) %>%
    filter(fold != 1)
  presence_test <- full_data %>%
    filter(pa == 1) %>%
    filter(fold == 1) %>%
    dplyr::select(predvars)
  # Create separate data frames for testing and training (pseudo)absence data
  absence_train <- full_data %>%
    filter(pa == 0) %>%
    filter(fold != 1)
  absence_test <- full_data %>%
    filter(pa == 0) %>%
    filter(fold == 1) %>%
    dplyr::select(predvars)
  
  # Add presence and pseudoabsence training data into single data frame
  sdmtrain <- rbind(presence_train, absence_train)

  # Identify a folder to store maxent files
  max_file <- "development/output/maxent"
  
  if(verbose) {
    message("Running MaxExt model.")
  }  

  # Run a Maxent model, without tuning model parameters
  maxent_model <- dismo::maxent(x = sdmtrain[,3:ncol(sdmtrain)],
                                p = sdmtrain$pa,
                                removeDuplicates = FALSE,
                                path = max_file,
                                args = "nothreshold")

  if(verbose) {
    message("Model complete. Evaluating Maxent model with testing data.")
  }
  # Evaluate model performance with testing data
  maxent_eval <- dismo::evaluate(p = presence_test, 
                                 a = absence_test,
                                 model = maxent_model)
  
  # Calculate threshold so we can make a P/A map later
  pres_threshold <- dismo::threshold(x = maxent_eval, 
                                     stat = "spec_sens")
  
  # Bind everything together and return as list  
  results <- list(model = maxent_model,
                  evaluation = maxent_eval,
                  thresh = pres_threshold)
  
  return(results)
}
