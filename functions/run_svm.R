#' Run support vector machine species distribution model
#' 
#' @param obs data frame with longitude and latitude coordinates of occurrence
#' data
#' @param absence absence or pseudo-absence data longitude and latitude 
#' coordinates
#' @param predictors environmental predictor data to use for modeling
#' @param verbose logical indicating whether or not to print processing messages
#' 
#' @details Uses \code{kernlab::ksvm()} for support vector machine model
#' 
#' @return a list with the following elements:
#' \describe{
#'   \item{model}{Support vector machine SDM; the output of \code{kernlab::ksvm}}
#'   \item{evaluation}{Evaluation of model using testing data; the output of 
#'   \code{dismo::evaluate}}
#'   \item{probs}{Occurrence probabilities predicted from SVM model}
#'   \item{thresh}{Threshold value of probabilities for determining absence or 
#'   presence; the output of \code{dismo::threshold} with \code{stat = "spec_sens"}}
#' }
run_svm <- function(obs, absence, predictors, verbose = TRUE) {
  if (!require(raster)) {
    stop("run_svm requires raster package, but it could not be loaded")
  }
  if (!require(dplyr)) {
    stop("run_svm requires dplyr package, but it could not be loaded")
  }
  if (!require(dismo)) {
    stop("run_svm requires dismo package, but it could not be loaded")
  }
  if (!require(kernlab)) {
    stop("run_svm requires kernlab package, but it could not be loaded")
  }
  # make sure latitude & longitude are there
  if (!("longitude" %in% colnames(obs))) {
    stop("get_extent requires column named 'longitude' in passed data argument)")
  }
  if (!("latitude" %in% colnames(obs))) {
    stop("get_extent requires column named 'latitude' in passed data argument)")
  }
  
  # Only need geo coordinates, so extract those (in x, y order)
  presence <- obs %>%
    dplyr::select(longitude, latitude)
  
  if (verbose) {
    message("Extracting predictor values based on data. (Step 1 of 4)")
  }
  
  # Use the observed points to pull out relevant predictor values
  predictors_presence <- raster::extract(x = predictors, y = presence)
  predictors_absence <- raster::extract(x = predictors, y = absence)

  # Make a vector of appropriate length with 0/1 values for 
  # (pseudo)absence/presence
  pa_data <- c(rep(x = 1, times = nrow(presence)), 
               rep(x = 0, times = nrow(absence)))
  
  # Create a vector of folds for easier splitting into testing/training
  num_folds <- 5 # for 20/80 split
  fold <- c(rep(x = 1:num_folds, length.out = nrow(presence)),
            rep(x = 1:num_folds, length.out = nrow(absence)))
  
  # Combine our presence / absence and fold vectors with environmental data we 
  # extracted
  full_data <- data.frame(cbind(pa = pa_data,
                                fold = fold,
                                rbind(predictors_presence, predictors_absence)))

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
  sdmtest <- rbind(presence_test, absence_test)

  if(verbose) {
    message("Running support vector machine model. (Step 2 of 4)")
  }  
  # Run an SVM model, specifying model with standard formula syntax
  svm_model <- kernlab::ksvm(pa ~ bio1 + bio5 + bio6 + bio7 + bio8 + bio12 +
                               bio16 + bio17,
                             data = sdmtrain)
  
  if(verbose) {
    message("Model complete. Evaluating SVM model with testing data. (Step 3 of 4)")
  }
  # Evaluate model performance with testing data
  svm_eval <- dismo::evaluate(p = presence_test, 
                              a = absence_test, 
                              model = svm_model)


  # Get the extent of data so we can calculate probabilities for that 
  # geographic extent
  # TODO: Could use extent of absence(background) data, but would need to be 
  # sure the data included longitude and latitude columns (not just x, y) in 
  # order to work with get_extent
  obs_extent <- get_extent(data = obs)
  
  # Do the predictions so we can map things (takes a few seconds with worldclim)
  # Note predicted probabilities include values < 0 and > 1. Not expected when 
  # applying a linear model to what is a classification problem
  if(verbose) {
    message("Predicting occurrence probabilities from SVM model. (Step 4 of 4)")
  }
  probs <- predict(predictors, 
                   svm_model, 
                   ext = obs_extent)
  
  # Calculate threshold so we can include a P/A map
  pres_threshold <- dismo::threshold(x = svm_eval, 
                                     stat = "spec_sens")

  # Bind everything together and return as list  
  results <- list(model = svm_model,
                  evaluation = svm_eval,
                  probs = probs,
                  thresh = pres_threshold)
  
  return(results)
}