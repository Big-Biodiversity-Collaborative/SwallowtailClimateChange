#' Run random forest (RF) species distribution model
#' 
#' @param full_data dataframe with presence-absence data (1/0), columns with 
#' climate data
#' @param importance logical indicating whether or not to assess importance of 
#' predictors
#' @param ntree number of trees to grow (default = 1000)
#' 
#' @details Uses \code{randomForest::randomForest()} for classification-type 
#' random forest model
#' 
#' @return An object of class randomForest, with parameter estimates for a 
#' random forest SDM. Output of \code{randomForest::randomForest()}.

run_rf <- function(full_data, importance = TRUE, ntree = 1000) {
  # Extract the name of this function for reporting
  function_name <- as.character(match.call())[1]
  
  if (!require(randomForest)) {
    stop(function_name, " requires randomForest package, but it could not be loaded")
  }
  # Make sure presence-absence data are there
  if (!("pa" %in% colnames(full_data))) {
    stop(function_name, " requires column named 'pa' in full_data")
  }
  # Make sure multiple climate variables are there
  if (sum(grepl("bio", colnames(full_data))) < 2) {
    stop(function_name, " requires multiple climate variables in full_data")
  }
  if(is.null(ntree)) {
    stop(function_name, " requires the number of trees to be specified")
  } 
  
  # Identify climate variables in model
  climate_vars <- colnames(full_data)[grepl("bio", colnames(full_data))]
  
  # Create model formula
  model_formula <- paste(climate_vars, collapse = " + ")
  model_formula <- as.formula(paste0("pa ~ ", model_formula))
  
  # Generate equal sample sizes for presence, pseudo-absence data. Note that 
  # if we have more presences than absences in a fold (happens rarely), need 
  # to then sample the presences.
  prNum <- as.numeric(table(full_data$pa)["1"])
  bgNum <- as.numeric(table(full_data$pa)["0"])
  if (prNum <= bgNum) {
    smpsize <- c("0" = prNum, "1" = prNum)
  } else {
    smpsize <- c("0" = bgNum, "1" = bgNum)
  } 
  
  # Convert pa column to a factor
  full_data$pa <- as.factor(full_data$pa)
  
  rf_fit <- randomForest(formula = model_formula,
                         data = full_data,
                         ntree = ntree,
                         sampsize = smpsize,
                         importance = importance,
                         replace = TRUE)
  
  results <- rf_fit
  
  return(results)
}
