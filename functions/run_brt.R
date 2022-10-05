#' Run boosted regression tree (BRT) species distribution model
#' 
#' @param full_data dataframe with presence-absence data (1/0), fold (for 
#' separating testing and training data), and 19 columns with climate data
#' @param verbose logical indicating whether or not to print processing messages
#' 
#' @details Uses \code{dismo::gbm.step()} for a boosted regression tree model.
#' This function assesses the optimal number of boosting trees (that
#' minimizes deviance) using k-fold cross validation. Then it fits a model with 
#' this number of trees and returns it as a gbm model (along with additional 
#' information from the cross-validation selection process).
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
#'   \item{trees}{number of trees used in final model}
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

  # Creating values to downweight background points (so total (summed) 
  # weight of background pts equal to the total weight of presence pts)
  prNum <- sum(sdmtrain$pa == 1)
  bgNum <- sum(sdmtrain$pa == 0)
  wt <- ifelse(sdmtrain$pa == 1, 1, prNum / bgNum)
  
  # Set model parameters
    # Tree complexity (number of nodes in a tree)
    tc <- ifelse(prNum < 50, 1, 5)
    # Learning rate (weights applied to individual trees)
    poss_lr_values <- c(0.001, 0.01, 0.05, 0.10)
    lr_index <- 3
    lr <- poss_lr_values[lr_index]
    # Number of initial trees (and number to add at each step)
    n_trees <- 50
    # Maximum number of trees to fit before stopping
    max_trees <- 10000
    # Number of folds for cross-validation (to find optimal number of trees)
    n_folds <- 5
 
  if (verbose) {
    message("Running ", method_name, ".")
  }  
  
  # Run the model
  # Note: using try() function so if model fails to fit the loop will continue
  model_fit <- NULL  
  opt_trees <- 0 
  no_model <-  FALSE
  while (is.null(model_fit) | opt_trees < 1000 | opt_trees == max_trees)  {
    # Run gbm.step
    try(
      model_fit <- gbm.step(data = sdmtrain,
                            gbm.x = 3:ncol(sdmtrain), # Columns with predictor data
                            gbm.y = 1,                # Columns with pa data
                            family = "bernoulli",
                            tree.complexity = tc,
                            learning.rate = lr,
                            n.trees = n_trees,
                            max.trees = max_trees,
                            n.folds = n_folds,
                            verbose = TRUE)
    )
    
    # Extract the optimal number of trees
    opt_trees <- ifelse(is.null(model_fit), 0, model_fit$gbm.call$best.trees)
    
    # If the algorithm is unable to find an optimum number with the fastest 
    # learning rate (0.10) within 10,000 trees, then exit. 
    if (lr_index == 4 & opt_trees == max_trees) {
      no_model <- TRUE
      message("Unable to find optimal number of trees with learning rate = 0.1 and max trees = 10,000.")
      break
    }
    
    # If the optimal number of trees is < 1000 with a learning rate of 0.001, 
    # save this model and exit.
    if (lr_index == 1 & opt_trees < 1000) {
      message("Optimal number of trees < 1000 with learning rate = 0.001. ",
              "Saving model with < 1000 trees.")
      break      
    }
    
    # Adjust the learning rate if the optimal number of trees is < 1000 or 
    # optimal number couldn't be identified (ie, equal to max_trees) 
    lr_index <- ifelse(opt_trees < 1000, 
                       max(lr_index - 1, 1), 
                       ifelse(opt_trees == max_trees, 
                              min(lr_index + 1, 4),
                              lr_index))
    lr <- poss_lr_values[lr_index]
  }
    
  if (no_model | is.null(model_fit)) {
    message("Unable to find optimal number of trees. Model not saved.")
    results <- NULL
  } else {
    if (verbose) {
      message("Model complete. Evaluating ", method_name, 
              " with testing data.")
    }
    
    # Evaluate model performance with testing data
    # Need to specify the optimal number of trees and the type of predictions 
    # (so they're on a [0, 1] scale)
    model_eval <- dismo::evaluate(p = presence_test,
                                  a = absence_test,
                                  model = model_fit,
                                  n.trees = opt_trees,
                                  type = "response") 
    
    # Calculate threshold so we can make a P/A map later
    pres_threshold <- dismo::threshold(x = model_eval, 
                                       stat = "spec_sens")
    
    ### TODO: Everything seems to be working BUT the model objects are huge 
    ### (many MB). Need to figure out whether there are ways to reduce the size.
    
    # Bind everything together and return as list  
    # For BRT models, including the number of trees
    results <- list(model = model_fit,
                    evaluation = model_eval,
                    thresh = pres_threshold,
                    trees = opt_trees)
  }

  return(results)
}
