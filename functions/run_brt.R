#' Run boosted regression tree (BRT) species distribution model
#' 
#' @param full_data dataframe with presence-absence data (1/0), columns with 
#' climate data
#' @param step logical indicating whether to use \code{dismo::gbm.step()} or 
#' \code{dismo::gbm.fixed()}.
#' @param bag.fraction random sample size for each new tree (default = 0.75)
#' @param complexity number of nodes in tree (default = 5)
#' @param ntrees number of trees (only needed if step = FALSE)
#' @param learning.rate learning rate (weights applied to individual trees;
#' only needed if step = FALSE)
#' @param verbose logical indicating whether or not to print processing messages
#' 
#' @details If step = TRUE, uses \code{dismo::gbm.step()} for a boosted 
#' regression tree model. This function assesses the optimal number of boosting 
#' trees (that minimizes deviance) using k-fold cross validation. Then it fits a 
#' model with this number of trees. If step = FALSE, uses 
#' \code{dismo::gbm.fixed()} to run a boosted regression tree model with a 
#' pre-specified number of trees.
#' 
#' @return An object of class gbm, with parameter estimates for a Boosted 
#' Regression Tree (BRT) SDM. Output of \code{dismo::gbm.step()} or 
#' \code{dismo::gbm::fixed()}

run_brt <- function(full_data, step = TRUE, bag.fraction = 0.75, 
                    complexity = 5, ntrees = NA, learning.rate = NA,
                    verbose = TRUE) {
  # Extract the name of this function for reporting
  function_name <- as.character(match.call())[1]
  
  # Libraries required for this function to work
  method_name <- "boosted regression tree"
  dependencies <- "gbm"
  if (!all(unlist(lapply(X = dependencies, FUN = require, character.only = TRUE)))) {
    stop("At least one package required by ", function_name, 
         " could not be loaded: ", paste(dependencies, collapse = ", "),
         " are required.")
  }
  
  # Make sure presence-absence data are there
  if (!("pa" %in% colnames(full_data))) {
    stop(function_name, " requires column named 'pa' in full_data")
  }
  # Make sure climate data are there
  if (sum(grepl("bio", colnames(full_data))) < 2) {
    stop(function_name, " requires multiple climate variables in full_data")
  }
  # Make sure parameter settings are appropriate
  if (bag.fraction <= 0 | bag.fraction > 1) {
    stop(function_name, " requires a bag.fraction between 0 and 1")
  }
  if (step == FALSE & complexity %%1 != 0) {
    stop(function_name, " requires complexity to be an integer")
  }		
  if (step == FALSE & ntrees %% 1 != 0) {
    stop(function_name, " requires ntrees to be an integer")
  }	
  if (step == FALSE & (learning.rate <= 0 | learning.rate > 1)) {
    stop(function_name, " requires a learning.rate between 0 and 1")
  }	
  
  # Identify columns with presence-absence and climate data
  pa_column <- which(colnames(full_data) == "pa")
  clim_columns <- which(grepl("bio", colnames(full_data)))
  
  if (step) {
    # Learning rate
    poss_lr_values <- c(0.001, 0.01, 0.05, 0.10)
    lr_index <- 2
    lr <- poss_lr_values[lr_index] 
    # Number of initial trees
    n_trees <- 100
    # Number of trees to add at each step
    step_size <- 100
    # Maximum number of trees to fit before stopping
    max_trees <- 10000
    # Number of folds for internal cross validation
    n_folds <- 5
    
    # Note: using try() function so if model fails to fit the loop will continue
    brt_fit <- NULL  
    opt_trees <- 0 
    no_model <-  FALSE
    
    while (is.null(brt_fit) | opt_trees < 1000 | opt_trees == max_trees)  {
      try(
        brt_fit <- dismo::gbm.step(data = full_data,
                                   gbm.x = clim_columns,
                                   gbm.y = pa_column,
                                   family = "bernoulli",
                                   tree.complexity = complexity,
                                   learning.rate = lr,
                                   n.trees = n_trees,
                                   step.size = step_size,
                                   max.trees = max_trees,
                                   n.folds = n_folds,
                                   bag.fraction = bag.fraction,
                                   verbose = verbose, 
                                   silent = !verbose,
                                   plot.main = FALSE,  
                                   plot.folds = FALSE)
      )
      
      # Extract the optimal number of trees
      opt_trees <- ifelse(is.null(brt_fit), 0, brt_fit$gbm.call$best.trees)
      
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
  } else {
    try(
      brt_fit <- dismo::gbm.fixed(data = full_data,
                                  gbm.x = clim_columns,
                                  gbm.y = pa_column, 
                                  family = "bernoulli",
                                  tree.complexity = complexity,
                                  learning.rate = learning.rate,
                                  n.trees = ntrees,
                                  bag.fraction = bag.fraction,
                                  verbose = verbose)
    )
  }
  
  if (is.null(brt_fit)) {
    message("Unable to find optimal number of trees. Model not saved.")
    results <- NULL
  } else {
    results <- brt_fit
  }
  
  return(results)
}
