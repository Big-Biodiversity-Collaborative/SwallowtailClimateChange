#' Predict presence / absence based on species distribution model
#' 
#' @param nice_name character name of species to use for predictions; should be
#' all lowercase with spaces replaced by underscores (i.e. 
#' "papilio_multicaudata" for Papilio multicaudata)
#' @param predictors bioclimatic data to use for predictions
#' @param model character indicating model (e.g. "glm" or "svm") to use for 
#' predictions
#' @param pred_extf numeric indicating how far beyond current observations to 
#' make predictions. e.g. a value of 1.1 will extend the geographic extent by 
#' 5% on all four sides of the extent of current observations
predict_pa <- function(nice_name, predictors, model = c("glm", "svm"), 
                       pred_extf = 1.5) {
  if (!require(raster)) {
    stop("predict_pa requires raster package, but it could not be loaded")
  }
  if (!require(dplyr)) {
    stop("predict_pa requires dplyr package, but it could not be loaded")
  }
  if (!require(dismo)) {
    stop("predict_pa requires dismo package, but it could not be loaded")
  }
  source(file = "functions/get_extent.R")
  
  model <- match.arg(arg = model)

  # Get the current observations, to dictate the geographic extent of the 
  # predicted presence / absence
  obs_file <- paste0("data/gbif/",
                     nice_name,
                     "-gbif.csv")
  if (!file.exists(obs_file)) {
    warning(paste0("Could not find observations for ", nice_name, 
                   " predictions not geographically restricted"))
  } else {
    obs <- read.csv(file = obs_file)
    obs_extent <- get_extent(data = obs)
    rm(obs)
  }
  
  # Load the model, which as the model and the threshold for determining 
  # presence / absence
  model_file <- paste0("output/models/", nice_name,
                       "-model-", model,
                       "-current.rds")
  if (!file.exists(model_file)) {
    warning(paste0("No model file found for ", nice_name, "; ", model, 
                   " predictions not made"))
    return(NULL)
  }
  
  sdm_model <- readRDS(file = model_file)
  
  # Calculate probabilities based on predictors and model
  if (is.null(obs_extent)) {
    # For some reason, obs not found, so cannot restrict extent
    probs <- predict(predictors, 
                     sdm_model$model)
  } else {
    probs <- predict(predictors, 
                     sdm_model$model, 
                     ext = (obs_extent * pred_extf))
  }
  
  # Make a raster of presence / absence values
  pa_raster <- probs > sdm_model$thresh

  # Send back this raster
  return(pa_raster)
}