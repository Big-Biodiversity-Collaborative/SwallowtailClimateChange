#' Predict values from species distribution model
#' 
#' @param nice_name character name of species to use for predictions; should be
#' all lowercase with spaces replaced by underscores (i.e. 
#' "papilio_multicaudata" for Papilio multicaudata)
#' @param predictors bioclimatic data to use for predictions
#' @param model saved SDM model 
#' @param pred_extf numeric indicating how far beyond current observations to 
#' make predictions. e.g. a value of 1.1 will extend the geographic extent by 
#' 5% on all four sides of the extent of current observations
predict_sdm <- function(nice_name, 
                        predictors, 
                        model,
                        pred_extf = 1.5) {
  if (!require(raster)) {
    stop("predict_sdm requires raster package, but it could not be loaded")
  }
  if (!require(dplyr)) {
    stop("predict_sdm requires dplyr package, but it could not be loaded")
  }
  if (!require(dismo)) {
    stop("predict_sdm requires dismo package, but it could not be loaded")
  }
  source(file = "functions/get_extent.R")

  # Get the current observations, to dictate the geographic extent of the 
  # predicted presence / absence
  pa_file <- paste0("development/data/presence-absence/",
                    nice_name,
                    "-pa.csv")
  if (!file.exists(pa_file)) {
    unzip(zipfile = "data/pa-datasets.zip")
  }
  if (!file.exists(pa_file)) {
    warning(paste0("Could not find observations for ", nice_name, 
                   " predictions not geographically restricted"))
  } else {
    pa <- read.csv(file = pa_file)
    pa <- pa %>% 
      filter(pa == 1) %>%
      rename(longitude = x, latitude = y)
    p_extent <- get_extent(data = pa)
    rm(pa)
  }

  # Calculate probabilities based on predictors and model
  if (is.null(p_extent)) {
    # For some reason, obs not found, so cannot restrict extent
    preds <- predict(predictors, 
                     model)
  } else {
    preds <- predict(predictors, 
                     model, 
                     ext = p_extent * pred_extf)
  }
    # Note: predict() above is using the raster package
  
  # Send back this raster with the predicted values
  return(preds)

}