#' Predict values from species distribution model
#' 
#' @param nice_name character name of species to use for predictions; should be
#' all lowercase with spaces replaced by underscores (i.e. 
#' "papilio_multicaudata" for Papilio multicaudata)
#' @param predictors bioclimatic data to use for predictions
#' @param model saved SDM model 
#' @param yr character indicating year for which predictions are being made 
#' (eg, current, 2041, 2071)

predict_sdm <- function(nice_name, 
                        predictors, 
                        model,
                        yr) {
  if (!require(raster)) {
    stop("predict_sdm requires raster package, but it could not be loaded")
  }
  if (!require(dplyr)) {
    stop("predict_sdm requires dplyr package, but it could not be loaded")
  }
  if (!require(dismo)) {
    stop("predict_sdm requires dismo package, but it could not be loaded")
  }
  if (!require(fields)) {
    stop("predict_sdm requires fields package, but it could not be loaded")
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
  
  # Want the prediction extent to extend a reasonable amount beyond GBIF obs
  # Calculate extent based on P. cresphontes movements (Wilson et al. 2021)
  # Can move 180 km per decade, so ~ 360 km by 2041 or 900 km by 2071
  # 360 km latitude = 3.24 deg; 900 km latitude = 8.09 deg 
  # km longitude depends on latitude (lower lat = more km, fewer deg)
  # Use the fields::rdist.earth to convert 1-degree distance longitude into km
  # at minimum latitude 

  lon1 <- 100
  lon2 <- 101
  lat <- p_extent@ymin
  km_minlat <- rdist.earth(matrix(c(lon1, lat), ncol = 2),
                           matrix(c(lon2, lat), ncol = 2),
                           miles = FALSE,
                           R = 6371)
  
  if (yr %in% c("current", "2041")) {
    lat_toadd <- 3.24
    lon_toadd <- 360 * 1/km_minlat
  } else {
    lat_toadd <- 8.09
    lon_toadd <- 900 * 1/km_minlat
  }
  pred_extent <- p_extent + c(-lon_toadd, lon_toadd, -lat_toadd, lat_toadd)
  
  # Calculate probabilities based on predictors and model
  if (is.null(p_extent)) {
    # For some reason, obs not found, so cannot restrict extent
    preds <- predict(predictors, 
                     model)
  } else {
    preds <- predict(predictors, 
                     model, 
                     ext = pred_extent)
  }
  # Note: predict() above is using the raster package
  
  # Send back this raster with the predicted values
  return(preds)
  
}