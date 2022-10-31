#' Predict values from species distribution model
#' 
#' @param nice_name character name of species to use for predictions; should be
#' all lowercase with spaces replaced by underscores (i.e. 
#' "papilio_multicaudata" for Papilio multicaudata)
#' @param model species distribution model object; generally output from model
#' function such as maxent or glm
#' @param sdm_method character indicating the type of SDM ("brt", "glm", "gam", 
#' "maxent-notune", "maxent-tune", "rf", "svm")
#' @param yr character indicating year for which predictions are being made 
#' ("current", "2041", "2071")
#' @param ssp character indicating shared socioeconomic pathway of global 
#' climate model; ignored if \code{yr == "current"}
#' @param stand_obj an object of class save_means_sds (created with the 
#' save_means_sds function) that contains means and SDs for predictor variables 
#' in a training dataset (Optional, except for lasso regression models)
#' @param quad a logical indicating whether or not quadratics were included 
#' in model (Optional, except for lasso regression models)
#' 
#' @return raster of predicted probabilities of occurrence based on given 
#' species distribution model and global climate model data

predict_sdm <- function(nice_name, 
                        model, 
                        sdm_method = c("brt", "glm", "gam", "lasso",
                                       "maxent-notune", "maxent-tune", "rf",
                                       "svm"),
                        yr = c("current", "2041", "2071"), 
                        ssp = c(NA, "245", "370"),
                        stand_obj = NULL,
                        quad = NULL) {
  
  sdm_method <- match.arg(arg = sdm_method)
  yr <- match.arg(arg = yr)
  ssp <- match.arg(arg = ssp)
  
  if (!require(raster)) {
    stop("predict_sdm requires raster package, but it could not be loaded")
  }
  if (!require(dplyr)) {
    stop("predict_sdm requires dplyr package, but it could not be loaded")
  }
  if (!require(sf)) {
    stop("predict_sdm requires sf package, but it could not be loaded")
  }
  if (!require(dismo)) {
    stop("predict_sdm requires dismo package, but it could not be loaded")
  }
  if (sdm_method == "brt") {
    if (!require(gbm)) {
      stop("predict_sdm requires gbm package, but it could not be loaded")    
    }
  }
  if (sdm_method == "lasso") {
    if (!require(glmnet)) {
      stop("predict_sdm requires glmnet package, but it could not be loaded")       
    }
    if(is.null(stand_obj) | is.null(quad)) {
      stop("predict_sdm requires stand_obj and quad arguments")
    }
  }
  
  # Get MCP shapefile for geographic extent
  shapefile_name <- paste0("data/gbif/shapefiles/",
                           nice_name, 
                           "-buffered-mcp.shp")
  # If species' shapefile isn't in shapefiles folder, unzip gbif-shapefiles
  if (!file.exists(shapefile_name)) {
    unzip(zipfile = "data/gbif-shapefiles.zip")
  }
  buffered_mcp <- sf::st_read(shapefile_name, quiet = TRUE)
  
  # If necessary, adjust buffered MCP as appropriate - allowing larger buffers
  # for more distant time periods
  if (yr %in% c("2041", "2071")) {
    # Project buffered MCP to NA Albers Equal Area Conic 
    buffered_mcp_proj <- sf::st_transform(buffered_mcp, crs = "ESRI:102008")
    dist_mult <- dplyr::if_else(yr == "2041",
                                true = 350,  # 350 km for 2041
                                false = 900) # 900 km for 2071
    
    # Add to the buffer, based on appropriate distance multiplier
    buffered_mcp <- sf::st_buffer(buffered_mcp_proj,
                                  dist = dist_mult * 1000)
    
    # Transform back to lat/long
    buffered_mcp <- sf::st_transform(buffered_mcp, 4326) 
  }
  
  # Grab predictors
  gcm_directory <- dplyr::if_else(yr == "current",
                                  true = "data/wc2-1",
                                  false = paste0("data/ensemble/ssp", ssp, "/", yr))
  
  predictors <- raster::stack(list.files(path = gcm_directory,
                                         pattern = ".tif$",
                                         full.names = TRUE))
  
  # Crop and mask as appropriate (takes a few moments)
  pred_mask <- raster::crop(predictors, buffered_mcp)
  pred_mask <- raster::mask(pred_mask, buffered_mcp)
  
  # If using a lasso model, need to standardize predictors using means and SDs 
  # based on training dataset
  if (sdm_method == "lasso") {
    pred_mask <- prep_predictors(stand_obj, pred_mask, quad = quad)
  }
  
  # Create list of arguments for predict function
  params <- list(object = pred_mask, 
                 model = model,
                 type = "response")
  
  # If using a BRT model, specify the number of trees and add to list of args
  if (sdm_method == "brt") {
    n_trees <- model$n.trees
    params <- c(params, n.trees = n_trees)
  }  
  
  # Make predictions
  if (sdm_method != "lasso") {
    preds <- do.call(predict, params)
  } else {
    # For lasso regression models from cv.glmnet: 
    # predict.cv.glmnet doesn't allow for rasters as input, so we need to
    # convert pred_mask to a sparse matrix. Used code from Roozbeh Valavi: 
    # https://github.com/rvalavi/myspatial/blob/master/R/prediction.R
    pred_points <- raster::rasterToPoints(pred_mask, spatial = TRUE)
    data_sparse <- sparse.model.matrix(~. -1, pred_points@data)
    pred_points@data$pred = as.numeric(predict(object = model, 
                                               newx = data_sparse, 
                                               type = "response", 
                                               s = model$lambda.1se))
    preds <- raster::rasterize(pred_points, pred_mask, field = "pred")
  }
  
  # Send back this raster with the predicted values
  return(preds)
}
