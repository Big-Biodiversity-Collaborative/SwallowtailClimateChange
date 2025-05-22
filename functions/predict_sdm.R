#' Predict values from species distribution model
#' 
#' @param nice_name character name of species to use for predictions; should be
#' all lowercase with spaces replaced by underscores (i.e. 
#' "papilio_multicaudata" for Papilio multicaudata)
#' @param sdm_method character indicating the type of SDM ("brt", "gam",
#' "lasso", "maxent", "rf")
#' @param model species distribution model object
#' @param stand_obj an object of class save_means_sds (created with the 
#' save_means_sds function) that contains means and SDs for predictor variables 
#' in a training dataset (Optional, except for lasso regression and gam models)
#' @param quad a logical indicating whether or not quadratics were included 
#' in model (Optional, except for lasso regression and gam models)
#' @param predictors SpatRaster with global climate model data (a layer for each 
#' climate variable in the model). Raster should already be cropped/masked to 
#' area of interest
#' 
#' @return SpatRaster with predicted suitability values based on given 
#' species distribution model and global climate model data
#' 
#' #' \dontrun{
#' # Test predictions of current climate for P. rumiko for RF and MaxEnt models
#' nice_name <- "papilio_rumiko"
#' predictors <- terra::rast(list.files(path = "data/wc2-1",
#'                                      pattern = ".tif$",
#'                                      full.names = TRUE))
#' # Get and subset climate variables                                    
#' all_climate_vars <- read.csv("data/climate-variables.csv")
#' climate_vars <- all_climate_vars$variable[all_climate_vars$include == TRUE]
#' predictors <- terra::subset(predictors, climate_vars)
#' # Get minimum convex polygon for this species and crop to extent of 
#' # predictors
#' mcp <- terra::vect(paste0("data/gbif/shapefiles/",
#'                           nice_name, 
#'                           "-buffered-mcp.shp"))
#' mcp <- terra::crop(mcp, terra::ext(predictors))  
#' 
#' # Crop and mask *predictors* to mcp
#' pred_mask <- terra::crop(predictors, mcp, snap = "out")
#' pred_mask <- terra::mask(pred_mask, mcp)
#' # Make predictions
#' # Random forest model
#' model_list <- readRDS(file = paste0("output/SDMs/", nice_name, "-rf.rds"))
#' preds_rf <- predict_sdm(nice_name = nice_name,
#'                         sdm_method = "rf",
#'                         model = model_list$model,
#'                         stand_obj = model_list$stand_obj,
#'                         quad = model_list$quad,
#'                         predictors = pred_mask)
#' # MaxEnt model
#' model_list <- readRDS(file = paste0("output/SDMs/", nice_name, "-maxent.rds"))
#' preds_maxent <- predict_sdm(nice_name = nice_name,
#'                             sdm_method = "maxent",
#'                             model = model_list$model,
#'                             stand_obj = model_list$stand_obj,
#'                             quad = model_list$quad,
#'                             predictors = pred_mask)
#' }
predict_sdm <- function(nice_name, 
                        sdm_method = c("brt", "gam", "lasso", "maxent", "rf"),
                        model, 
                        stand_obj = NULL,
                        quad = NULL,
                        predictors) {
  
  sdm_method <- match.arg(arg = sdm_method)

  if (!require(terra)) {
    stop("predict_sdm requires terra package, but it could not be loaded")
  }
  if (!require(dplyr)) {
    stop("predict_sdm requires dplyr package, but it could not be loaded")
  }
  if (sdm_method == "brt") {
    if (!require(gbm)) {
      stop("predict_sdm requires gbm package, but it could not be loaded")    
    }
  }
  if (sdm_method == "gam") {
    if (!require(mgcv)) {
      stop("predict_sdm requires mgcv package, but it could not be loaded")    
    }
  }
  if (sdm_method == "lasso") {
    if (!require(glmnet)) {
      stop("predict_sdm requires glmnet package, but it could not be loaded")       
    }
    if (!require(Matrix)) {
      stop("predict_sdm requires Matrix package, but it could not be loaded")
    }
  }
  if (sdm_method == "maxent") {
    if (!require(ENMeval)) {
      stop("predict_sdm requires ENMeval package, but it could not be loaded")       
    }
  }
  if (sdm_method == "rf") {
    if (!require(randomForest)) {
      stop("predict_sdm requires randomForest package, but it could not be loaded")       
    }
  }
  if (sdm_method == "gam" | sdm_method == "lasso") {
    if(is.null(stand_obj) | is.null(quad)) {
      stop("predict_sdm requires stand_obj and quad arguments")
    }
  }

  # If using a Maxent model (with maxnet algorithm), need enm.maxnet@predict
  # See ?ENMevaluate or https://github.com/jamiemkass/ENMeval/issues/112
  if (sdm_method == "maxent") {
    preds <- enm.maxnet@predict(model, predictors,
                                list(pred.type = "cloglog", doClamp = FALSE))
    # The resultant preds object has no CRS, so grab the one attached to the 
    # predictors
    terra::crs(preds) <- terra::crs(predictors)
    
    # We also want to ensure the extent of the predictions match the extent of 
    # the predictors, so that happens here, too.
    preds <- terra::crop(x = preds, y = predictors)
    preds <- terra::extend(x = preds, y = predictors)
    terra::ext(preds) <- terra::ext(predictors)

  }

  # Random forest prediction we want second index (i.e. the probability a cell 
  # is scored as a 1; first index is probability a cell is scored as a 0)
  if (sdm_method == "rf") {
    preds <- terra::predict(object = predictors,
                            model = model,
                            type = "prob",
                            index = 2)
  }

  # If using a GAM or LASSO model, need to standardize predictors using means 
  # and SDs from training dataset
  if (sdm_method %in% c("lasso", "gam")) {
    predictors <- prep_predictors(object = stand_obj, 
                                  newdata = predictors, 
                                  quad = quad)
    invisible(gc())
  }
  
  # Create list of arguments for predict function for remaining model types
  params <- list(object = predictors, 
                 model = model,
                 type = "response", 
                 na.rm = TRUE)

  # If using a BRT model, specify the number of trees and add to list of args
  if (sdm_method == "brt") {
    n_trees <- model$n.trees
    params <- c(params, n.trees = n_trees)
  }  
  
  # Make predictions for BRT, GAM, or LASSO
  if (sdm_method %in% c("brt", "gam")) {
    preds <- do.call(predict, params)
  } 
  if (sdm_method == "lasso") {
    # For lasso regression models from cv.glmnet: 
    # predict.cv.glmnet doesn't allow for rasters as input, so we need to
    # convert predictors to a sparse matrix. Referenced code from Roozbeh Valavi: 
    # https://github.com/rvalavi/myspatial/blob/master/R/prediction.R
    pred_points <- terra::as.points(predictors, 
                                    values=TRUE, 
                                    na.rm=TRUE, 
                                    na.all=FALSE)
    data_sparse <- sparse.model.matrix(~. -1, terra::values(pred_points))  
    pred_vect <- as.numeric(predict(object = model, 
                                    newx = data_sparse, 
                                    type = "response", 
                                    s = model$lambda.1se))
    pred_points$pred <- pred_vect
    rm(data_sparse, pred_vect)
    preds <- terra::rasterize(pred_points, predictors, field = "pred")
    rm(pred_points, predictors)
  }

  invisible(gc())
  
  # Send back this raster with the predicted values
  return(preds)
}
