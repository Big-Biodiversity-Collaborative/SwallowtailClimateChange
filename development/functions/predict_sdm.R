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

  shapefile_name <- paste0("development/output/shapefiles/",
                           nice_name, 
                           "-buffered-mcp.shp")
  buffered_mcp <- st_read(shapefile_name)
  
  # Crop and mask predictors
  if (yr == "current") {
    
    pred_mask <- raster::crop(predictors, buffered_mcp)
    pred_mask <- raster::mask(pred_mask, buffered_mcp)
  
  } else {
    
    # Project buffered mcp to NA Albers Equal Area Conic 
    buffered_mcp_proj <- st_transform(buffered_mcp, crs = "ESRI:102008")
    
    if (yr == "2041") {

      # Add 350 km to the buffer
      buffered_mcp_2041 <- st_buffer(buffered_mcp_proj,
                                     dist = 350 * 1000)
      # Transform back to lat/long
      buffered_mcp_2041 <- st_transform(buffered_mcp_2041, 4326) 
      
      pred_mask <- raster::crop(predictors, buffered_mcp_2041)
      pred_mask <- raster::mask(pred_mask, buffered_mcp_2041)
       
    } else {
      
      # Add 900 km to the buffer
      buffered_mcp_2071 <- st_buffer(buffered_mcp_proj,
                                     dist = 900 * 1000)
      # Transform back to lat/long
      buffered_mcp_2071 <- st_transform(buffered_mcp_2071, 4326) 
      
      pred_mask <- raster::crop(predictors, buffered_mcp_2071)
      pred_mask <- raster::mask(pred_mask, buffered_mcp_2071)

    }
  }
    
  preds <- predict(pred_mask, 
                   model)  
  
  # Send back this raster with the predicted values
  return(preds)
  
}