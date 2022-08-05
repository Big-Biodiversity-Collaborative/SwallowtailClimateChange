#' Predict values from species distribution model
#' 
#' @param nice_name character name of species to use for predictions; should be
#' all lowercase with spaces replaced by underscores (i.e. 
#' "papilio_multicaudata" for Papilio multicaudata)
#' @param model species distribution model object; generally output from model
#' function such as maxent or glm
#' @param yr character indicating year for which predictions are being made 
#' ("current", "2041", "2071")
#' @param ssp character indicating shared socioeconomic pathway of global 
#' climate model; ignored if \code{yr == "current"}
#' 
#' @return raster of predicted probabilities of occurrence based on given 
#' species distribution model and global climate model data
predict_sdm <- function(nice_name, model, yr = c("current", "2041", "2071"), 
                       ssp = c(NA, "245", "370")) {
  if (!require(raster)) {
    stop("predict_pa requires raster package, but it could not be loaded")
  }
  if (!require(dplyr)) {
    stop("predict_pa requires dplyr package, but it could not be loaded")
  }
  if (!require(sf)) {
    stop("predict_pa requires sf package, but it could not be loaded")
  }
  if (!require(dismo)) {
    stop("predict_pa requires dismo package, but it could not be loaded")
  }

  yr <- match.arg(arg = yr)
  ssp <- match.arg(arg = ssp)

  # Get MCP shapefile for geographic extent
  shapefile_name <- paste0("data/gbif/shapefiles/",
                           nice_name, 
                           "-buffered-mcp.shp")
  # TODO: Could do better job for when this file does not exist?
  if (!file.exists(shapefile_name)) {
    message("Shapefile ", shapefile_name, " does not exist on disk.")
    return(NULL)
  }
  buffered_mcp <- sf::st_read(shapefile_name, quiet = TRUE)

  # If necessary, adjust buffered MCP as appropriate - allowing larger buffers
  # for more distant time periods
  if (yr %in% c("2041", "2071")) {
    # Project buffered mcp to NA Albers Equal Area Conic 
    buffered_mcp_proj <- sf::st_transform(buffered_mcp, crs = "ESRI:102008")
    dist_mult <- dplyr::if_else(yr == "2041",
                                true = 350,  # 350 for 2041
                                false = 900) # 900 for 2071
    
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

  # Make predictions with the remaining predictor data and model  
  preds <- predict(pred_mask, 
                   model)
  
  # Send back this raster with the predicted values
  return(preds)
}