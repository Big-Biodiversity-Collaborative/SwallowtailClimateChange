#' Returns a raster::extent object based on coordinates in data
#' 
#' @param data a data frame or matrix with columns 'longitude' and 'latitude'
#' 
#' @return a \code{raster::extent} object based on the minimum and maximum 
#' values in latitude and longitude coordinates of \code{data}
get_extent <- function(data) {
  if (!require(raster)) {
    stop("get_extent requires raster package, but it could not be loaded")
  }
  # make sure latitude & longitude are there
  if (!("longitude" %in% colnames(data))) {
    stop("get_extent requires column named 'longitude' in passed data argument)")
  }
  if (!("latitude" %in% colnames(data))) {
    stop("get_extent requires column named 'latitude' in passed data argument)")
  }
  
  obs_extent <- raster::extent(x = c(floor(min(data$longitude)),
                                     ceiling(max(data$longitude)),
                                     floor(min(data$latitude)),
                                     ceiling(max(data$latitude))))
  return(obs_extent)
}