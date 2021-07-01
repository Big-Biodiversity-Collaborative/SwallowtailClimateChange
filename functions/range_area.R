#' Area of range in square kilometers
#' 
#' @param r A Raster* object
#' 
#' @return numeric value in square kilometers
range_area <- function(r) {
  if (!require(raster)) {
    stop("range_area requires raster package, but it could not be loaded")
  }
  cell_areas <- tapply(X = raster::area(r),
                       INDEX = r[],
                       FUN = sum)
  
  # Count how many pixels are "present" (== TRUE)
  present_area <- cell_areas["TRUE"]
  return(present_area)
}