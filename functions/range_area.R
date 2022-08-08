#' Area of range in square kilometers
#' 
#' @param overlap A Raster* object with overlaps of insect and host
#' 
#' @return numeric vector of areas, in square kilometers, of (1) area occupied 
#' by insect in the absence of host plants, (2) area occupied by insect and at 
#' least one host plant, (3) total area occupied by insect (sum of 1 and 2).
range_area <- function(overlap) {
  if (!require(raster)) {
    stop("range_area requires raster package, but it could not be loaded")
  }
  # Do calculations for overlaps, using raster::area, we get (approximate) km2 
  # of each cell value
  cell_areas <- tapply(X = raster::area(overlap),
                       INDEX = overlap[],
                       FUN = sum)
  
  # Pull out area of those cells for insect only (== 1)
  insect_only <- cell_areas["1"]
  # In case where there are NO pixels of insect only, need to set this to 0
  if (length(insect_only) == 0) {
    insect_only <- 0
  }
  
  # Pull out area of those cells for plant AND insect (== 3)
  insect_plant <- cell_areas["3"]
  # If there are no pixels with both, set to 0
  if (length(insect_plant) == 0) {
    insect_plant <- 0
  }
  
  # Removing names for easier downstream processing
  names(insect_only) <- NULL
  names(insect_plant) <- NULL
  
  total_insect <- insect_only + insect_plant

  # Package everything up in a vector and name appropriately
  areas_vector <- c(insect_only, insect_plant, total_insect)
  names(areas_vector) <- c("Insect_only", "Insect_plant", "Total_insect")
  
  return(areas_vector)
}