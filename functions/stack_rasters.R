#' Stack rasters of predicted presence / absence
#' 
#' @param r \code{list} of \code{SpatRaster} objects
#' @param out character indicating output format; "binary" returns raster where 
#' cells with summed values >= 1 are reported as 1, while "total" will return 
#' the total summed value of the cell
#' 
#' @details Because the desired output may be based on individual 
#' \code{SpatRaster} objects of differing geographic extent, 
#' \code{stack_rasters} will attempt to extend every raster to the maximum 
#' extent covered by the union of all elements of \code{r}. This function 
#' uses some creative solutions to ensure that cells that are missing data 
#' from a subset of the elements of \code{r} but have data from at least one 
#' element of \code{r} have a non-\code{NA} value returned for that cell. This 
#' behavior was developed for use with \code{raster::mosaic()}, but with the 
#' switch to \code{terra::mosaic()}, may not be necessary. The treatment of 
#' missing values by \code{raster::mosaic()} and binary raster addition (i.e. 
#' `+`). Direct use of \code{raster::mosaic(fun = sum)} would result in 
#' \code{NA} values being converted to zeros in the output raster, while the 
#' binary operation (`+`) returns a raster where the only cells that are not 
#' \code{NA} are those cells which did not have any missing data across all 
#' elements of \code{r}.
#' 
#' @param A \code{SpatRaster} with cell values reflecting the sum of values 
#' across rasters in \code{r}
stack_rasters <- function(r, out = c("total", "binary")) {
  if (!require(terra)) {
    stop("stack_rasters requires terra package, but it could not be loaded")
  }
  out <- match.arg(out)

  # if r is length 1, no need to extend rasters, just assign to our sum_r 
  # object
  if (length(r) == 1) {
    sum_r <- r[[1]]
  } else {
    # Need to create mosiac of ALL rasters, to ensure summing works without 
    # warning. We use this to extend all rasters to the same extent before 
    # summing; start by copying the list of rasters
    x <- r
    # Remove names from x because they cause issues?
    names(x) <- NULL
    x$fun = sum # named argument for do.call()

    # Make a SpatRaster that is the sum of all values; missing and values of 
    # zero all become zero
    mosaic_raster <- do.call(terra::mosaic, x)
    
    # We use this to keep track of the sum of cell values  
    sum_r <- NULL
    # Iterate over all rasters in list r
    for (i in 1:length(r)) {
      one_raster <- r[[i]]
      
      if (is.null(sum_r)) {
        # First one, so just extend the raster to the appropriate extent, 
        # leaving missing values as missing, and assign to sum_r
        sum_r <- terra::extend(x = one_raster,
                               y = mosaic_raster)
      } else {
        # for each additional raster, need to have two extensions: One with 
        # missing values as missing, one with missing values as zero
        
        # We use the 0 raster for actual addition, but the NA raster to then 
        # turn any 0 that should be an NA *back* to NA
        one_raster_NA <- terra::extend(x = one_raster,
                                       y = mosaic_raster)
        one_raster_0 <- terra::extend(x = one_raster,
                                      y = mosaic_raster,
                                      fill = 0)
        # Because one_raster_0 only has 0s in the *extended* area, we need to 
        # manually set any remaining NAs to 0
        one_raster_0[is.na(one_raster_0)] <- 0
        
        # We're adding to the sum_r raster, which has zeros and NAs at this 
        # point.
        # Make a copy of sum_r, and convert the NAs to zeros, so we can sum the 
        # rasters
        sum_r_0 <- sum_r
        sum_r_0[is.na(sum_r_0)] <- 0
        
        # Add the two rasters that have missings as zero together
        sum_r_0 <- sum_r_0 + one_raster_0
        
        # Now use indexing of the two rasters that have missing as NA to change 
        # back any cells that are NA in both rasters
        sum_r_0[is.na(sum_r) & is.na(one_raster_NA)] <- NA
        sum_r <- sum_r_0
      } # end else for elements of r beyond the first
    } # end iteration over list r
  } # end else for input list length > 1
  # Do final conversion if necessary  
  if (out == "binary") {
    sum_r[sum_r > 1] <- 1
  }
  return(sum_r)
}
