#' Stack rasters of predicted presence / absence
#' 
#' @param r \code{list} of \code{Raster*} objects
#' @param neg_values DEPRECATED character indicating how to treat negative values; 
#' "missing" will ignore values below zero when summing values for a cell, 
#' while "as-is" will use negative values as one normally would in addition
#' @param out character indicating output format; "binary" returns raster where 
#' cells with summed values >= 1 are reported as 1, while "total" will return 
#' the total summed value of the cell
#' 
#' @details Because the desired output may be based on individual \code{Raster*}
#' objects of differing geographic extent, \code{stack_rasters} will attempt to 
#' extend every raster to the maximum extent covered by the union of all 
#' elements of \code{r}. Due to the treatment of missing values by 
#' \code{raster::mosaic()} and binary raster addition (i.e. `+`), this function 
#' uses some creative workarounds to ensure that cells that are missing data 
#' from a subset of the elements of \code{r} but have data from at least one 
#' element of \code{r} have a non-\code{NA} value returned for that cell. 
#' Direct use of \code{raster::mosaic(fun = sum)} would result in NA values 
#' being converted to zeros in the output raster, while the binary operation 
#' (`+`) returns a raster where the only cells that are not \code{NA} are those 
#' cells which did not have any missing data across all elements of \code{r}.
#' 
#' @param A \code{RasterLayer} with cell values reflecting the sum of values 
#' across rasters in \code{r}
stack_rasters <- function(r, neg_values = c("missing", "as-is"), 
                          out = c("total", "binary")) {
  if (!require(raster)) {
    stop("stack_rasters requires raster package, but it could not be loaded")
  }
  neg_values <- match.arg(neg_values)
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
    x$fun = sum
    x$na.rm = TRUE
    
    mosaic_raster <- do.call(raster::mosaic, x)
    
    # We use this to keep track of the sum of cell values  
    sum_r <- NULL
    # Iterate over all rasters in list r
    for (i in 1:length(r)) {
      one_raster <- r[[i]]
      
      if (is.null(sum_r)) {
        # First one, so just extend the raster to the appropriate extent, leaving
        # missing values as missing, and assign to sum_r
        sum_r <- raster::extend(x = one_raster,
                                y = mosaic_raster)
      } else {
        # for each additional raster, need to have two extentions: One with missing 
        # values as missing, one with missing values as zero
        
        # We use the 0 raster for actual addition, but the NA raster to then turn all 
        # zeros back to that should be NA afterwards
        one_raster_NA <- raster::extend(x = one_raster,
                                        y = mosaic_raster)
        one_raster_0 <- raster::extend(x = one_raster,
                                       y = mosaic_raster,
                                       value = 0)
        # We're adding to the sum_r raster, which has zeros and NAs at this point.
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
