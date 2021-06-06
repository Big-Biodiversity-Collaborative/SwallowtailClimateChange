#' Stack rasters
#' 
#' @param r \code{list} of \code{Raster*} objects
#' @param neg_values DEPRECATED character indicating how to treat negative values; 
#' "missing" will ignore values below zero when summing values for a cell, 
#' while "as-is" will use negative values as one normally would in addition
#' @param out character indicating output format; "binary" returns raster where 
#' cells with summed values >= 1 are reported as 1, while "total" will return 
#' the total summed value of the cell
#' 
stack_rasters <- function(r, neg_values = c("missing", "as-is"), 
                          out = c("total", "binary")) {
  if (!require(raster)) {
    stop("stack_rasters requires raster package, but it could not be loaded")
  }
  neg_values <- match.arg(neg_values)
  out <- match.arg(out)

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
    }
  }
  
  if (out == "binary") {
    sum_r[sum_r > 1] <- 1
  }
  
  return(sum_r)
  

  # Use a copy of the list because we'll add non-raster elements to it for the 
  # do.call application of raster::mosaic
  x <- r
  # Remove names from x because they cause issues?
  names(x) <- NULL
  x$fun = sum
  x$na.rm = TRUE
  mosaic_raster <- do.call(raster::mosaic, x)

  # Start by adding 1 to every raster, since missing values will end up taking 
  # on a value of zero
  incremented_r <- r
  for (i in 1:length(incremented_r)) {
    incremented_r[[i]][] <- incremented_r[[i]][] + 1
  }

  # apply this new raster to extend all original rasters to same extent
  extended_r <- lapply(X = incremented_r, 
                       FUN = raster::extend,
                       y = mosaic_raster,
                       value = 0)
plot(extended_r[[2]])
  # All rasters have been extended and padded with values of 0 as necessary
  sum_r <- Reduce("+", x = extended_r)
plot(sum_r)  

  # Need to turn anything that is still a 0 to missing
  sum_r[sum_r == 0] <- NA
  plot(sum_r)  
  
  # make a land object
  land <- sum_r
  land[!is.na(land)] <- 0
plot(land)

  
  # apply this new raster to extend all original rasters to same extent
  extended_r <- lapply(X = r, 
                       FUN = raster::extend,
                       y = mosaic_raster)
                       # value = 0)

  # Sum all rasters  
  # sum_r <- Reduce("+", x = extended_r)
  
  plot(sum_r)
  sum_r <- NULL
  for (i in 1:length(extended_r)) {
    if (is.null(sum_r)) {
      sum_r <- extended_r[[i]]
    } else {
      sum_r <- sum(sum_r, extended_r[[i]], na.rm = TRUE)
    }
  }
  plot(sum_r)
  
  # If desired, need to convert any values above 1 to 1
  # TODO: what about values 0 > x > 1?
  # if (out == "binary") {
  #   sum_r[sum_r > 1] <- 1
  # }

  return(sum_r)
  
  
  old <- FALSE
  if (old) {
    
  # Iterate over list of rasters, adding each as we go  
  mosaic_out <- NULL
  
  for (i in 1:length(r)) {
    one_raster <- r[[i]]
    
    # Extend raster to ensure everying is the same size
    one_raster <- raster::extend(one_raster, mosaic_raster)
    
    if (neg_values == "missing") {
      one_raster[one_raster < 0] <- NA
      # one_raster[one_raster <= 0] <- NA
    }
    
    
    
    
    if (is.null(mosaic_out)) {
      # First raster, just use that for the output
      mosaic_out <- one_raster
    } else {
      # Not the first, so use raster::mosaic to sum
      mosaic_out <- raster::mosaic(x = mosaic_out,
                                   y = one_raster,
                                   fun = sum)
    }
  }
}
  # If desired, need to convert any values above 1 to 1
  # TODO: what about values 0 > x > 1?
  if (out == "binary") {
    mosaic_out[mosaic_out > 1] <- 1
  }
  return(mosaic_out)
}