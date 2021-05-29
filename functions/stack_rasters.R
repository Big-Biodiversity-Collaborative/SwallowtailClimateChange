#' Stack rasters
#' 
#' @param r \code{list} of \code{Raster*} objects
#' @param neg_values character indicating how to treat negative values; 
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

  # Iterate over list of rasters, adding each as we go  
  mosaic_out <- NULL
  for (i in 1:length(r)) {
    one_raster <- r[[i]]
    
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

  # If desired, need to convert any values above 1 to 1
  # TODO: what about values 0 > x > 1?
  if (out == "binary") {
    mosaic_out[mosaic_out > 1] <- 1
  }
  return(mosaic_out)
}