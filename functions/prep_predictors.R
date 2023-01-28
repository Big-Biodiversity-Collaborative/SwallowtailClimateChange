#' Standardize predictors, and create quadratics where needed, for lasso 
#' regression (using glmnet) or other SDMs. Code based off of functions created 
#' in Valavi et al. 2021
#'
#' @param object an object of class save_means_sds (created with the 
#' save_means_sds function) that contains means and SDs for predictor variables 
#' in a training dataset
#' @param newdata a SpatRaster or data.frame containing the predictor variables 
#' listed in object
#' @param quad a logical indicating whether or not to create quadratics for each
#' of the predictors
#'
#' @return an object of the same class as newdata that contains standardized 
#' values of predictor variables. Names of linear predictors end in "_1". Names
#' of quadratic predictors end in "_2". 

prep_predictors <- function(object, newdata, quad = TRUE){
  if (!methods::is(object, "save_means_sds")) {
    stop("Object should be a save_means_sds object.")
  }
  if (!all(object$names %in% names(newdata))) {
    stop("The newdata does not have the same names as the object.")
  }
  
  # Extract the vector of biovar names in the model
  lyr_names <- object$names
  
  if (methods::is(newdata, "SpatRaster")){
    # Pull out just the biovar layers from the SpatRaster
    newdata <- newdata[[lyr_names]]
    
    # New implementation, not ready yet, doesn't behave quite the same...
    newdata_n <- newdata
    # Standardize biovar values (subtract mean, divide by sd)
    newdata_n <- terra::app(x = newdata_n,
                            fun = function(l, xbars, sds) {
                              return((l - xbars)/sds)
                            },
                            xbars = object$xbars,
                            sds = object$sds)

    # Will need to rename layers, so keep track of how many there are
    num_layers <- terra::nlyr(newdata_n)
    # If requested, also calculate quadratic values and add them as layers
    # to the SpatRaster object
    if (quad) {
      newdata_n <- c(newdata_n, newdata_n^2)
      # Update names for those new layers
      names(newdata_n)[(num_layers + 1):terra::nlyr(newdata_n)] <- paste0(names(newdata_n)[1:num_layers], "_2")
    }
    # Append a "_1" to (original) layer names
    names(newdata_n)[1:num_layers] <- paste0(names(newdata_n)[1:num_layers], "_1")
    # A cludge to get layers into same order as prior implementation; this is 
    # REQUIRED as subsequent processing happens via layer order, not layer
    # names
    name_order <- character(2 * length(lyr_names))
    for (i in 1:length(lyr_names)) {
      name_order[(2 * i) - 1] <- paste0(lyr_names[i], "_1")
      name_order[(2 * i)] <- paste0(lyr_names[i], "_2")
    }
    newdata_n <- newdata_n[[name_order]]
    newdata <- newdata_n

    # Original implementation
    # for (i in lyr_names) {
    #   # Standardize biovar values (subtract mean, divide by sd)
    #   x1 <- (newdata[[i]] - object$xbars[i]) / object$sds[i]
    #   if(terra::nlyr(newdata) > 1){
    #     newdata <- newdata[[-which(names(newdata) == i)]]
    #     newdata <- c(newdata, x1)
    #   } else {
    #     newdata <- x1
    #   }
    #   names(newdata)[terra::nlyr(newdata)] <- paste0(i, "_1")
    #   if (quad) {
    #     x2 <- x1 ^ 2
    #     newdata <- c(newdata, x2)
    #     names(newdata)[terra::nlyr(newdata)] <- paste0(i, "_2")
    #   }
    # }
    
  } else if (is.data.frame(newdata)) {
    newdata <- dplyr::select(newdata, all_of(lyr_names))
    for (i in lyr_names) {
      x1 <- (newdata[,i] - object$xbars[i]) / object$sds[i]
      newdata <- newdata[,-which(names(newdata) == i)]
      newdata[,ncol(newdata) + 1] <- x1
      names(newdata)[ncol(newdata)] <- paste0(i, "_1")
      if (quad) {
        x2 <- x1 ^ 2
        newdata[,ncol(newdata) + 1] <- x2
        names(newdata)[ncol(newdata)] <- paste0(i, "_2")
      }
    }
  } else stop("newdata should be a SpatRaster or a data.frame.")
  return(newdata)
}
