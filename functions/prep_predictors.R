#' Standardize predictors, and create quadratics where needed, for lasso 
#' regression (using glmnet) or other SDMs. Code based off of functions created 
#' in Valavi et al. 2021
#'
#' @param object an object of class save_means_sds (created with the 
#' save_means_sds function) that contains means and SDs for predictor variables 
#' in a training dataset
#' @param newdata a raster or data.frame containing the predictor variables 
#' listed in object
#' @param quad a logical indicating whether or not to create quadratics for each
#' of the predictors
#'
#' @return an object of the same class as newdata that contains standardized 
#' values of predictor variables. Names of linear predictors end in "_1". Names
#' of quadratic predictors end in "_2". 

prep_predictors <- function(object, newdata, quad = TRUE){
  if (!methods::is(object, "save_means_sds"))
    stop("Object should be a save_means_sds object.")
  if (!all(object$names %in% names(newdata)))
    stop("The newdata does not have the same names as the object.")
  ncl <- object$names
  
  if (methods::is(newdata, "Raster")){
    newdata <- newdata[[ncl]]
    for (i in ncl) {
      x1 <- (newdata[[i]] - object$xbars[i]) / object$sds[i]
      if(raster::nlayers(newdata) > 1){
        newdata <- newdata[[-which(names(newdata) == i)]]
        newdata <- raster::stack(newdata, x1)
      } else {
        newdata <- x1
      }
      names(newdata)[raster::nlayers(newdata)] <- paste0(i, "_1")
      if (quad) {
        x2 <- x1 ^ 2
        newdata <- raster::stack(newdata, x2)
        names(newdata)[raster::nlayers(newdata)] <- paste0(i, "_2")
      }
    }
    
  } else if (is.data.frame(newdata)) {
    newdata <- dplyr::select(newdata, all_of(ncl))
    for (i in ncl) {
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
  } else stop("Newdata should be a raster or a data.frame.")
  return(newdata)
}
