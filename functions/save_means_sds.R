#' Save means and SDs needed to standardize continuous covariates for lasso 
#' regression or other SDM models. Code based off of functions created in Valavi 
#' et al. 2021
#'
#' @param df a data.frame, typically the training data.
#' @param cols the name or index of the columns for which we want to calculate
#' a mean and SD. If NULL, all columns will be included.
#' @param verbose logical indicating whether or not to print processing messages
#'
#' @return a list (of class save_means_sds) with the following elements that can 
#' be used to standardize rasters and data.frames for prediction
#' \describe{
#'   \item{names}{vector of predictor names}
#'   \item{xbars}{vector of means}
#'   \item{sds}{vector of sds}
#' }

save_means_sds <- function(df, cols = NULL, verbose = TRUE){
  
  if(is.null(cols)){
    cols <- colnames(df)
  }
  if(is.numeric(cols)){
    cols <- colnames(df)[cols]
  }
  # remove the factors
  if(any(sapply(df[,cols], is.factor))){
    if(verbose){
      message("The factor columns were removed form cols: ", cols[which(sapply(df[,cols], is.factor))])
    }
    cols <- cols[-which(sapply(df[,cols], is.factor))]
  }
  if(!all(is.element(cols, colnames(df)))){
    stop("All cols need to appear in the column names.")
  }
  xbars <- apply(df[,cols], 2, mean)
  sds <- apply(df[,cols], 2, sd)
  finalList <- list(names = cols, xbars = xbars, sds = sds)
  class(finalList) <- c("save_means_sds")
  return(finalList)
}
