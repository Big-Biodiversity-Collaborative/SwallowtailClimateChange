#' Reads in GBIF data and cleans up for SDM
#' 
#' @param file a tab-delimited file of GBIF occurrence data
#' 
#' @details Developed for use with occurrence data that are manually downloaded 
#' from the GBIF website. Designed to filter such data and align selected 
#' column names to those output by \code{spocc::occ}. Specifically:
#' \tabular{ll}{
#'   \strong{Manual download} \tab \strong{\code{spocc::occ()}} \cr
#'   issue \tab issues \cr
#'   decimalLongitude \tab longitude \cr
#'   decimalLatitude \tab latitude
#' }
#' 
#' @return data frame with the following columns:
#' \describe{
#'   \item{gbifID}{First item}
#'   \item{species}{Second item}
#'   \item{longitude}{Longitude in decimal degrees}
#'   \item{latitude}{Latitude in decimal degrees}
#'   \item{year}{Four digit year}
#'   \item{month}{Two digit month}
#'   \item{day}{Two digit day}
#' }
clean_gbif <- function(file) {
  # TODO: See also some nice utilities in the scrubr package
  # https://cran.r-project.org/package=scrubr
  if (!require(dplyr)) {
    stop("clean_gbif requires dplyr package, but it could not be loaded")
  }
  if (!require(tidyr)) {
    stop("clean_gbif requires tidyr package, but it could not be loaded")
  }
  if (!require(stringr)) {
    stop("clean_gbif requires stringr package, but it could not be loaded")
  }
  
  obs <- read.delim(file = file)
  clean_obs <- obs %>% 
    dplyr::filter(countryCode %in% c("CA", "MX", "US")) %>%
    dplyr::filter(stringr::str_detect(issue, pattern = "ZERO_COORDINATE", negate = TRUE)) %>%
    dplyr::select(gbifID, species, decimalLongitude, decimalLatitude, 
                  year, month, day) %>%
    dplyr::rename(longitude = decimalLongitude,
                  latitude = decimalLatitude) %>%
    tidyr::drop_na() # Need complete records for all these
  
  return(clean_obs)
}
