#' Reads in GBIF data and cleans up for SDM
#' 
#' @param file a tab-delimited file of GBIF occurrence data
#' 
#' @return data frame with the following columns:
#' \describe{
#'   \item{gbifID}{First item}
#'   \item{species}{Second item}
#'   \item{decimalLongitude}{Longitude in decimal degrees}
#'   \item{decimalLatitude}{Latitude in decimal degrees}
#'   \item{year}{Four digit year}
#'   \item{month}{Two digit month}
#'   \item{day}{Two digit day}
#' }
clean_gbif <- function(file) {
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
    tidyr::drop_na() # Need complete records for all these
  
  return(clean_obs)
}
