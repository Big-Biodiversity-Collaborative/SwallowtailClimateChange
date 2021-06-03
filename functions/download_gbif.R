#' Download data from GBIF for a single species
#' 
#' @param species_name the name of the species for record keeping
#' @param gbif_name the name to query GBIF for; not necessarily the same as 
#' \code{species_name} due to GBIF taxon name problems
#' @param replace logical indicating whether or not to overwrite data for this 
#' species that may already exist on disk
#' @param verbose logical indicating whether or not to print progress messages
#' @param countries character vector of country codes to restrict search 
#' @param query_limit integer number of results to return per query
download_gbif <- function(species_name, gbif_name, replace = FALSE, 
                          verbose = FALSE, countries = c("CA", "MX", "US"),
                          query_limit = 500) {
  if (!require(dplyr)) {
    stop("download_gbif requires dplyr package, but it could not be loaded")
  }
  if (!require(spocc)) {
    stop("download_gbif requires spocc package, but it could not be loaded")
  }
  if (!require(stringr)) {
    stop("download_gbif requires stringr package, but it could not be loaded")
  }

  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species_name))
  filename <- paste0("data/", nice_name, "-gbif.csv")
  
  # Only proceed if file doesn't exist or we want to replace existing files
  if (!file.exists(filename) | replace) {
    # We use the gbif name to extract results from list object
    list_name <- tolower(x = gsub(pattern = " ",
                                  replacement = "_",
                                  x = gbif_name))
    # To restrict to three countries, need to do three separate queries
    total <- 0
    obs <- NULL
    for (country in countries) {
      if (verbose) {
        message(paste0("Counting GBIF records for ", 
                       species_name, " (as ", gbif_name, ") from ",
                       country))
      }
      # Start with query just to count number of records, so we know how many 
      # pages of results we'll need to request
      gbif_count <- spocc::occ(query = gbif_name,
                               from = "gbif",
                               limit = 1,
                               has_coords = TRUE,
                               gbifopts = list(country = country))
      
      num_records <- gbif_count$gbif$meta$found
      total <- total + num_records
      
      if (num_records > 0) {
        pages <- ceiling(num_records/query_limit) - 1
        
        # Perform multiple queries, paging over results
        if (verbose) {
          message(paste0("Downloading ", num_records, " records of ", 
                         species_name, " (as ", gbif_name, ") from ",
                         country))
        }
        for (page in 0:pages) {
          if (verbose) {
            message(paste0("...page ", (page + 1), " of ", (pages + 1)))
          }
          gbif_query <- spocc::occ(query = gbif_name,
                                   from = "gbif",
                                   limit = query_limit,
                                   start = page,
                                   has_coords = TRUE,
                                   gbifopts = list(country = country))
          
          # Extract the data and clean it up before adding to results
          # Doing the filtration separate as it may result in zero results
          # spocc::occ returns list of data frames, indexed by query
          query_data <- gbif_query$gbif$data[[list_name]] %>%
            dplyr::filter(countryCode %in% c("CA", "MX", "US"),
                          stringr::str_detect(issues, pattern = "ZERO_COORDINATE", 
                                              negate = TRUE))
          
          # If any data remain, add to results
          if (nrow(query_data) > 0) {
            if (is.null(obs)) {
              obs <- query_data
            } else {
              obs <- obs %>%
                dplyr::bind_rows(query_data)
            }
          }
        }
      } else {
        if (verbose) {
          message(paste0("Zero records of ", species_name, " found from ", country))
        }
      }
    } # End iterating over all countries

    # Ready to do final data prep and write to disk
    if (!is.null(obs)) {    
      # Want to make sure that, before we write to file, we have all the standard 
      # columns. It is possible to have results that lack date columns (yes, so 
      # much fun), so add them in if necessary; otherwise select fails
      if (!("year" %in% colnames(obs))) {
        obs <- obs %>%
          dplyr::mutate(year = NA)
      }
      if (!("month" %in% colnames(obs))) {
        obs <- obs %>%
          dplyr::mutate(month = NA)
      }
      if (!("day" %in% colnames(obs))) {
        obs <- obs %>%
          dplyr::mutate(day = NA)
      }
      
      # Select only those columns we want to write to file
      obs <- obs %>%
        dplyr::select(gbifID, species, longitude, latitude, 
                      year, month, day, countryCode)
      
      write.csv(x = obs,
                file = filename,
                row.names = FALSE)
      
      if (verbose) {
        message(paste0(nrow(obs), " records of ", species_name, 
                       " written to ", filename))
      }
    } else {
      if (verbose) {
        message(paste0("After filtering, no records of ", species_name,
                       " saved to disk."))
      }
    }
  }
}