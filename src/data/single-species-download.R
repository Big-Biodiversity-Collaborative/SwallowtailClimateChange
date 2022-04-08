# Test of pagination for all GBIF records of a single species
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-01

require(spocc)
require(dplyr)
require(stringr)
require(tidyr)

# Toggle weather or not to overwrite data on disk
replace <- TRUE
species_name <- "papilio cresphontes"
nice_name <- tolower(x = gsub(pattern = " ",
                              replacement = "_",
                              x = species_name))
filename <- paste0("data/gbif/", nice_name, "-gbif.csv")

# Only proceed if file doesn't exist or we want to replace existing files
if (!file.exists(filename) | replace) {
  # We use the gbif name to extract results from list object
  gbif_name <- "papilio cresphontes"
  list_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = gbif_name))
  # To restrict to three countries, need to do three separate queries
  countries <- c("CA", "MX", "US")
  query_limit <- 100
  total <- 0
  obs <- NULL
  for (country in countries) {
    message(paste0("Counting GBIF records for ", 
                   species_name, " (as ", gbif_name, ") from ",
                   country))
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
      starts <- seq(from = 0, 
                    to = num_records,
                    by = query_limit)

      # Perform multiple queries, paging over results
      message(paste0("Downloading ", num_records, " records of ", 
                     species_name, " (as ", gbif_name, ") from ",
                     country))
      
      # For edge case, since gbif record indexing actually starts at 0
      if (start < num_records) {
        for (start in starts) {
          end <- min((start + query_limit - 1), num_records)
          message(paste0("...records ", start, " through ", end))
          gbif_query <- spocc::occ(query = gbif_name,
                                   from = "gbif",
                                   limit = query_limit,
                                   start = start,
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
      }
    } else {
      message(paste0("Zero records of ", species_name, " found from ", country))
    }
  }
  
  # Want to make sure that, before we write to file, we have all the standard 
  # columns. It is possible to have results that lack date columns (yes, so much 
  # fun), so add them in if necessary; otherwise select fails
  if (!is.null(obs)) {
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
    
    message(paste0(nrow(obs), " records of ", species_name, 
                   " written to ", filename))
  }
}
