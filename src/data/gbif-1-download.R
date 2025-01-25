# Download data for all species from GBIF
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-02

require(dplyr) # For binding results when "other name" exists

# Load up the functions from the functions folder
source(file = "load_functions.R")

gbif_data <- read.csv(file = "data/gbif-reconcile.csv")
replace <- FALSE
verbose <- TRUE
year_range <- c(2000, 2024)

# How frequently to print message about number of species completed
print_freq <- 20

# File to write messages for failed queries; passed to download_gbif
logfile <- "logs/download.log"

sink(file = logfile)
cat(as.character(Sys.time()), " download log", sep = "")
sink()

# For testing with subset
# gbif_data <- gbif_data %>%
#   dplyr::filter(species %in% c("brevicauda", "maximum"))

# Loop over all entries in gbif data and do queries
for (i in 1:nrow(gbif_data)) {
  species_name <- gbif_data$accepted_name[i]
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species_name))
  filename <- paste0("data/gbif/downloaded/", nice_name, "-gbif.csv")
  # Only proceed if file doesn't exist or we want to replace existing files
  if (!file.exists(filename) | replace) {
    gbif_name <- gbif_data$gbif_name[i]
    obs <- download_gbif(species_name = species_name,
                         gbif_name = gbif_name,
                         replace = replace,
                         verbose = TRUE,
                         year_range = year_range,
                         max_attempts = 10,
                         logfile = logfile)
    # Check for other names (where the name of the thing on GBIF might not be 
    # an accepted name, and thus not come back in a "normal" query; e.g. 
    # records of Papilio bairdii oregonia will not be returned with normal 
    # query, as P. bairdii is not recognized as a valid name on GBIF)
    other_names <- unlist(strsplit(x = trimws(x = gbif_data$other_names[i]),
                                   split = ","))
    if (length(other_names) > 0) {
      # Now here's a spot to get fancy with lapply, or be lazy and loop
      # Loop it is!
      for (other_name in other_names) {
        other_obs <- download_gbif(species_name = species_name,
                                   verbatim_name = other_name,
                                   replace = replace,
                                   verbose = TRUE,
                                   year_range = year_range,
                                   max_attempts = 10,
                                   logfile = logfile)
        obs <- obs %>%
          bind_rows(other_obs)
      }
    }

    # Only write if there is at least one observation
    if (!is.null(obs)) {
      if (nrow(obs) > 0) {
        write.csv(x = obs,
                  file = filename,
                  row.names = FALSE)
      }
    }
    
    if (verbose) {
      message(paste0(nrow(obs), " records of ", species_name,
                     " written to ", filename))
    }
    # Adding a 2 second sleep to slow things down.
    Sys.sleep(time = 2)
  } else {
    if (verbose) {
      message(species_name, " records already on disk and rerun set to FALSE.")
    }
  }
  if (i %% print_freq == 0 | i == 1) {
    cat("\n==== Finished querying ", i, " of ", nrow(gbif_data), " species ====\n")
  }
}

# Need to determine the names of the files we just created
gbif_files <- list.files(path = "data/gbif/downloaded",
                         pattern = "*-gbif.csv",
                         full.names = TRUE)

# After all downloads are complete, throw them into a zip archive
zipfile <- "data/gbif-downloaded.zip"
if (file.exists(zipfile)) {
  invisible(file.remove(zipfile))
}
zip(zipfile = zipfile,
    files = gbif_files)
