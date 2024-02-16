# Download data for all species from GBIF
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-02

# require(dplyr) # currently only needed for testing subset

# Load up the functions from the functions folder
source(file = "load_functions.R")

gbif_data <- read.csv(file = "data/gbif-reconcile.csv")
replace <- FALSE

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
  gbif_name <- gbif_data$gbif_name[i]
  download_gbif(species_name = species_name,
                gbif_name = gbif_name,
                replace = replace,
                verbose = TRUE,
                max_attempts = 10,
                logfile = logfile)
  # Adding a 2 second sleep to slow things down.
  Sys.sleep(time = 2)
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
