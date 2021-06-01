# Counting number of occurrences we can get from gbif via spocc::occ
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-01

require(spocc)
require(dplyr)

# Read in list of species we are interested in
na_papilio_file <- "data/na-papilio-species.csv"
papilio_species <- read.csv(file = na_papilio_file)

# Create a num_gbif column if it doesn't exist already
if (!("gbif_count" %in% colnames(papilio_species))) {
  papilio_species$gbif_count <- NA
}

# Create a date_gbif column if it doesn't exist already
if (!("gbif_date" %in% colnames(papilio_species))) {
  papilio_species$gbif_date <- NA
}

# Iterate over each row, doing search for gbif records with lat/long data
for (i in 1:nrow(papilio_species)) {
  species <- paste(papilio_species$genus[i], papilio_species$species[i])
  gbif_name <- papilio_species$gbif_name[i]
  message(paste0("Counting GBIF records for ", species, " (as ", gbif_name, ")"))
  
  gbif_query <- spocc::occ(query = gbif_name,
                           from = "gbif",
                           limit = 1,
                           has_coords = TRUE)
  papilio_species$gbif_count[i] <- gbif_query$gbif$meta$found
  papilio_species$gbif_date <- Sys.Date()
}

# Update our data file with count information
write.csv(x = papilio_species,
          file = na_papilio_file)
