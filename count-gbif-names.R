# Counting number of occurrences we can get from gbif via spocc::occ
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-01

require(spocc)  # for querying
require(dplyr)  # Only needed for QA/QC

# Read in list of species we are interested in
gbif_name_file <- "data/gbif-reconcile.csv"
species_df <- read.csv(file = gbif_name_file)

# Create a num_gbif column if it doesn't exist already
if (!("gbif_count" %in% colnames(species_df))) {
  species_df$gbif_count <- NA
}

# Create a date_gbif column if it doesn't exist already
if (!("gbif_date" %in% colnames(species_df))) {
  species_df$gbif_date <- NA
}

countries <- c("CA", "MX", "US")
# Iterate over each row, doing search for gbif records with lat/long data
for (i in 1:nrow(species_df)) {
  species_name <- paste(species_df$genus[i], species_df$species[i])
  gbif_name <- species_df$gbif_name[i]

  # Limiting by continent DOES NOT WORK
  # Cannot search for multiple countries. Do one query for each country of 
  # interest (CA, MX, US)
  total <- 0
  for (country in countries) {
    message(paste0("Counting GBIF records for ", 
                   species_name, " (as ", gbif_name, ") from ",
                   country))
    gbif_query <- spocc::occ(query = gbif_name,
                             from = "gbif",
                             limit = 1,
                             has_coords = TRUE,
                             gbifopts = list(country = country))
    total <- total + gbif_query$gbif$meta$found
  }
  
  species_df$gbif_count[i] <- total
  species_df$gbif_date <- Sys.Date()
}

# To see things of interest
species_df %>%
  select(genus, species, gbif_name, gbif_count)

# Which zero (or very few) records?
species_df %>%
  select(gbif_name, gbif_count, notes) %>%
  filter(gbif_count < 40)

# Update our data file with count information
write.csv(x = species_df,
          file = gbif_name_file,
          row.names = FALSE)
