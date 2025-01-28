# Compare recent GBIF download to prior counts
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-01-28

library(dplyr)

# Make this super-easy and just count rows of data.
# old_files_dir <- "~/Desktop/gbif-downloaded/data/gbif/downloaded"
# old_files <- list.files(old_files_dir)
old_files_dir <- "data/gbif/filtered"

new_files_dir <- "data/gbif/downloaded"
# new_files <- list.files(new_files_dir)

# Files are named with the "accepted_name" column from gbif_reconcile
gbif <- read.csv(file = "data/gbif-reconcile.csv")
species_names <- gbif$accepted_name

# Keep track of name and old vs. new
gbif_counts <- data.frame(species_name = species_names,
                          old_count = NA_integer_,
                          new_count = NA_integer_)

# Iterate over all those names and compare count of old to new
for (i in 1:nrow(gbif_counts)) {
  species_name <- gbif_counts$species_name[i]
  nice_name <- tolower(x = gsub(pattern = " ",
                                 replacement = "_",
                                 x = species_name))
  # message("Checking ", species_name)
  old_file <- paste0(old_files_dir, "/", nice_name, "-gbif.csv")
  if (file.exists(old_file)) {
    old_data <- read.csv(file = old_file)
  } else {
    old_data <- NULL
  }

  new_file <- paste0(new_files_dir, "/", nice_name, "-gbif.csv")
  if (file.exists(new_file)) {
    new_data <- read.csv(file = new_file)
  } else {
    new_data <- NULL
  }
  
  if (!is.null(old_data) & !is.null(new_data)) {
    gbif_counts$old_count[i] <- nrow(old_data)
    gbif_counts$new_count[i] <- nrow(new_data)
  } else {
    if (is.null(new_data)) {
      if (!is.null(old_data)) {
        # Looks like only two fit these criteria, but both were ultimately 
        # filtered out in prior workflow (Peucedanum oreoselinum & Silaum 
        # silaus)
        message("New data NULL for ", species_name, " but had old data...")
      }
    }
  }
}
  
# Note a significant change in the workflow will result in many, if not most 
# counts to go *DOWN* at this point, as the download only included observations 
# in 2000-2024, where previous iterations delayed date filtering
gbif_counts <- gbif_counts %>%
  mutate(delta = old_count - new_count) %>%
  arrange(delta, species_name) 

