# Compare recent GBIF download to prior counts
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-01-28

library(dplyr)

# Make this super-easy and just count rows of data.
old_files_dir <- "~/Desktop/gbif-filtered/data/gbif/filtered"
# old_files <- list.files(old_files_dir)
# old_files_dir <- "data/gbif/filtered"

new_files_dir <- "data/gbif/filtered"
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
  
gbif_counts <- gbif_counts %>%
  mutate(delta = new_count - old_count) %>%
  arrange(delta, species_name) 

# There was a drop of 522 observations for P. canadensis. Pull out those that 
# were missing to see if there is anything suspicious about them 
# (i.e. re-classified?)
old <- paste0(old_files_dir, "/papilio_canadensis-gbif.csv")
new <- paste0(new_files_dir, "/papilio_canadensis-gbif.csv")

old_pc <- read.csv(file = old)
new_pc <- read.csv(file = new)

# Add a column for ease of identifying old/new
old_pc$source <- "old"
new_pc$source <- "new"

# Pull out those previous observations that did not make it through this time
new_ids <- new_pc$gbifID
old_gone <- old_pc %>%
  filter(!(gbifID %in% new_ids))

# Looks like some things in GBIF are called "Pterourus canadensis" (but not 
# all)
distinct_obs <- old_pc %>%
  bind_rows(new_pc) %>%
  distinct(longitude, latitude, year, month, day, countryCode, basisOfRecord, 
           .keep_all = TRUE)
