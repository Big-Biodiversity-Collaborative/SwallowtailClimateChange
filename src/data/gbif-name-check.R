# Check whether GBIF filenames match the species listed in the csv
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-05-09

require(dplyr)
require(stringr)

# Extracts data/gbif folder (that contains individual csvs) from zip file
unzip(zipfile = "data/gbif.zip") 

gbif_files <- list.files(path = "data/gbif",
                         pattern = "*-gbif.csv",
                         full.names = TRUE)

# Extract species from filename
species_filename <- gbif_files %>% 
  str_remove_all("data/gbif/") %>%
  str_remove_all("-gbif.csv") %>%
  str_replace("_", " ")%>%
  str_to_sentence()

# Extract species from "species" column in csv file
species_col <- rep(NA, length(species_filename))
for (i in 1:length(gbif_files)) {
  dat <- read.csv(gbif_files[i])
  species_col[i] <- dat$species[1] 
}

# Create dataframe comparing the two names
gbif_names <- data.frame(species_filename = species_filename, 
                         species_col = species_col)
gbif_names$match <- 1*(gbif_names$species_filename == gbif_names$species_col)

# Inspect cases when names do not match
filter(gbif_names, match == 0)

# Export dataframe as csv
write.csv(gbif_names, 
          file = "output/gbif-name-check.csv",
          row.names = FALSE)

