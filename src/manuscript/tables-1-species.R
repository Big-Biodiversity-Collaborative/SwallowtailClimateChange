# Create table with species information for manuscript
# Erin Zylstra & Jeff Oliver
# ezylstra@arizona.edu & jcoliver@arizona.edu
# 2024-12-05

library(dplyr)
require(stringr)

# Load insect-host information and gbif data summaries
ih <- read.csv("data/insect-host.csv")
gbif <- read.csv("data/gbif-pa-summary.csv")

# File with east/west designation information
ew <- read.csv(file = "data/insect-eastwest.csv")

# Base of output filenames
output_basename <- "output/manuscript/"

# Get number of filtered records for each swallowtail spp
insects <- gbif %>%
  filter(str_detect(species, "Papilio")) 

# Retain only those that have a presence/absence CSV file
insects <- insects %>%
  filter(pa_csv == "yes") %>%
  select(species, n_filtered)

# Get number of host plant species (total or with 40+ filtered records)
ih2 <- ih %>%
  select(insect, host_accepted) %>%
  left_join(select(gbif, species, pa_csv), 
            join_by("host_accepted" == "species")) %>%
  group_by(insect) %>%
  summarize(n_hosts = length(host_accepted),
            n_hosts_suff = sum(pa_csv == "yes")) %>%
  data.frame()

# Add host species counts to insects data
insects <- left_join(insects, ih2, join_by("species" == "insect"))

# Add in east/west indicator & shorten names
insects <- insects %>%
  left_join(ew, by = c("species" = "insect")) %>%
  mutate(species = str_replace(species, "Papilio", "P."))

# Update column names more appropriate for manuscript
insects <- insects %>%
  rename(Species = species,
         `# Records` = n_filtered,
         `# Host species` = n_hosts,
         `# Host SDMs` = n_hosts_suff,
         Location = ew)

# Write to file
papilio_table <- paste0(output_basename, "papilio-table.csv")
write.csv(x = insects, 
          file = papilio_table, 
          row.names = FALSE)
