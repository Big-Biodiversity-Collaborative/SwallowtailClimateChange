# Update accepted names of hostplants in gbif-reconcile.csv and insect-host.csv
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-05-16

require(TNRS)
require(dplyr)

# Read in files
rec <- read.csv(file = "data/gbif-reconcile.csv")
insect_host <- read.csv(file = "data/insect-host.csv")

# For convenience, temporarily create Genus_species column in rec
rec$Genus_species <- paste(rec$genus, rec$species, sep = " ")

# Extract (original) hostplant names from gbif-reconcile
# (note: we may get rid of these columns and just go with accepted name eventually)
gbif_hosts <- subset(rec, genus != "Papilio", select = c(Genus_species, accepted_name))

# Use TNRS package to find the accepted name for each hostplant
# (using default taxonomic sources: TROPICOS, WCVP)
gbif_hosts$accepted_new <- TNRS(gbif_hosts$Genus_species)[,"Accepted_species"]

# Extract (original names of) hostplants that need to be updated
spp_update <- 
  gbif_hosts$Genus_species[which(gbif_hosts$accepted_name != gbif_hosts$accepted_new)]

if (length(spp_update) == 0) {
  message("Accepted names of hostplants already up to date")
} else {
  for (species in spp_update) {
    message(paste0("Updating accepted name for ", species))
    new_name <- gbif_hosts$accepted_new[gbif_hosts$Genus_species == species]
    # Update accepted names in gbif-reconcile (rec) and insect-host
    rec <- rec %>% mutate(accepted_name = replace(accepted_name, 
                                                  Genus_species == species,
                                                  new_name))
    insect_host <- insect_host %>% mutate(host_accepted = replace(host_accepted,
                                                                  host == species,
                                                                  new_name))
  } 
}

# If any names have been updated, overwrite gbif-reconcile and insect-host csvs
if (length(spp_update) > 0) {
  write.csv(select(rec, -Genus_species),
            file = "data/gbif-reconcile.csv",
            row.names = FALSE)
  write.csv(insect_host,
            file = "data/insect-host.csv",
            row.names = FALSE)
}

