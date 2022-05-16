# Add accepted name of hostplants to gbif-reconcile.csv and insect-host.csv
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-05-16

require(TNRS)
require(dplyr)

# TO DO: 
# Add accepted name of hostplants to insect_host
# Modify the code above to check or update accepted names in gbif-reconcile.csv
# (Current version adds an accepted_name column assuming it doesn't already exist)
# Output updated csvs

# Read in files
rec <- read.csv(file = "data/gbif-reconcile.csv")
insect_host <- read.csv(file = "data/insect-host.csv")

# Extract hostplant names from gbif-reconcile
gbif_hosts <- subset(rec, genus != "Papilio", select = c(genus, species))
gbif_hosts$Genus_species <- paste(gbif_hosts$genus, gbif_hosts$species, sep = " ")

# Use TNRS package to find the accepted name for each hostplant
# (using the default taxonomic sources: TROPICOS, WCVP)
gbif_hosts$accepted_name <- 
  TNRS(taxonomic_names = gbif_hosts$Genus_species)[,"Accepted_species"]

# Which species have different accepted names?
filter(gbif_hosts, Genus_species != accepted_name)

# Merge accepted names with original csv
rec <- left_join(rec, select(gbif_hosts, -Genus_species))

# Add in insect accepted names
rec$accepted_name <- ifelse(is.na(rec$accepted_name),
                                 paste(rec$genus, rec$species, sep = " "),
                                 rec$accepted_name)
rec <- relocate(rec, accepted_name, .after = gbif_name)


# Are there hostplants in insect_host that aren't in gbif-reconcile?
insect_host$host[!insect_host$host %in% paste0(rec$genus, " ", rec$species)]

  # Heracleum lanatum (known issue -- included with H. maximum)
  # TNRS will output H. maximum as accepted name
  
  # Prunus armeniaca (looks like its non-native, but throughout, North America)
  insect_host[insect_host$host == "Prunus armeniaca",]
  rec[rec$genus == "Prunus",]




