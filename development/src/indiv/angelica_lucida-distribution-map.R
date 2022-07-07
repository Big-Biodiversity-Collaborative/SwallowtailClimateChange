# Create a distribution map for Angelica lucida
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-07-07

# Load up the necessary functions
source(file = "development/functions/single_map_devel.R")

genus <- "Angelica"
species <- "lucida"

model <- "svmw"

time_period <- "current"

# Name for reporting
species_name <- paste0(genus, " ", species)

distribution_map <- single_map_devel(species_name,
                                     time_period, 
                                     model)
print(distribution_map)



