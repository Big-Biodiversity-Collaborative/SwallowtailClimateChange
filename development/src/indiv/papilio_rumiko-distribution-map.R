# Create a distribution map for Papilio rumiko
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-07-07

# Load up the necessary functions
source(file = "development/functions/single_map_devel.R")

genus <- "Papilio"
species <- "rumiko"

model <- "maxent"

time_periods <- c("current", 
                  "ssp245-2041", "ssp245-2071", 
                  "ssp370-2041", "ssp370-2071")

# Name for reporting
species_name <- paste0(genus, " ", species)

distribution_map <- single_map_devel(species_name,
                                     time_period = time_periods[5], 
                                     model,
                                     show_obs = TRUE)
print(distribution_map)



