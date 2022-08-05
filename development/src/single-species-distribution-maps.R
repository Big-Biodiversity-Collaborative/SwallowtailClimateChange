# Create predicted distribution maps for a given species
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-08-05

# Load up the necessary functions
source(file = "functions/single_map.R")

genus <- "Papilio"
species <- "rumiko"

model <- "maxent-notune"

climate_models <- read.csv(file = "data/climate-models.csv")

# Name for reporting
species_name <- paste0(genus, " ", species)

# Print a distribution map for each set of predictors (current and future)
for (predictor in climate_models$name) {

  distribution_map <- single_map(species_name,
                                 predictor = predictor, 
                                 model,
                                 show_obs = TRUE) 
  print(distribution_map)

}

