# Create maps from maxent predictions for Papilio rumiko and 2 of its hosts
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-07-15

require(ggplot2)

genus <- "Papilio"
species <- "rumiko"

# Name for reporting
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

method <- "maxent"
# logfile <- "logs/maps-glm-out.log"
file_ext <- "png" # "pdf"

# Load up the functions from the functions folder
source(file = "load_functions.R")
source(file = "development/functions/overlap_map_devel.R")

# Number of maps created for this species (for reporting)
num_maps <- 0
  
# For each of the climate models, want to create the map then save it to a 
# file.
predictors <- c("current", "ssp370-2041")
for (predictor in predictors) {
  one_map <- overlap_map_devel(species_name = species_name,
                               predictor = predictor,
                               model = method)
  
  # Write to file if not null
  if (!is.null(one_map)) {
    mapfile <- paste0("development/output/maps/",
                      nice_name, 
                      "-overlap-",
                      method, 
                      "-",
                      predictor, 
                      ".",
                      file_ext)
    ggsave(filename = mapfile,
           plot = one_map)
    num_maps <- num_maps + 1
  }
} # end iterating over predictors
message_out <- paste0("Wrote ", num_maps, " overlap map(s) for ", 
                      species_name, ".")
  
# Extra info if fewer than expected maps were made
if (num_maps < length(predictors)) {
  message_out <- paste0(message_out, 
                        " At least one map for the set of predictors for ",
                        species_name, " was not made.")
}
  
