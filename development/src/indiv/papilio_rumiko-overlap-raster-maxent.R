# Generate overlap raster for Papilio rumiko and two hostplants based
# on maxent model
# ezylstra@arizona.edu
# 2022-07-15

source(file = "load_functions.R")
source(file = "development/functions/overlap_raster_devel.R")

genus <- "Papilio"
species <- "rumiko"

model <- "maxent"

# Name for reporting
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

message(paste0("Creating overlap rasters based on ", toupper(model), 
               " for ", species_name))

# Number of overlap rasters created for this species (for reporting)
num_overlaps <- 0

# For each of the predictors, want to create the overlap raster then save it 
# to a file. 
predictors <- c("current", "ssp370-2041")
for (predictor in predictors) {
  overlap <- overlap_raster_devel(species_name = species_name,
                                  predictor = predictor,
                                  model = model)
  
  # As long as there is something there, write to file
  if (!is.null(overlap)) {
    overlap_file <- paste0("development/output/overlap/",
                           nice_name, 
                           "-overlap-",
                           model, 
                           "-",
                           predictor, 
                           ".rds")
    saveRDS(object = overlap, 
            file = overlap_file)
    num_overlaps <- num_overlaps + 1
  } # end conditional for non-null overlap object
} # end iterating over predictors
