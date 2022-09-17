# A template for building overlap rasters for an insect species & hosts from a
# MaxEnt model (no tune)
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-08-05

source(file = "load_functions.R")

genus <- "GENUS"
species <- "SPECIES"

sdm_name <- "maxent-tune"

# Name for reporting
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

message(paste0("Creating overlap rasters based on ", toupper(sdm_name), 
               " for ", species_name))

# For each of the global climate change models, want to create the overlap 
# raster then save it to a file 
climate_models <- read.csv(file = "data/climate-models.csv")
gcm_names <- climate_models$name

for (gcm_name in gcm_names) {
  overlap <- overlap_raster(species_name = species_name,
                            predictor = gcm_name,
                            model = sdm_name)
  
  # As long as there is something there, write to file
  if (!is.null(overlap)) {
    overlap_file <- paste0("output/overlaps/",
                           nice_name, 
                           "-overlap-",
                           sdm_name, 
                           "-",
                           gcm_name, 
                           ".rds")
    saveRDS(object = overlap, 
            file = overlap_file)
  } # end conditional for non-null overlap object
} # end iterating over predictors
