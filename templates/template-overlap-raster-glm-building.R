# A template for building overlap rasters for an insect species & hosts from GLM
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-07-08

source(file = "load_functions.R")

genus <- "GENUS"
species <- "SPECIES"

model <- "glm"

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
predictors = c("current", "GFDL-ESM4_RCP45")
for (predictor in predictors) {
  overlap <- overlap_raster(species_name = species_name,
                            predictor = predictor,
                            model = model)
  
  # As long as there is something there, write to file
  if (!is.null(overlap)) {
    overlap_file <- paste0("output/ranges/",
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