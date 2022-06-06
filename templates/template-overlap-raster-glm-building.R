# A template for building overlap rasters for an insect species & hosts from GLM
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-07-08

source(file = "load_functions.R")

genus <- "GENUS"
species <- "SPECIES"

method <- "glm"

# Name for reporting
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

message(paste0("Creating overlap rasters based on ", toupper(method), 
               " for ", species_name))

# For each of the global climate change models, want to create the overlap 
# raster then save it to a file. 
climate_models <- read.csv(file = "data/climate-models.csv")
model_names <- climate_models$name

for (model_name in model_names) {
  overlap <- overlap_raster(species_name = species_name,
                            predictor = model_name,
                            model = method)
  
  # As long as there is something there, write to file
  if (!is.null(overlap)) {
    overlap_file <- paste0("output/ranges/",
                           nice_name, 
                           "-overlap-",
                           method, 
                           "-",
                           model_name, 
                           ".rds")
    saveRDS(object = overlap, 
            file = overlap_file)
  } # end conditional for non-null overlap object
} # end iterating over predictors
