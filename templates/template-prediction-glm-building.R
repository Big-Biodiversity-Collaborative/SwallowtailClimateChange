# A template for building predicted distributions for a single species from GLM
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-02

require(raster)

# Load up the functions from the functions folder
function_files <- list.files(path = "./functions", 
                             pattern = ".R$", 
                             full.names = TRUE)
for(fun_file in function_files) {
  source(file = fun_file)
}

genus <- "GENUS"
species <- "SPECIES"

# Name for reporting
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

message(paste0("Predicting presence / absence  based on GLM for ", species_name))
# To keep track if any returned presence / absence rasters were NULL
success <- TRUE 

################################################################################
# Start by doing prediction for current conditions (worldclim data)
current_predictors <- raster::stack(list.files(path = "data/wc2-5",
                                               pattern = ".bil$",
                                               full.names = TRUE))

# Estimate presence / absence
current_pa <- predict_pa(nice_name = nice_name,
                         model = "glm",
                         predictors = current_predictors)

if (!is.null(current_pa)) {
  # Save presence / absence raster
  current_pa_file <- paste0("output/distributions/", nice_name,
                            "-distribution-glm-current.rds")
  saveRDS(object = current_pa,
          file = current_pa_file)
} else {
  success <- FALSE
}

################################################################################
# Do forecast model, with GFDL-ESM4_RCP45 as forecast data
# Load forecast data, download if it does not exist
# For now, want to suppress warning messages about Discarded datum unknown...
suppressWarnings({
  gfdl_data <- raster::getData(name = "CMIP5",
                               var = "bio",
                               res = 2.5,
                               rcp = 45,
                               model = "GD",
                               year = 70,
                               path = "data/")
})

# Need to rename variables in forecast climate data so our predictions work
# (these are the same names as the bioclim data, used for the creation of our
# species distribution model)
names(gfdl_data) <- paste0("bio", 1:19)

# Estimate presence / absence
forecast_pa <- predict_pa(nice_name = nice_name,
                         model = "glm",
                         predictors = gfdl_data)

if (!is.null(forecast_pa)) {
  # Save presence / absence raster
  forecast_pa_file <- paste0("output/distributions/", nice_name,
                             "-distribution-glm-GFDL-ESM4_RCP45.rds")
  saveRDS(object = forecast_pa,
          file = forecast_pa_file)
} else {
  success <- FALSE
}

################################################################################
# Reporting
completion_message <- paste0("Prediction process based on GLM for ", 
                             species_name, " complete.")
if (!success) {
  completion_message <- paste0(completion_message, 
                               " But one or more rasters were null; check for warnings.")
}
message(completion_message)
