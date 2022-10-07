# Create maps from MaxEnt predictions for each insect species and its hosts
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-08-05

require(ggplot2)
require(parallel)

sdm_method <- "maxent-notune"
file_ext <- "png" # "pdf"

logfile <- paste0("logs/maps-", sdm_method, "-out.log")

# Function we use with mclapply to build overlap rasters in parallel
overlap_maps <- function(species_name, 
                         sdm_method,
                         predictors,
                         logfile,
                         file_ext) {
  
  # Load up the functions from the functions folder, really just needed to load
  # the overlap_map function; have to do this because of they way parLapply 
  # works
  source(file = "load_functions.R")

  # Make the computer-friendly name
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species_name))
  
  # Number of maps created for this species (for reporting)
  num_maps <- 0
  
  # For each of the climate models, want to create the map then save it to a 
  # file.
  for (predictor in predictors) {
    one_map <- overlap_map(species_name = species_name,
                           predictor = predictor,
                           sdm_method = sdm_method, 
                           crop_to_insect = TRUE,
                           include_legend = TRUE,
                           horizontal_legend = TRUE,
                           generic_legend = TRUE,
                           title_scenarioyear = FALSE)
    
    # Write to file if not null
    if (!is.null(one_map)) {
      mapfile <- paste0("output/maps/",
                        nice_name, 
                        "-overlap-",
                        sdm_method, 
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
  
  # Write any output messages to the log file  
  write(x = message_out,
        file = logfile,
        append = TRUE)
}

# Data for identifying insect species
insects_hosts <- read.csv(file = "data/insect-host.csv")

# identify unique species of insects
insect_species <- unique(insects_hosts$insect)
insect_species_list <- as.list(insect_species)

# Iterate over all climate models listed in data/climate-models.csv
climate_models <- read.csv(file = "data/climate-models.csv")
predictors <- climate_models$name

# For parallel processing, do two fewer cores or eight (whichever is lower)
num_cores <- parallel::detectCores() - 2
if (num_cores > 8) {
  num_cores <- 8
}

# Create that log file before running the parallel processes
f <- file.create(logfile)

# Run each script in parallel
clust <- parallel::makeCluster(num_cores)
r <- parallel::parLapply(cl = clust,
                         X = insect_species_list,
                         fun = overlap_maps,
                         sdm_method = sdm_method,
                         predictors = predictors,
                         logfile = logfile,
                         file_ext = file_ext)
parallel::stopCluster(cl = clust)
