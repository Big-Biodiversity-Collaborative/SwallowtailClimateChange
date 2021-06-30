# Create maps from GLM predictions for each insect species and its hosts
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-29

require(ggplot2)

model <- "glm"
logfile <- "logs/maps-glm-out.log"
file_ext <- "png" # "pdf"

# Load up the functions from the functions folder
function_files <- list.files(path = "./functions", 
                             pattern = ".R$", 
                             full.names = TRUE)
for(fun_file in function_files) {
  source(file = fun_file)
}

# Function we use with mclapply to build overlap rasters in parallel
overlap_maps_glm <- function(species_name, 
                             model,
                             logfile,
                             predictors = c("current", "GFDL-ESM4_RCP45")) {
  # Have species name
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species_name))
  
  # Number of maps created for this species (for reporting)
  num_maps <- 0
  
  # For each of the predictors, want to create the map then save it 
  # to a file.
  for (predictor in predictors) {
    one_map <- overlap_map(species_name = species_name,
                           predictor = predictor,
                           model = model, 
                           crop_to_insect = TRUE)
    
    # Write to file if not null
    if (!is.null(one_map)) {
      mapfile <- paste0("output/maps/",
                        nice_name, 
                        "-overlap-",
                        model, 
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
output_file <- paste0("output/overlaps/", model, "-overlaps.csv")

# identify unique species of insects
insect_species <- unique(insects_hosts$insect)
insect_species_list <- as.list(insect_species)

# For parallel processing, do two fewer cores or eight (whichever is lower)
num_cores <- parallel::detectCores() - 2
if (num_cores > 8) {
  num_cores <- 8
}

# Create that log file before running the parallel processes
f <- file.create(logfile)

r <- parallel::mclapply(X = insect_species_list,
                        FUN = overlap_maps_glm,
                        mc.cores = num_cores,
                        model = model,
                        logfile = logfile)
