# Create raster from GLM predictions for each insect species and its hosts
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-28

require(parallel)

model <- "glm"
logfile <- "logs/overlaps-glm-out.log"

# Load up the functions from the functions folder
function_files <- list.files(path = "./functions", 
                             pattern = ".R$", 
                             full.names = TRUE)
for(fun_file in function_files) {
  source(file = fun_file)
}

# Function we use with mclapply to build overlap rasters in parallel
overlap_raster_glm <- function(species_name, 
                            model,
                            logfile,
                            predictors = c("current", "GFDL-ESM4_RCP45")) {
  # Have species name
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species_name))

  # Number of overlap rasters created for this species (for reporting)
  num_overlaps <- 0
  
  # For each of the predictors, want to create the overlap raster then save it 
  # to a file. Could be vectorized. Later.
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
  message_out <- paste0("Wrote ", num_overlaps, " overlap raster(s) for ", 
                        species_name, ".")
  
  # Write any output messages to the log file  
  write(x = message_out,
        file = logfile,
        append = TRUE)
  
} # end function operating on single species

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
                        FUN = overlap_raster_glm,
                        mc.cores = num_cores,
                        model = model,
                        logfile = logfile)
