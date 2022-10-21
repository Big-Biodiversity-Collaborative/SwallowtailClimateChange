# Run all MaxEnt (no tune) SDM scripts
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-08-05

require(parallel)

sdm_method <- "brt"

logfile <- paste0("logs/SDM-", sdm_method, "-out.log")
remove_log <- FALSE

# Logical indicating whether or not to re-run script if the model output already 
# exists
rerun <- TRUE

sdm_files <- list.files(path = "./src/indiv",
                        pattern = paste0("*-SDM-", sdm_method, ".R"),
                        full.names = TRUE)

# For testing, subset this vector
# sdm_files <- sdm_files[1:2]

sdm_file_list <- as.list(sdm_files)

# Function we'll use to parallelize the process
run_sdm_script <- function(script_name,
                           log_file,
                           rerun,
                           sdm_method) {
  sdm_script <- script_name

  # Need to extract species name from file to see if model has already been run
  nice_name <- strsplit(x = basename(sdm_script),
                        split = "-")[[1]][1]
  
  pa_file <- paste0("data/gbif/presence-absence/",
                     nice_name,
                     "-pa.csv")
  
  # Will hold message for log file
  message_out <- ""
  if (file.exists(pa_file)) {
    pa_data <- read.csv(file = pa_file)

    # the file name that would be used for model output
    model_out <- paste0("output/SDMs/", nice_name, "-", sdm_method, ".rds")
      
    if (!file.exists(model_out) | rerun) {
      if (file.exists(sdm_script)) {
        # In this one case, we want to let user know that we are running
        write(x = paste0("About to run ", sdm_script), 
              file = log_file,
              append = TRUE)
        message(paste0("About to run ", sdm_script))
        # Run the actual script
        source(file = sdm_script)
        message_out <- paste0("Finished running script: ", sdm_script)
        message(message_out)
      } else {
        message_out <- paste0("Could not find script: ", sdm_script)
        warning(message_out)
      }
    } else {
      message_out <- paste0("SDM already exists and rerun set to FALSE.")
      message(message_out)
    }
  } else {
    message_out <- paste0("No data file found for ", nice_name, " (", 
                          pa_file, ").")
    message(message_out)
  }

  # Write any output messages to the log file  
  write(x = message_out, 
        file = log_file,
        append = TRUE)
}

# Create that log file before running the parallel processes
f <- file.create(logfile)

# For parallel processing, do two fewer cores or eight (whichever is lower)
num_cores <- parallel::detectCores() - 2
if (num_cores > 8) {
  num_cores <- 8
}
clust <- parallel::makeCluster(num_cores)

# Run each script in parallel
r <- parallel::parLapply(cl = clust,
                         X = sdm_file_list,
                         fun = run_sdm_script,
                         log_file = logfile,
                         rerun = rerun,
                         sdm_method = sdm_method)
stopCluster(cl = clust)

if (remove_log && file.exists(logfile)) {
  file.remove(logfile)
}
