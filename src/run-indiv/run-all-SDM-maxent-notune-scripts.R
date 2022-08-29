# Run all MaxEnt (no tune) SDM scripts
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-08-05

# TODO: An open question of whether we want to run *all* the appropriate 
# scripts that are found in the scripts folder (as implemented below) or if 
# we want to use another file (such as data/gbif-reconcile.csv) to dictate 
# which scripts to run...

require(parallel)

logfile <- "logs/SDM-maxent-notune-out.log"
remove_log <- FALSE

# Logical indicating whether or not to for re-running script if the model 
# output already exists
rerun <- TRUE

# Integer indicating minimum number of observations necessary to run script
min_obs <- 50

maxent_files <- list.files(path = "./src/indiv",
                           pattern = "*-SDM-maxent-notune.R",
                           full.names = TRUE)

# For testing, subset this vector
# maxent_files <- maxent_files[1:12]

maxent_file_list <- as.list(maxent_files)

# Function we'll use to parallelize the process
run_maxent_script <- function(script_name,
                              log_file,
                              rerun, min_obs) {
  maxent_script <- script_name

  # Need to extract species name from file to see if model has already been run
  nice_name <- strsplit(x = basename(maxent_script),
                        split = "-")[[1]][1]
  
  # Need to count the number of observations in data file to see if it meets 
  # minimums
  pa_file <- paste0("data/gbif/presence-absence/",
                     nice_name,
                     "-pa.csv")
  
  # Will hold message for log file
  message_out <- ""
  if (file.exists(obs_file)) {
    pa_data <- read.csv(file = obs_file)
    n_obs <- nrow(pa_data[pa_data$pa == 1, ])
    
    if (n_obs >= min_obs) {
      # the file name that would be used for model output
      model_out <- paste0("output/SDMs/", nice_name, "-maxent-notune.rds")
      
      if (!file.exists(model_out) | rerun) {
        if (file.exists(maxent_script)) {
          # In this one case, we want to let user know that we are running
          write(x = paste0("About to run ", maxent_script), 
                file = log_file,
                append = TRUE)
          message(paste0("About to run ", maxent_script))
          # Run the actual script
          source(file = maxent_script)
          message_out <- paste0("Finished running script: ", maxent_script)
          message(message_out)
        } else {
          message_out <- paste0("Could not find script: ", maxent_script)
          warning(message_out)
        }
      }
    } else {
      message_out <- paste0("Too few observations for ", nice_name, " (", 
                            n_obs, " < ", min_obs, "). Skipping modeling.")
      message(message_out)
    }
  } else {
    message_out <- paste0("No data file found for ", nice_name, " (", 
                          obs_file, ").")
    message(message_out)
  }

  # Write any output messages to the log file  
  write(x = message_out, 
        file = log_file,
        append = TRUE)
}

# For parallel processing, do two fewer cores or eight (whichever is lower)
num_cores <- detectCores() - 2
if (num_cores > 8) {
  num_cores <- 8
}

# Create that log file before running the parallel processes
f <- file.create(logfile)

r <- parallel::mclapply(X = maxent_file_list,
                        FUN = run_maxent_script,
                        mc.cores = num_cores,
                        log_file = logfile,
                        rerun = rerun,
                        min_obs = min_obs)

if (remove_log && file.exists(logfile)) {
  file.remove(logfile)
}

# TODO: After running, should extract any errors that happened...
