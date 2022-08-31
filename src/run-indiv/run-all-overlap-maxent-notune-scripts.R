# Run all overlap raster scripts for MaxEnt (no tune) models
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-08-05

require(parallel)

logfile <- "logs/overlap-maxent-notune-out.log"
remove_log <- FALSE

script_files <- list.files(path = "./src/indiv",
                           pattern = "*-overlap-maxent-notune.R",
                           full.names = TRUE)

# For testing, subset this vector
# script_files <- script_files[1:12]

script_file_list <- as.list(script_files)

# Function we'll use to parallelize the process
run_scripts <- function(script_name,
                        log_file) {

  # Will hold message for log file
  message_out <- ""
  
  if (file.exists(script_name)) {
    write(x = paste0("About to run ", script_name),
          file = log_file,
          append = TRUE)
    message(paste0("About to run ", script_name))
    source(file = script_name)
    message_out <- paste0("Finished running script: ", script_name)
    message(message_out)
  } else {
    message_out <- paste0("Could not find script: ", script_name)
    warning(message_out)
    
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
                         X = script_file_list,
                         fun = run_scripts,
                         log_file = logfile)
stopCluster(cl = clust)

if (remove_log && file.exists(logfile)) {
  file.remove(logfile)
}
