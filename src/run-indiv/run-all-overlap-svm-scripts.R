# Run all overlap raster scripts for SVM
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-07-08

require(parallel)

logfile <- "logs/overlap-svm-out.log"
remove_log <- FALSE

script_files <- list.files(path = "./src/indiv",
                           pattern = "*-overlap-svm.R",
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

# For parallel processing, do two fewer cores or eight (whichever is lower)
num_cores <- detectCores() - 2
if (num_cores > 8) {
  num_cores <- 8
}

# Create that log file before running the parallel processes
f <- file.create(logfile)

r <- parallel::mclapply(X = script_file_list,
                        FUN = run_scripts,
                        mc.cores = num_cores,
                        log_file = logfile)

if (remove_log && file.exists(logfile)) {
  file.remove(logfile)
}
