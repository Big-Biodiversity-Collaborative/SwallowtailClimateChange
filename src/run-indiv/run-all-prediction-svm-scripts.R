# Run all presence / absence prediction scripts for SVM models
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-24

# TODO: An open question of whether we want to run *all* the appropriate 
# scripts that are found in the scripts folder (as implemented below) or if 
# we want to use another file (such as data/gbif-reconcile.csv) to dictate 
# which scripts to run...

require(parallel)

logfile <- "logs/prediction-svm-out.log"
remove_log <- FALSE
rerun <- TRUE

pred_scripts <- list.files(path = "./src/indiv",
                         pattern = "*-prediction-svm.R",
                         full.names = TRUE)

# For testing, use only a subset 
# pred_scripts <- pred_scripts[1:12]

pred_script_list <- as.list(pred_scripts)

run_svm_prediction <- function(script_name,
                            log_file,
                            rerun) {
  
  one_file <- script_name
  # Need to extract nice name to find model
  nice_name <- strsplit(x = basename(one_file),
                        split = "-")[[1]][1]
  
  # Make sure model output exists
  model_out <- paste0("output/models/", nice_name, "-model-svm-current.rds")
  if (file.exists(model_out)) {
    # TODO: Seems duplicated with one_file and script_name...
    prediction_script <- paste0("src/indiv/", nice_name, "-prediction-svm.R")
    if (file.exists(prediction_script)) {
      # In this one case, we want to let user know that we are running
      write(x = paste0("About to run ", prediction_script), 
            file = log_file,
            append = TRUE)
      message(paste0("About to run ", prediction_script))
      source(file = prediction_script)
      message_out <- paste0("Finished running script: ", prediction_script)
      message(message_out)
    } else {
      message_out <- paste0("Could not find script: ", prediction_script)
      warning(message_out)
    }
  } else {
    message_out <- paste0("No model found for ", nice_name, 
                          ". Skipping forecast.")
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

# Run the processes in parallel
r <- parallel::mclapply(X = pred_script_list,
                        FUN = run_svm_prediction,
                        mc.cores = num_cores,
                        log_file = logfile,
                        rerun = rerun)

if (remove_log && file.exists(logfile)) {
  file.remove(logfile)
}
