# Run all forecast distribution-generating scripts for GLM models
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-03

# TODO: An open question of whether we want to run *all* the appropriate 
# scripts that are found in the scripts folder (as implemented below) or if 
# we want to use another file (such as data/gbif-reconcile.csv) to dictate 
# which scripts to run...

require(parallel)

logfile <- "logs/forecast-glm-out.log"
remove_log <- FALSE

# Logical indicating whether or not to for re-running script if the model 
# output already exists
rerun <- TRUE

dist_files <- list.files(path = "./scripts",
                         pattern = "*-forecast-glm.R",
                         full.names = TRUE)

# For testing, use only a subset 
# dist_files <- dist_files[1:12]

dist_file_list <- as.list(dist_files)

run_glm_forcast <- function(script_name,
                            log_file,
                            rerun) {
  
  one_file <- script_name
  # Need to extract species name from file to see if model has already been run
  nice_name <- strsplit(x = basename(one_file),
                        split = "-")[[1]][1]
  
  # the file name that would be used for distribution output
  # TODO: only checks for one forecast
  dist_out <- paste0("output/distributions/", nice_name, "-distribution-glm-GFDL-ESM4_RCP45.rds")
  
  if (!file.exists(dist_out) | rerun) {
    
    # Make sure model output exists
    model_out <- paste0("output/models/", nice_name, "-model-glm-current.rds")
    if (file.exists(model_out)) {
      forecast_script <- paste0("scripts/", nice_name, "-forecast-glm.R")
      if (file.exists(forecast_script)) {
        # In this one case, we want to let user know that we are running
        write(x = paste0("About to run ", forecast_script), 
              file = log_file,
              append = TRUE)
        message(paste0("About to run ", forecast_script))
        source(file = forecast_script)
        message_out <- paste0("Finished running script: ", forecast_script)
        message(message_out)
      } else {
        message_out <- paste0("Could not find script: ", forecast_script)
        warning(message_out)
      }
    } else {
      message_out <- paste0("No model found for ", nice_name, 
                            ". Skipping forecast.")
      message(message_out)
    }
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

r <- parallel::mclapply(X = dist_file_list,
                        FUN = run_glm_forcast,
                        mc.cores = num_cores,
                        log_file = logfile,
                        rerun = rerun)

if (remove_log && file.exists(logfile)) {
  file.remove(logfile)
}


################################################################################
# Serial implementation

# for (one_file in dist_files) {
#   # Need to extract species name from file to see if model has already been run
#   nice_name <- strsplit(x = basename(one_file),
#                            split = "-")[[1]][1]
#   
#   # the file name that would be used for distribution output
#   # TODO: only checks for one forecast
#   dist_out <- paste0("output/distributions/", nice_name, "-distribution-glm-GFDL-ESM4_RCP45.rds")
#   
#   if (!file.exists(dist_out) | rerun) {
#     
#     # Make sure model output exists
#     model_out <- paste0("output/models/", nice_name, "-model-glm-current.rds")
#     if (file.exists(model_out)) {
#       forecast_script <- paste0("scripts/", nice_name, "-forecast-glm.R")
#       if (file.exists(forecast_script)) {
#         source(file = forecast_script)
#       } else {
#         warning(paste0("\nCould not find script: ", forecast_script))
#       }
#     } else {
#       message(paste0("\nNo model found for ", nice_name, ". Skipping forecast."))
#     }
#   }
# }
