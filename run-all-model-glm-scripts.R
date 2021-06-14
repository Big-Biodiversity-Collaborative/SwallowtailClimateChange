# Run all generalized linear model scripts
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-03

# TODO: An open question of whether we want to run *all* the appropriate 
# scripts that are found in the scripts folder (as implemented below) or if 
# we want to use another file (such as data/gbif-reconcile.csv) to dictate 
# which scripts to run...

require(parallel)

logfile <- "logs/model-glm-out.log"
remove_log <- FALSE

# Logical indicating whether or not to for re-running script if the model 
# output already exists
rerun <- TRUE

# Integer indicating minimum number of observations necessary to run script
min_obs <- 50

glm_files <- list.files(path = "./scripts",
                        pattern = "*-model-glm.R",
                        full.names = TRUE)

# For testing, subset this vector
# glm_files <- glm_files[1:12]

glm_file_list <- as.list(glm_files)

# Function we'll use to parallelize the process
run_glm_script <- function(script_name,
                           log_file,
                           rerun, min_obs) {
  one_file <- script_name

  # Need to extract species name from file to see if model has already been run
  nice_name <- strsplit(x = basename(one_file),
                        split = "-")[[1]][1]
  
  # Need to count the number of observations in data file to see if it meets 
  # minimums
  obs_file <- paste0("data/",
                     nice_name,
                     "-gbif.csv")
  
  # Will hold message for log file
  message_out <- ""
  if (file.exists(obs_file)) {
    n_obs <- nrow(x = read.csv(file = obs_file))
    
    if (n_obs >= min_obs) {
      # the file name that would be used for model output
      model_out <- paste0("output/models/", nice_name, "-model-glm-current.rds")
      
      if (!file.exists(model_out) | rerun) {
        glm_script <- paste0("scripts/", nice_name, "-model-glm.R")
        if (file.exists(glm_script)) {
          # In this one case, we want to let user know that we are running
          write(x = paste0("About to run ", glm_script), 
                file = log_file,
                append = TRUE)
          message(paste0("About to run ", glm_script))
          # Run the actual script
          source(file = glm_script)
          message_out <- paste0("Finished running script: ", glm_script)
          message(message_out)
        } else {
          message_out <- paste0("Could not find script: ", glm_script)
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

r <- parallel::mclapply(X = glm_file_list,
                        FUN = run_glm_script,
                        mc.cores = num_cores,
                        log_file = logfile,
                        rerun = rerun,
                        min_obs = min_obs)

if (remove_log && file.exists(logfile)) {
  file.remove(logfile)
}

# TODO: After running, should extract any errors that happened...

################################################################################
# Below here is serial implentation

message("End parallel implementation")

# TODO: Will need to update file checks if multiple model types end up being 
# run (e.g. random forest, bioclim, etc); current code just checks for 
# generalized linear model
# for (one_file in glm_files) {
#   # Need to extract species name from file to see if model has already been run
#   nice_name <- strsplit(x = basename(one_file),
#                            split = "-")[[1]][1]
#   
#   # Need to count the number of observations in data file to see if it meets 
#   # minimums
#   obs_file <- paste0("data/",
#                      nice_name,
#                      "-gbif.csv")
# 
#     
#   if (file.exists(obs_file)) {
#     n_obs <- nrow(x = read.csv(file = obs_file))
#     
#     if (n_obs >= min_obs) {
#       # the file name that would be used for model output
#       model_out <- paste0("output/models/", nice_name, "-model-glm-current.rds")
#       
#       if (!file.exists(model_out) | rerun) {
#         glm_script <- paste0("scripts/", nice_name, "-model-glm.R")
#         if (file.exists(glm_script)) {
#           source(file = glm_script)
#         } else {
#           warning(paste0("\nCould not find script: ", glm_script))
#         }
#       }
#     } else {
#       message(paste0("\nToo few observations for ", nice_name, " (", n_obs, 
#                      " < ", min_obs, "). Skipping modeling."))
#     }
#   } else {
#     message(paste0("\nNo data file found for ", nice_name, " (", obs_file, ")."))
#   }
# }
