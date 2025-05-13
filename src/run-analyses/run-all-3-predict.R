# Run all predictions for full models in parallel
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-08-05

require(parallel)
source(file = "load_functions.R")

# Re-run script if the model output already exists?
rerun <- TRUE
# Run SDMs for all species? If FALSE, just runs rumiko/cresphontes and 
# associated host plants
all_insects <- FALSE
# Integer for the maximum number of cores to utilize, if NULL, will use n - 2, 
# where n is the number of cores available
max_cores <- NULL # 8

# If this script is called from bash (e.g. Rscript run-all-...), parse
# arguments and update variables accordingly. e.g. 
# $ Rscript run-all-SDM-<method>-scripts.R -a -f
#    -a: sets all_insects to TRUE
#    -f: sets rerun to FALSE
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  all_insects <- "-a" %in% args
  rerun <- !("-f" %in% args)
}

# Log file to write status to
logfile <- paste0("logs/predict-out.log")
# Create log file before running full SDMs
f <- file.create(logfile)
# Write some stuff at start of log file (as long as it was created successfully)
head_message <- paste0("START: 3-predict. ", Sys.time(), "\n",
                       "all_insects = ", all_insects, "\n",
                       "rerun = ", rerun, "\n")
if (file.exists(logfile)) {
  write(x = head_message, 
        file = logfile,
        append = TRUE)
} else {
  message(head_message)
}

# Load insect-host file
ih <- read.csv("data/insect-host.csv")

# Decide which insects (and associated hosts) to run
if (all_insects) {
  insects <- unique(ih$insect)
} else {
  insects <- c("Papilio rumiko", "Papilio cresphontes")
}

# Extract host plant names for those species of interest
plants <- ih$host_accepted[ih$insect %in% insects]

# Combine insects and plants to a single vector
species_to_run <- unique(c(insects, plants))

#' Run predictions for a single species
#' 
#' @param species_name character scientific name of species to run, e.g. 
#' "Papilio rumiko"
#' @param log_file path to file for logging information
#' @param rerun logical indicating whether or not to re-run model 
#' predictions for \code{species_name} regardless of whether or not files are
#' already on disk
#' 
#' @details
#' This function provides a wrapper that calls \code{run_one_predict} for a 
#' species, for use in parallel processing. The bulk of the function is 
#' error/warning handling via a tryCatch, to prevent a single species' analyses 
#' from bringing the whole thing crashing to a halt.
predict_run <- function(species_name, log_file, rerun) {
  # Only try writing to a log if log file exists
  # Can't figure out how to do this in one step since R has to evaluate 
  # ALL conditions in an if statement...
  write_to_log <- !is.null(log_file)
  if (write_to_log) {
    write_to_log <- file.exists(log_file)
  }
  start_message <- paste0("About to model predictions on ", 
                          species_name, ".")
  if (write_to_log) {
    write(x = start_message,
          file = log_file,
          append = TRUE)
  } else {
    message(start_message)
  }
  # try/catch the function that would run the script; write status to log
  tryCatch(
    {
      # Call the function to make predictions for this species
      complete_message <- run_one_predict(species_name = species_name,
                                          rerun = rerun)
      if (write_to_log) {
        write(x = complete_message, 
              file = log_file,
              append = TRUE)
      } else {
        message(complete_message)
      }
    },
    # Handle errors (write to log)
    error = function(e) {
      error_message <- paste0("Error while making predictions for ", 
                              species_name, ": ", e)
      if (write_to_log) {
        write(x = error_message, 
              file = log_file,
              append = TRUE)
      } else { # No log file, so just send to stdout
        message(error_message)
      }
      return(error_message)
    }, # End error function
    # Handle warnings (write to log)
    warning_f = function(w) {
      warning_message <- paste0("Warning while making predictions for ", 
                                species_name, ": ", w)
      if (write_to_log) {
        write(x = warning_message, 
              file = log_file,
              append = TRUE)
      } else { # No log file, so just send to stdout
        message(warning_message)
      }
      return(warning_message)
    } # End warning function
  ) # end tryCatch
} # end predict_run

# For parallel processing, do two fewer cores or max (whichever is lower)
num_cores <- parallel::detectCores() - 2
if (!is.null(max_cores)) {
  if (num_cores > max_cores) {
    num_cores <- max_cores
  }
}
clust <- parallel::makeCluster(num_cores)

# Need to make some other scripts available on those clusters
invisible(clusterEvalQ(cl = clust,
                       expr = {
                         source(file = "load_functions.R")
                       }))

# Run each script in parallel
r <- parallel::parLapply(cl = clust,
                         X = species_to_run,
                         fun = predict_run,
                         log_file = logfile,
                         rerun = rerun)
stopCluster(cl = clust)

# Final closing message to log
tail_message <- paste0("END: 3-predict. ", Sys.time())
if (file.exists(logfile)) {
  write(x = tail_message, 
        file = logfile,
        append = TRUE)
} else {
  message(tail_message)
}
