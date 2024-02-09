# Run all cross-validation scripts
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2024-02-08

# IN DEVELOPMENT. Not ready for use.

# TODO: Update this script to call the run_one_CV function for each species, 
# once that function is actually written. Rationale for this is to allow us to 
# pass information about whether or not to re-run analyses for individual 
# species/SDM combinations.

require(parallel)
source(file = "load_functions.R")

# Re-run script if the model output already exists?
rerun <- TRUE
# Run SDMs for all species? If FALSE, just runs rumiko/cresphontes
all_insects <- FALSE
# Integer for the maximum number of cores to utilize, if NULL, will use n - 2, 
# where n is the number of cores available
max_cores <- 8 # 8

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

# This is where we previously called a separate function, run_all_CV_scripts()

logfile <- paste0("logs/CV-out.log")
# Create log file before evaluating models
f <- file.create(logfile)
# Create hold message for log file
message_out <- ""

# Identify scripts to run
cv_files <- list.files(path = "./src/indiv",
                       pattern = paste0("*-1-CV.R"),
                       full.names = TRUE)

# If not running for all species, identify which species to include
if (!all_insects) {
  insects <- c("Papilio rumiko", "Papilio cresphontes")
  
  # Load insect-host file
  ih <- read.csv("data/insect-host.csv")
  
  plants <- ih$host_accepted[ih$insect %in% insects]
  species <- unique(c(insects, plants))
  nice_names <- tolower(gsub(pattern = " ",
                             replacement = "_",
                             x = species))
  
  # Extract just those cv_files we'll need
  file_index <- NULL
  for (i in 1:length(species)) {
    spp_index <- grep(nice_names[i], cv_files)
    if (length(spp_index) == 0) {
      message_out <- paste0("No CV script for: ", species[i], " (.src/indiv/",
                            nice_names[i], "-1-CV.R)")
      message(message_out)
      # Write message to log file if species CV script doesn't exist
      write(x = message_out,
            file = logfile,
            append = TRUE)
    } else {
      file_index <- c(file_index, spp_index)
    }
  }
  cv_files <- cv_files[file_index]
}

cv_file_list <- as.list(cv_files)

#' Run a single CV script
#' 
#' @param script_name path to the script to run
#' @param log_file path to file for logging information
#' @param rerun logical indicating whether or not to re-run analyses for 
#' species/method combinations that are already on disk
run_CV_script <- function(script_name,
                          log_file,
                          rerun) {
  # Need to extract species name from file to see if model has already been run
  nice_name <- strsplit(x = basename(script_name),
                        split = "-1-CV")[[1]][1]
  
  pa_file <- paste0("data/gbif/presence-absence/",
                    nice_name,
                    "-pa.csv")
  
  if (file.exists(pa_file)) {
    pa_data <- read.csv(file = pa_file)
    
    # File name that would be used for evaluation output
    eval_file <- paste0("output/eval-metrics/", nice_name, "-CVevals.csv")
    
    if (!file.exists(eval_file) | rerun) {
      if (file.exists(script_name)) {
        # Let user know (in log file) what's being run 
        # Note: sometimes these messages overwrite each other, so adding a small
        # system delay to see if we can avoid the problem.
        Sys.sleep(time = runif(n = 1, min = 0 , max = 3))
        write(x = paste0("About to run: ", script_name), 
              file = log_file,
              append = TRUE)
        # Run the actual script
        script_run <- exec_script(script_name = script_name,
                                  log_file = log_file)
        message_out <- paste0("Finished running script: ", script_name)
      } else {
        message_out <- paste0("Could not find script: ", script_name)
      }
    } else {
      message_out <- paste0("Evaluations for: ", nice_name, 
                            " already exists and rerun set to FALSE.")
    }
  } else {
    message_out <- paste0("No data file found for: ", nice_name, " (", 
                          pa_file, ").")
  }
  # Write any output messages to the log file  
  write(x = message_out, 
        file = log_file,
        append = TRUE)
}

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
                         X = cv_file_list,
                         fun = run_CV_script,
                         log_file = logfile,
                         rerun = rerun)
stopCluster(cl = clust)