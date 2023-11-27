# Run all full model estimation scripts
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-08-05

require(parallel)
source(file = "load_functions.R")

# Re-run script if the model output already exists?
rerun <- TRUE
# Run SDMs for all species? If FALSE, just runs rumiko/cresphontes
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

# Previously called separate function run_all_SDMs_full_scripts() to do the work
logfile <- paste0("logs/SDMs-full-out.log")
# Create log file before running full SDMs
f <- file.create(logfile)
# Create hold message for log file
message_out <- ""

# Identify scripts to run
sdm_files <- list.files(path = "./src/indiv",
                        pattern = paste0("*-2-SDMs-full.R"),
                        full.names = TRUE)

# If not running SDMs for all species, identify which species to include
if (!all_insects) {
  insects <- c("Papilio rumiko", "Papilio cresphontes")
  
  # Load insect-host file
  ih <- read.csv("data/insect-host.csv")
  
  plants <- ih$host_accepted[ih$insect %in% insects]
  species <- unique(c(insects, plants))
  nice_names <- tolower(gsub(pattern = " ",
                             replacement = "_",
                             x = species))
  
  # Extract just those sdm_files we'll need
  file_index <- NULL
  for (i in 1:length(species)) {
    spp_index <- grep(nice_names[i], sdm_files)
    if (length(spp_index) == 0) {
      message_out <- paste0("No SDM script for: ", species[i], " (.src/indiv/",
                            nice_names[i], "-2-SDMs-full.R)")
      message(message_out)
      # Write message to log file if species SDM script doesn't exist
      write(x = message_out,
            file = logfile,
            append = TRUE)
    } else {
      file_index <- c(file_index, spp_index)
    }
  }
  sdm_files <- sdm_files[file_index]
}

sdm_file_list <- as.list(sdm_files)

#' Run a single SDM script
#' 
#' @param script_name path to the script to run
#' @param log_file path to file for logging information
#' @param rerun logical indicating whether or not to re-run analyses for 
#' species/method combinations that are already on disk
run_SDMs_full_script <- function(script_name,
                                 log_file,
                                 rerun) {
  sdm_script <- script_name
  
  # Need to extract species name from file to see if model has already been run
  nice_name <- strsplit(x = basename(sdm_script),
                        split = "-2-SDMs-full")[[1]][1]
  
  pa_file <- paste0("data/gbif/presence-absence/",
                    nice_name,
                    "-pa.csv")
  
  if (file.exists(pa_file)) {
    pa_data <- read.csv(file = pa_file)
    
    # See if all output files are on disk; if *any* are missing, re-run them 
    # all if rerun is set to FALSE.
    model_files <- paste0("output/SDMs/", nice_name, "-", 
                          c("brt", "rf", "maxent", "lasso", "gam"),
                          ".rds")
    # If any model output files are missing, this is TRUE
    model_out_missing <- !all(file.exists(model_files))
    
    if (model_out_missing | rerun) {
      if (file.exists(sdm_script)) {
        # Let user know (in log file) what's being run 
        # Note: sometimes these messages overwrite each other, so adding a small
        # system delay to see if we can avoid the problem.
        Sys.sleep(time = runif(1, 0 ,3))
        write(x = paste0("About to run: ", sdm_script), 
              file = log_file,
              append = TRUE)
        # Run the actual script
        script_run <- exec_script(script_name = script_name,
                                  log_file = log_file)
        message_out <- paste0("Finished running script: ", sdm_script)
      } else {
        message_out <- paste0("Could not find script: ", sdm_script)
      }
    } else {
      message_out <- paste0("SDMs for: ", nice_name, 
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
                         X = sdm_file_list,
                         fun = run_SDMs_full_script,
                         log_file = logfile,
                         rerun = rerun)
stopCluster(cl = clust)
