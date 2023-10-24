# Run all prediction scripts for full models
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
max_cores <- NULL # NULL

# For parallel processing, do two fewer cores or max (whichever is lower)
# Normally happens further along, but we use num_cores in reporting to log file
num_cores <- parallel::detectCores() - 2
if (!is.null(max_cores)) {
  if (num_cores > max_cores) {
    num_cores <- max_cores
  }
}

# If this script is called from bash (e.g. Rscript run-all-...), parse
# arguments and update variables accordingly. e.g. 
# $ Rscript run-all-distribution-<method>-scripts.R -a -f
#    -a: sets all_insects to TRUE
#    -f: sets rerun to FALSE
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  all_insects <- "-a" %in% args
  rerun <- !("-f" %in% args)
}

# previously called separate function run_all_predict_scripts() to to the work
logfile <- paste0("logs/predict-out.log")
write(x = paste0("Start time ", Sys.time(), " on ", num_cores, " processors"),
      file = logfile,
      append = FALSE)

# Create hold message for log file
message_out <- ""

# Identify prediction scripts
pred_scripts <- list.files(path = "./src/indiv",
                           pattern = paste0("*-3-predict.R"),
                           full.names = TRUE)

# If not running predictions for all species, identify which insects (and their 
# host plants) to include
if (!all_insects) {
  insects <- c("Papilio rumiko", "Papilio cresphontes")
  
  # Load insect-host file
  ih <- read.csv("data/insect-host.csv")
  
  plants <- ih$host_accepted[ih$insect %in% insects]
  species <- unique(c(insects, plants))
  nice_names <- tolower(gsub(pattern = " ",
                             replacement = "_",
                             x = species))
  
  # Extract just those scripts we'll need
  file_index <- NULL
  for (i in 1:length(species)) {
    spp_index <- grep(nice_names[i], pred_scripts)
    if (length(spp_index) == 0) {
      message_out <- paste0("No prediction script for: ", species[i], " (",
                            nice_names[i], "-3-predict-.R)")
      message(message_out)
      # Write message to log file if species prediction script doesn't exist
      write(x = message_out,
            file = logfile,
            append = TRUE)
    } else {
      file_index <- c(file_index, spp_index)
    }
  }
  pred_scripts <- pred_scripts[file_index]
}

pred_script_list <- as.list(pred_scripts)

#' Run a single distribution script
#' 
#' @param script_name path to the script to run
#' @param log_file path to file for logging information
#' @param rerun logical indicating whether or not to re-run analyses for 
#' species/method combinations that are already on disk
run_predict_script <- function(script_name,
                               log_file,
                               rerun) {
  
  # Need to extract nice name
  nice_name <- strsplit(x = basename(script_name),
                        split = "-3-predict")[[1]][1]
  
  # File name that would be used for SDM model output
  # TODO: Only checks for BRT. Move to *-3-predict.R scripts
  sdm_out <- paste0("output/SDMs/", nice_name, "-brt.rds")
  
  if (file.exists(sdm_out)) {
    
    # See whether predictions have already been made
    pred_out_files <- list.files(path = "output/distributions",
                                 pattern = paste0(nice_name, 
                                                  "-distribution"))
    if(length(pred_out_files) < 5 | rerun) {
      if (file.exists(script_name)) {
        # Let user know (in log file) what's being run 
        # Note: sometimes these messages overwrite each other, so adding a small
        # system delay to see if we can avoid the problem.
        Sys.sleep(time = runif(1, 0, 3))
        write(x = paste0("About to run: ", script_name),
              file = log_file,
              append = TRUE)
        script_run <- exec_script(script_name = script_name,
                                  log_file = log_file)
        message_out <- paste0("Finished running script: ", script_name)
      } else {
        message_out <- paste0("Could not find script: ", script_name)
      }
    } else {
      message_out <- paste0("Predictions for: ", nice_name, 
                            " already exist and rerun set to FALSE.")
    } 
  } else {
    message_out <- paste0("No SDM found for: ", nice_name, 
                          ". Skipping prediction.")
  }
  # Write any output messages to the log file  
  write(x = message_out, 
        file = log_file,
        append = TRUE)
}

# Ready to run; set up cluster
clust <- parallel::makeCluster(num_cores)

# Need to make some other scripts available on those clusters
invisible(clusterEvalQ(cl = clust,
                       expr = {
                         source(file = "load_functions.R")
                       }))

# Run each script in parallel
r <- parallel::parLapply(cl = clust,
                         X = pred_script_list,
                         fun = run_predict_script,
                         log_file = logfile,
                         rerun = rerun)
stopCluster(cl = clust)
