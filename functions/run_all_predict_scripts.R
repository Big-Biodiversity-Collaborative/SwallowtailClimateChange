#' Run all individual species prediction scripts
#' 
#' @param rerun logical indicating whether or not to re-run analyses for 
#' species/method combinations that are already on disk
#' @param all_insects logical indicating whether to run analyses for all 
#' insects and their associated hostplants (\code{all_insects = TRUE}) or for 
#' only a subset of species (\emph{P. rumiko} and \emph{P. cresphontes}).
#' @param max_cores integer indicating the maximum number of cores to use for 
#' analyses (if running in parallel). The default value (\code{NULL}) will use 
#' n - 2, where n is the number of cores available
#' 
#' @return logical indicating whether all scripts ran without issue 
#' (\code{TRUE}) or not (\code{FALSE}).
run_all_predict_scripts <- function(rerun = TRUE, all_insects = TRUE, 
                                    max_cores = NULL) {
  # Extract the name of this function for reporting
  function_name <- as.character(match.call())[1]
  
  if (!require(parallel)) {
    stop(function_name, "requires parallel package, but it could not be loaded")
  }

  # For parallel processing, do two fewer cores or max (whichever is lower)
  num_cores <- parallel::detectCores() - 2
  if (!is.null(max_cores)) {
    if (num_cores > max_cores) {
      num_cores <- max_cores
    }
  }
  
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
}

#' Run a single distribution script
#' 
#' @param script_name path to the script to run
#' @param log_file path to file for logging information
#' @param rerun logical indicating whether or not to re-run analyses for 
#' species/method combinations that are already on disk

# Function we'll use to parallelize the process
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