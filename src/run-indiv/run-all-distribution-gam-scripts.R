# Run all prediction scripts for GAM models
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-08-05

require(parallel)

sdm_method <- "gam"

# Whether or not to remove log file after running
remove_log <- FALSE
# Logical indicating whether or not to re-run script if predictions already
# exist
rerun <- TRUE
# Logical indicating whether to run prediction scripts for all species or only a 
# subset of insects and their host plants
all_insects <- FALSE
# If this script is called from bash (e.g. Rscript run-all-...), see if the -a
# flag was set to run all insects; if so, update all_insects to TRUE
# e.g. from command line:
# $ Rscript run-all-SDM-<method>-scripts.R -a
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  all_insects <- args[1] == "-a"
}

# Integer for the maximum number of cores to utilize, if NULL, will use n - 2, 
# where n is the number of cores available
max_cores <- 2 # predictions are pretty memory-intensive, so we're being cautious

# For parallel processing, do two fewer cores or max (whichever is lower)
num_cores <- parallel::detectCores() - 2
if (!is.null(max_cores)) {
  if (num_cores > max_cores) {
    num_cores <- max_cores
  }
}

# Setup log file and write first line to file
logfile <- paste0("logs/distribution-", sdm_method, "-out.log")
write(x = paste0("Start: ", Sys.time(), " on ", num_cores, " processors"),
      file = logfile,
      append = FALSE)

# Create hold message for log file
message_out <- ""

# Identify prediction scripts
pred_scripts <- list.files(path = "./src/indiv",
                         pattern = paste0("*-distribution-", sdm_method, ".R"),
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
      message_out <- paste0("No prediction script for ", species[i], " (",
                            nice_names[i], "-distribution-", sdm_method, ".R)")
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

run_prediction_script <- function(script_name,
                                  log_file,
                                  rerun,
                                  sdm_method) {
  
  # Need to extract nice name
  nice_name <- strsplit(x = basename(script_name),
                        split = "-distribution-")[[1]][1]

  # File name that would be used for SDM model output
  sdm_out <- paste0("output/SDMs/", nice_name, "-", sdm_method, ".rds")
  
  if (file.exists(sdm_out)) {
  
    # See whether predictions have already been made
    pred_out_files <- list.files(path = "output/distributions",
                                 pattern = paste0(nice_name, 
                                                  "-distribution-", 
                                                  sdm_method))
    if(length(pred_out_files) < 5 | rerun) {
      if (file.exists(script_name)) {
        # Let user know (in log file) what's being run 
        # Note: sometimes these messages overwrite each other, so adding a small
        # system delay to see if we can avoid the problem.
        Sys.sleep(time = runif(1, 0, 3))
        write(x = paste0("About to run ", script_name),
              file = log_file,
              append = TRUE)
        source(file = script_name)
        message_out <- paste0("Finished running script: ", script_name)
      } else {
        message_out <- paste0("Could not find script: ", script_name)
      }
    } else {
      message_out <- paste0("Predictions for ", nice_name, 
                            " already exist and rerun set to FALSE.")
    } 
  } else {
    message_out <- paste0("No SDM found for ", nice_name, 
                          ". Skipping prediction.")
  }
  # Write any output messages to the log file  
  write(x = message_out, 
        file = log_file,
        append = TRUE)
}

# Run each script in parallel
clust <- parallel::makeCluster(num_cores)
r <- parallel::parLapply(cl = clust,
                         X = pred_script_list,
                         fun = run_prediction_script,
                         log_file = logfile,
                         rerun = rerun,
                         sdm_method = sdm_method)
stopCluster(cl = clust)

if (remove_log && file.exists(logfile)) {
  file.remove(logfile)
}
