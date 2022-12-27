# Run all Maxent (tuned) SDM scripts
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-08-05

require(parallel)

sdm_method <- "maxent-tune"

logfile <- paste0("logs/SDM-", sdm_method, "-out.log")
remove_log <- FALSE

# Create log file before running SDMs
f <- file.create(logfile)
# Create hold message for log file
message_out <- ""

# Logical indicating whether or not to re-run script if the model output already 
# exists
rerun <- TRUE

# Logical indicating whether to run SDMs for all species or only a subset of 
# insects and their host plants
all_insects <- FALSE

# Identify scripts to run SDMs
sdm_files <- list.files(path = "./src/indiv",
                        pattern = paste0("*-SDM-", sdm_method, ".R"),
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
      message_out <- paste0("No SDM script for ", species[i], " (.src/indiv/",
                            nice_names[i], "-SDM-", sdm_method, ".R)")
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

# Function we'll use to parallelize the process
run_sdm_script <- function(script_name,
                           log_file,
                           rerun,
                           sdm_method) {
  sdm_script <- script_name

  # Need to extract species name from file to see if model has already been run
  nice_name <- strsplit(x = basename(sdm_script),
                        split = "-SDM-")[[1]][1]
  
  pa_file <- paste0("data/gbif/presence-absence/",
                     nice_name,
                     "-pa.csv")

  if (file.exists(pa_file)) {
    pa_data <- read.csv(file = pa_file)

    # File name that would be used for model output
    model_out <- paste0("output/SDMs/", nice_name, "-", sdm_method, ".rds")
      
    if (!file.exists(model_out) | rerun) {
      if (file.exists(sdm_script)) {
        # Let user know (in log file) what's being run 
        # Note: sometimes these messages overwrite each other, so adding a small
        # system delay to see if we can avoid the problem.
        Sys.sleep(time = runif(1, 0 ,3))
        write(x = paste0("About to run ", sdm_script), 
              file = log_file,
              append = TRUE)
        # Run the actual script
        source(file = sdm_script)
        message_out <- paste0("Finished running script: ", sdm_script)
      } else {
        message_out <- paste0("Could not find script: ", sdm_script)
      }
    } else {
      message_out <- paste0("SDM for ", nice_name, 
                            " already exists and rerun set to FALSE.")
    }
  } else {
    message_out <- paste0("No data file found for ", nice_name, " (", 
                          pa_file, ").")
  }

  # Write any output messages to the log file  
  write(x = message_out, 
        file = log_file,
        append = TRUE)
}

# For parallel processing, do two fewer cores or eight (whichever is lower)
num_cores <- parallel::detectCores() - 2
if (num_cores > 8) {
  num_cores <- 8
}
clust <- parallel::makeCluster(num_cores)

# Run each script in parallel
r <- parallel::parLapply(cl = clust,
                         X = sdm_file_list,
                         fun = run_sdm_script,
                         log_file = logfile,
                         rerun = rerun,
                         sdm_method = sdm_method)
stopCluster(cl = clust)

if (remove_log && file.exists(logfile)) {
  file.remove(logfile)
}
