# Run all forecast distribution-generating scripts
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-03

# TODO: An open question of whether we want to run *all* the appropriate 
# scripts that are found in the scripts folder (as implemented below) or if 
# we want to use another file (such as data/gbif-reconcile.csv) to dictate 
# which scripts to run...

# Logical indicating whether or not to for re-running script if the model 
# output already exists
rerun <- TRUE
dist_files <- list.files(path = "./scripts",
                         pattern = "*-forecast-svm.R",
                         full.names = TRUE)

for (one_file in dist_files) {
  # Need to extract species name from file to see if model has already been run
  nice_name <- strsplit(x = basename(one_file),
                           split = "-")[[1]][1]
  
  # the file name that would be used for distribution output
  # TODO: only checks for one forecast
  dist_out <- paste0("output/distributions/", nice_name, "-distribution-svm-GFDL-ESM4_RCP45.rds")
  
  if (!file.exists(dist_out) | rerun) {
    
    # Make sure model output exists
    model_out <- paste0("output/models/", nice_name, "-model-svm-current.rds")
    if (file.exists(model_out)) {
      forecast_script <- paste0("scripts/", nice_name, "-forecast.R")
      if (file.exists(forecast_script)) {
        source(file = forecast_script)
      } else {
        warning(paste0("\nCould not find script: ", forecast_script))
      }
    } else {
      message(paste0("\nNo model found for ", nice_name, ". Skipping forecast."))
    }
  }
}
