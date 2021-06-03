# Run all SDM scripts
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-03

# TODO: An open question of whether we want to run *all* the appropriate 
# scripts that are found in the scripts folder (as implemented below) or if 
# we want to use another file (such as data/gbif-reconcile.csv) to dictate 
# which scripts to run...

# Logical indicating whether or not to for re-running script if the model 
# output already exists
rerun <- FALSE
sdm_files <- list.files(path = "./scripts",
                        pattern = "*-sdm.R",
                        full.names = TRUE)

# TODO: Will need to update file checks if multiple model types end up being 
# run (e.g. random forest, bioclim, etc); current code just checks for support
# vector machine
for (one_file in sdm_files) {
  # Need to extract species name from file to see if model has already been run
  species_name <- strsplit(x = basename(one_file),
                           split = "-")[[1]][1]
  
  # the file name that would be used for model output
  model_out <- paste0("output/models/", species_name, "-model-svm-current.rds")
  
  if (!file.exists(model_out) | rerun) {
    source(file = one_file)
  }
}
