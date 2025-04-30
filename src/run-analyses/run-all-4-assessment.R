# Assess model evaluation, estimation, and prediction process
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-03-08

# INCOMPLETE

# Primary goal of the script is to run this on the HPC after the final step 
# (predictions) to assess if all prediction outputs have been created and, if 
# not, to report which outputs are missing. Only considering those species for 
# which a presence / absence file was created.

library(tidyr)
library(dplyr)

# Assessment based on only those taxa with a presence/absence CSV
pa_summary_file <- "data/gbif-pa-summary.csv"
if(!file.exists(pa_summary_file)) {
  warning("No data summary file found; no assessment performed.")
} else {
  # Read in pa summary data; 
  pa_summary <- read.csv(file = pa_summary_file)
  # Filter to only include taxa with enough observations
  pa_summary <- pa_summary %>%
    filter(pa_csv == "yes")
  
  # The compute-friendly name we use for filenames
  nice_names <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = pa_summary$species))
  
  # Check for model evaluation file: output/eval-metrics/<nice_name>-CVevals.csv
  eval_files <- paste0("output/eval-metrics/", nice_names, "-CVevals.csv")
  eval_exists <- file.exists(eval_files)
  
  # Check for model estimation files: output/SDMs/<nice_name>-<sdm>.rds
  # There should be five for each species
  sdms <- c("brt", "gam", "lasso", "maxent", "rf")
  # making a data frame for easier (lazier?) file name creation
  est_df <- data.frame(nice_name = rep(nice_names, times = length(sdms)))
  est_df <- est_df %>%
    dplyr::arrange(nice_name)
  est_df$sdm <- sdms
  est_files <- paste0("output/SDMs/", est_df$nice_name, "-", est_df$sdm, ".rds")
  est_exists <- file.exists(est_files)
  
  # Check for prediction files: output/suitabilities/<nice_name>-<climate>.rds
  # Should be seven files: one for current and six for forecast models; 
  # individual SDM predictions for current climate may or may not be present, 
  # so it is OK if those are missing.
  # Check for prediction files: output/distributions/<nice_name>-<climate>.rds
  # Should be seven files: one for current and six for forecast models.
  climate_models <- read.csv(file = "data/climate-models.csv")
  
  
    
  # After process is complete, report back. Perhaps with this structure:
  # species step output complete
  # Alnus rubra evaluation CVevals TRUE
  # Alnus rubra estimation brt TRUE
  # ...
  # Alnus rubra suitabilities current TRUE
  # Alnus rubra suitabilities ssp245_2041 TRUE
  
  # Could then see if every species has all TRUEs and log those that do not 
  # (writing above table to file regardless)
}