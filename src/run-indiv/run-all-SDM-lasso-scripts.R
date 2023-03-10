# Run all LASSO SDM scripts
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-08-05

source(file = "load_functions.R")
sdm_method <- "lasso"

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

# Call function to do the heavy lifting
run_all_SDM_scripts(sdm_method = sdm_method,
                    rerun = rerun,
                    all_insects = all_insects,
                    max_cores = max_cores)
