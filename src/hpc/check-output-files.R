# Check for completion in output/distributions and output/suitabilities
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-05-27

# NOTE: Currently built to run on a local machine *after* the output files have 
# been transferred from HPC to local computer (likely via another cloud option, 
# like OneDrive).

# Want to know if analyses run on the HPC created all the distribution and 
# suitabilities files for species with enough data.

# Find out which species have a presence/absence file (the criteria for 
# estimating models and making predictions)
pa_files <- list.files(path = "data/gbif/presence-absence", 
                       pattern = "*-pa.csv$")
nice_names <- gsub(pattern = "-pa.csv$",
                   replacement = "", 
                   x = pa_files)

# We just need the name of the climate model, so pull out that column (it is 
# used as part of the file name for suitabilities and distributions)
climate_models <- read.csv(file = "data/climate-models.csv")[, "name"]

# Find any species that are missing model/scenario combinations for 
# suitabilities
# 12 = one contemporary ensemble + (two time periods x three scenarios) 
#      + five individual model suitabilities for contemporary climate
sdms <- c("brt", "gam", "lasso", "maxent", "rf")
# Calling this a "tail" for end of file names
suitabilities_tails <- c(sdms, climate_models)

# need a logical matrix of Species x file tails to see what is missing?
suitabilities_mat <- vapply(X = suitabilities_tails,
                            FUN = function(x, cols = nice_names){
                              species_files <- paste0("output/suitabilities/",
                                                      cols, "-", x, ".rds")
                              return(file.exists(species_files))
                            },
                            FUN.VALUE = logical(length = length(nice_names)))
rownames(suitabilities_mat) <- nice_names
colnames(suitabilities_mat) <- suitabilities_tails

# Now just pull out those with any FALSE in the row
suitabilities_missing <- suitabilities_mat[rowSums(x = suitabilities_mat) < length(suitabilities_tails), ]

# Find which, if any, species are missing any of the seven distribution files
# 7 = one contemporary + (two time periods x three scenarios)
distributions_tails <- climate_models

# Logical matrix of Species x file tails to see if file exists
distributions_mat <- vapply(X = distributions_tails,
                            FUN = function(x, cols = nice_names){
                              species_files <- paste0("output/distributions/",
                                                      cols, "-", x, ".rds")
                              return(file.exists(species_files))
                            },
                            FUN.VALUE = logical(length = length(nice_names)))
rownames(distributions_mat) <- nice_names
colnames(distributions_mat) <- distributions_tails

# Now just pull out those with any FALSE in the row
distributions_missing <- distributions_mat[rowSums(x = distributions_mat) < length(distributions_tails), ]
