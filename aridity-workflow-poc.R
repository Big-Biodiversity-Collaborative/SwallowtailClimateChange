# Large workflow script for proof of concept with P. rutulus & P. glaucus
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-01-26

# This script should do little else than call *other* scripts to accomplish 
# each step

insect_species <- c("Papilio rutulus", "Papilio glaucus")
model_names <- c("svm", "glm")

########################################
# find hosts
# load file with insect-host associations
insects_hosts <- read.csv(file = "data/insect-host.csv")

# identify all host plants used for each insect
host_species <- insects_hosts$host[insects_hosts$insect %in% insect_species]
rm(insects_hosts)

# drop duplicates, as some insects may share host plant species
host_species <- sort(unique(trimws(host_species)))

########################################
# grab the data
# Data retrieval is done for all species by the script download-data.R. 
# Just a check to ensure data exist, and for plant species, exclude any species 
# from subsequent analyses that lack data files
all_species <- data.frame(category = c(rep(x = "insect", 
                                           times = length(insect_species)),
                                       rep(x = "host", 
                                           times = length(host_species))),
                          species_name = c(insect_species, host_species))
# the "nice_name" column is used for file paths
all_species$nice_name <- tolower(x = gsub(pattern = " ",
                                          replacement = "_",
                                          x = all_species$species_name))


# keeping track of which host plants we lack data for
rows_to_exclude <- integer(0)

message(paste0("Checking data files for ", nrow(all_species), " species."))
for (i in 1:nrow(all_species)) {
  species_name <- all_species$species_name[i]
  nice_name <- all_species$nice_name[i]
  data_filename <- paste0("data/", nice_name, "-gbif.csv")
  if (!file.exists(data_filename)) {
    # If this is a host species, remove it from the list of hosts to include 
    # in subsequent steps
    if (all_species$category[i] == "host") {
      message(paste0("Data file ", data_filename, " for ", species_name,
                     " is missing. Species will be excluded from subsequent steps."))
      rows_to_exclude <- c(rows_to_exclude, i)
    } else {
      warning(paste0("Data file ", data_filename, " for ", species_name,
                     " is missing. Has download-data.R been run locally?"))
    }
  }
}

# Drop any rows that are host plants that lack data
if (length(rows_to_exclude) > 0) {
  all_species <- all_species[-rows_to_exclude, ]
  warning(paste0(length(rows_to_exclude)), " host species missing data file(s).",
          " These species will be excluded from subsequent steps.")
}

########################################
# do any data qa/qc
#     consider restricting bug and plant data temporally...
# Data download restricts observations to those in Canada, Mexico, and the 
# United States. It further discards any observations outside a rough rectangle 
# of North America (latitude: 14-80, longitude: -170, -52). See 
# functions/download_gbif.R for details.

########################################
# build sdm models for all remaining species (insects and plants)
message(paste0("\n*** Estimating SDMs for ", nrow(all_species), " species and ",
               length(model_names), " models."))

# TODO: At least some of this should be parallelized. Over species, probably
for (i in 1:nrow(all_species)) {
  species_name <- all_species$species_name[i]
  nice_name <- all_species$nice_name[i]
  for (model_name in model_names) {
    model_filename <- paste0("scripts/", nice_name, "-model-", model_name, ".R")
    if (!file.exists(model_filename)) {
      warning(paste0("Model file ", model_filename,
                     " is missing. Has build-scripts-model-", model_name, 
                     ".sh been run locally?"))
    } else {
      message(paste0("Running model script ", model_filename))
      # source(file = model_filename)
    }
  }
}
message(paste0("Fininshed estimating SDMs for ", nrow(all_species), 
               " species and ", length(model_names), " models."))

########################################
# do current distributions for bug (all bugs)
# do current distributions for plants (all plants)
# do forecast distributions for bug (all bugs)
# do forecast distributions for plants (all plants)

# At this point we have vectors for insects and plants; as the distributions
# for each are independent (at least from a programming point of view), we can 
# run them all in parallel.
# As currently written, contemporary distributions are created in the same 
# script as forecast distributions ([nice_name]-prediction-[model].R). Those 
# scripts run *all* the forecast climate models

message(paste0("\n*** Predicting distributions for ", nrow(all_species), 
               " species and ", length(model_names), " models."))
missing_predictions <- integer(0)
for (i in 1:nrow(all_species)) {
  species_name <- all_species$species_name[i]
  nice_name <- all_species$nice_name[i]
  for (model_name in model_names) {
    prediction_filename <- paste0("scripts/", nice_name, "-prediction-",
                                  model_name, ".R")
    if (!file.exists(prediction_filename)) {
      warning(paste0("Prediction file ", prediction_filename,
                     " is missing. Has build-prediction-", model_name, 
                     "-files.sh been run locally?"))
      missing_predictions <- c(missing_predictions, i)
    } else {
      message(paste0("Running prediction script ", prediction_filename))
      # source(file = prediction_filename)
    }
  }
}
message(paste0("Finished predicting distributions for ", 
               nrow(all_species) - length(missing_predictions), 
               " species and ", length(model_names), " models."))
if (length(missing_predictions > 0)) {
  warning(paste0("Predictions for ", length(missing_predictions), 
                 " species not made, due to missing prediction scripts."))
}

# create restricted current range of bug to areas with >= 1 plant
# create restricted forecast range of bug to areas with >= 1 plant
# calculate change between current and forecast (bugs only)
# calculate change between current and forecast (bug range with plants in mind)
# get aridity data & calculate measurement for a species
# do regression between aridity & percent change