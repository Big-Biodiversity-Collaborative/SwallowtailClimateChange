# Workflow script to test code on other machines
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-05-04

# This script should do little else than call *other* scripts to accomplish 
# each step. Before running _this_ script, the bash scripts to generate R 
# scripts for individual species will need to be run. These three scripts are 
# in the src/bash folder and can be run from a bash command line:
# src/bash/build-scripts-model.sh
# src/bash/build-scripts-overlap-raster.sh
# src/bash/build-scripts-prediction.sh

require(dplyr)
require(ggplot2)
require(tidyr)
require(parallel)
source(file = "load_functions.R")

insect_names <- c("Papilio rumiko")
sdm_names <- c("maxent-notune")

########################################
# extract data
# start by unzipping the data archives that have presence / absence data
unzip(zipfile = "data/gbif-pa.zip")
# and the shapefiles of minimum convex polygons
unzip(zipfile = "data/gbif-shapefiles.zip")

########################################
# find hosts
# load file with insect-host associations
insects_hosts <- read.csv(file = "data/insect-host.csv")

# identify all host plants used for each insect
host_species <- insects_hosts$host_accepted[insects_hosts$insect %in% insect_names]
rm(insects_hosts)

# drop duplicates, as some insects may share host plant species
host_species <- sort(unique(trimws(host_species)))

########################################
# grab the data
# Data retrieval is done for all species by the script download-data.R. 
# This is just a check to ensure data exist, and for plant species, exclude any 
# species from subsequent analyses that lack data files
all_species <- data.frame(category = c(rep(x = "insect", 
                                           times = length(insect_names)),
                                       rep(x = "host", 
                                           times = length(host_species))),
                          species_name = c(insect_names, host_species))
# the "nice_name" column is used for file paths
all_species$nice_name <- tolower(x = gsub(pattern = " ",
                                          replacement = "_",
                                          x = all_species$species_name))

# this will keep track of which host plants we lack data for
rows_to_exclude <- integer(0)

message("Checking data files for ", nrow(all_species), " species.")
for (i in 1:nrow(all_species)) {
  species_name <- all_species$species_name[i]
  nice_name <- all_species$nice_name[i]
  data_filename <- paste0("data/gbif/presence-absence/", nice_name, "-pa.csv")
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
# build sdm models for all remaining species (insects and plants)
message(paste0("\n*** Estimating SDMs for ", nrow(all_species), " species and ",
               length(sdm_names), " models."))

# Set up cluster for parallel processing of SDMs
n <- parallel::detectCores() - 2
clust <- parallel::makeCluster(n)
# Write a short function to use for running individual species scripts
run_sdms <- function(x, sdm_names) {
  nice_name <- x
  message_out <- ""
  for (sdm_name in sdm_names) {
    model_filename <- paste0("src/indiv/", nice_name, "-SDM-", sdm_name, ".R")
    if (!file.exists(model_filename)) {
      message_out <- paste0("Model file ", model_filename,
                            " is missing. Has build-scripts-SDM-", sdm_name, 
                            ".sh been run locally?")
      warning(message_out)
    } else {
      # message(paste0("Running model script ", model_filename))
      source(file = model_filename)
      message_out <- paste0("Model script ", model_filename, " run.")
    }
  }
  return(message_out)
}
s <- parallel::parLapply(cl = clust,
                         X = all_species$nice_name,
                         fun = run_sdms,
                         sdm_names = sdm_names)
# TODO: Could do a better job of reporting problems than just requiring folks 
# to read every element in resultant list...
# unlist(s)
stopCluster(cl = clust)

message(paste0("Finished estimating SDMs for ", nrow(all_species), 
               " species and ", length(sdm_names), " models."))

########################################
# do current distributions for bug (all bugs)
# do current distributions for plants (all plants)
# do forecast distributions for bug (all bugs)
# do forecast distributions for plants (all plants)

# At this point we have vectors for insects and plants; as the distributions
# for each are independent (at least from a programming point of view), we 
# could run them all in parallel. However, keeping the iterative, serial 
# approach because each is pretty RAM-intensive and could bring processes to a 
# crawl.
# As currently written, contemporary distributions are created in the *same* 
# script as forecast distributions ([nice_name]-prediction-[model].R). Those 
# scripts run *all* the forecast climate models

message(paste0("\n*** Predicting distributions for ", nrow(all_species), 
               " species and ", length(sdm_names), " models."))

missing_predictions <- integer(0)
for (i in 1:nrow(all_species)) {
  species_name <- all_species$species_name[i]
  nice_name <- all_species$nice_name[i]
  for (sdm_name in sdm_names) {
    prediction_filename <- paste0("src/indiv/", nice_name, "-prediction-",
                                  sdm_name, ".R")
    if (!file.exists(prediction_filename)) {
      warning(paste0("Prediction file ", prediction_filename,
                     " is missing. Has build-prediction-", sdm_name, 
                     "-files.sh been run locally?"))
      missing_predictions <- c(missing_predictions, i)
    } else {
      # message(paste0("Running prediction script ", prediction_filename))
      source(file = prediction_filename)
    }
  }
}
message(paste0("Finished predicting distributions for ", 
               nrow(all_species) - length(missing_predictions), 
               " species and ", length(sdm_names), " models."))
if (length(missing_predictions > 0)) {
  warning(paste0("Predictions for ", length(missing_predictions), 
                 " species not made, due to missing prediction scripts."))
}

########################################
# create restricted current range of bug to areas with >= 1 plant
# create restricted forecast range of bug to areas with >= 1 plant

# These are called "overlaps" and the output is a raster with four values (see
# functions/overlap_raster.R for meanings of each value). As currently written, 
# contemporary distributions are created in the *same* script as forecast 
# distributions ([nice_name]-overlap-raster-[model].R)

# Subsequent analyses will be insect-based, so we can extract just the insects
# from the data frame of names we have been using
insect_species <- all_species[all_species$category == "insect",]

message(paste0("\n*** Estimating overlaps for ", nrow(insect_species), 
               " species and ", length(sdm_names), " models."))
missing_overlaps <- integer(0)
for (i in 1:nrow(insect_species)) {
  species_name <- insect_species$species_name[i]
  nice_name <- insect_species$nice_name[i]
  for (sdm_name in sdm_names) {
    overlap_filename <- paste0("src/indiv/", nice_name, "-overlap-raster-",
                               sdm_name, ".R")
    if (!file.exists(overlap_filename)) {
      warning(paste0("Overlap raster file ", overlap_filename,
                     " is missing. Has build-overlap-raster-", sdm_name, 
                     "-files.sh been run locally?"))
      missing_overlaps <- c(missing_overlaps, i)
    } else {
      # message(paste0("Running overlap raster script ", overlap_filename))
      source(file = overlap_filename)
    }
  }
}
message(paste0("Finished creating overlap rasters for ", 
               nrow(insect_species) - length(missing_overlaps), 
               " species and ", length(sdm_names), " models."))
if (length(missing_overlaps > 0)) {
  warning(paste0("Overlap rasters for ", length(missing_overlaps), 
                 " species not made, due to missing overlap raster scripts."))
}

########################################
# make maps based on the overlap rasters

climate_models <- read.csv(file = "data/climate-models.csv")
predictors = climate_models$name

for (i in 1:nrow(insect_species)) {
  species_name <- insect_species$species_name[i]
  nice_name <- insect_species$nice_name[i]
  for (sdm_name in sdm_names) {
    for (predictor in predictors) {
      one_map <- overlap_map(species_name = species_name,
                             predictor = predictor,
                             model = sdm_name, 
                             crop_to_insect = TRUE)
      # Write to file if not null
      if (!is.null(one_map)) {
        mapfile <- paste0("output/maps/",
                          nice_name, 
                          "-overlap-",
                          sdm_name, 
                          "-",
                          predictor, 
                          ".png")
        ggsave(filename = mapfile,
               plot = one_map)
      }
    }
  }
}
