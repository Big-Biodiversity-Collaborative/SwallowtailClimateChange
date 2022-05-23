# Large workflow script for proof of concept with P. rutulus & P. glaucus
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-01-26

# This script should do little else than call *other* scripts to accomplish 
# each step
require(dplyr)
require(ggplot2)
require(tidyr)

insect_species <- c("Papilio rumiko")
# insect_species <- c("Papilio rutulus", "Papilio glaucus",
#                     "Papilio polyxenes", "Papilio zelicaon",
#                     "Papilio rumiko", "Papilio cresphontes",
#                     "Papilio indra", "Papilio canadensis",
#                     "Papilio eurymedon", "Papilio multicaudata",
#                     "Papilio troilus", "Papilio machaon",
#                     "Papilio palamedes")
model_names <- c("svm", "glm")

########################################
# find hosts
# load file with insect-host associations
insects_hosts <- read.csv(file = "data/insect-host.csv")

# identify all host plants used for each insect
host_species <- insects_hosts$accepted_host[insects_hosts$insect %in% insect_species]
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
  data_filename <- paste0("data/gbif/", nice_name, "-gbif.csv")
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
# Data download restricts observations to those in Canada, Mexico, and the 
# United States. It further discards any observations outside a rough rectangle 
# of North America (latitude: 14-80, longitude: -170, -52). See 
# functions/download_gbif.R for details.

# TODO

########################################
# build sdm models for all remaining species (insects and plants)
message(paste0("\n*** Estimating SDMs for ", nrow(all_species), " species and ",
               length(model_names), " models."))

# TODO: At least some of this should be parallelized. Over species, probably
for (i in 1:nrow(all_species)) {
  species_name <- all_species$species_name[i]
  nice_name <- all_species$nice_name[i]
  for (model_name in model_names) {
    model_filename <- paste0("src/indiv/", nice_name, "-model-", model_name, ".R")
    if (!file.exists(model_filename)) {
      warning(paste0("Model file ", model_filename,
                     " is missing. Has build-scripts-model-", model_name, 
                     ".sh been run locally?"))
    } else {
      message(paste0("Running model script ", model_filename))
      source(file = model_filename)
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
# As currently written, contemporary distributions are created in the *same* 
# script as forecast distributions ([nice_name]-prediction-[model].R). Those 
# scripts run *all* the forecast climate models

message(paste0("\n*** Predicting distributions for ", nrow(all_species), 
               " species and ", length(model_names), " models."))
missing_predictions <- integer(0)
for (i in 1:nrow(all_species)) {
  species_name <- all_species$species_name[i]
  nice_name <- all_species$nice_name[i]
  for (model_name in model_names) {
    prediction_filename <- paste0("src/indiv/", nice_name, "-prediction-",
                                  model_name, ".R")
    if (!file.exists(prediction_filename)) {
      warning(paste0("Prediction file ", prediction_filename,
                     " is missing. Has build-prediction-", model_name, 
                     "-files.sh been run locally?"))
      missing_predictions <- c(missing_predictions, i)
    } else {
      message(paste0("Running prediction script ", prediction_filename))
      source(file = prediction_filename)
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
               " species and ", length(model_names), " models."))
missing_overlaps <- integer(0)
for (i in 1:nrow(insect_species)) {
  species_name <- insect_species$species_name[i]
  nice_name <- insect_species$nice_name[i]
  for (model_name in model_names) {
    overlap_filename <- paste0("src/indiv/", nice_name, "-overlap-raster-",
                               model_name, ".R")
    if (!file.exists(overlap_filename)) {
      warning(paste0("Overlap raster file ", overlap_filename,
                     " is missing. Has build-overlap-raster-", model_name, 
                     "-files.sh been run locally?"))
      missing_overlaps <- c(missing_overlaps, i)
    } else {
      message(paste0("Running overlap raster script ", overlap_filename))
      source(file = overlap_filename)
    }
  }
}
message(paste0("Finished creating overlap rasters for ", 
               nrow(insect_species) - length(missing_overlaps), 
               " species and ", length(model_names), " models."))
if (length(missing_overlaps > 0)) {
  warning(paste0("Overlap rasters for ", length(missing_overlaps), 
                 " species not made, due to missing overlap raster scripts."))
}

########################################
# calculate area where insect is present
# calculate area where insect and at least one host species is present

# Both are accomplished by calculate-range-sizes-[model].R. These files 
# iterate over insect species (in parallel) to calculate 
#   (1) the total area in which the insect occurs 
#   (2) the area where the insect occurs with at least one host species
#   (3) the area where the insect occurs but zero host species occur

# As it is written, it does this for *all* insect species, not just the ones 
# that are the focus of this script.
for (model_name in model_names) {
  area_script <- paste0("summary/calcuate-range-sizes-", model_name, ".R")
  message(paste0("Running area calculation script ", area_script))
  # source(file = area_script)
}

########################################
# calculate change between current and forecast (bugs only)
# calculate change between current and forecast (bug range with plants in mind)

# TODO

########################################
# get aridity data & calculate measurement for a species
# Want to get the Thornthwaite Aridity Index from https://envirem.github.io
# [region]_[time period]_[circulation model]_[resolution]_[file format].zip
# Since we want current data, no circulation_model is necessary, for the AI, 
# we need Set1 data. Bioclimate variables are at 2.5 minute resolution, so that 
# should be fine. Using generic, .bil files to get the data.
# Files are archived at https://deepblue.lib.umich.edu/data/concern/generic_works/gt54kn05f
# We want NAmerica_current_2.5arcmin_generic.zip
# Which is at https://deepblue.lib.umich.edu/data/downloads/4b29b610g

# TODO: need to decide on most informative measure of aridity
# PET Seasonality might be interesting to look at, to see if those highly 
# variable environments are likely to 

# They should live in data/envirem, but check to see if they don't
aridity_measure <- "aridityIndexThornthwaite"
# aridity_measure <- "climaticMoistureIndex"

aridity_file <- paste0("data/envirem/current_2-5arcmin_",
                       aridity_measure,
                       ".bil")

if (!file.exists(aridity_file)) {
  # Download the whole archive
  download.file(url = "https://deepblue.lib.umich.edu/data/downloads/4b29b610g",
                destfile = "data/envirem/NAmerica_current_2.5arcmin_generic.zip")
  # Unzip said archive, which will extract to working directory; only pulling out
  # aridity index for now
  file_start <- paste0("current_2-5arcmin_", aridity_measure)
  to_extract <- paste0(file_start, c(".bil", ".bil.aux.xml", ".hdr", ".prj"))
  # to_extract <- c("current_2-5arcmin_aridityIndexThornthwaite.bil",
  #                 "current_2-5arcmin_aridityIndexThornthwaite.bil.aux.xml",
  #                 "current_2-5arcmin_aridityIndexThornthwaite.hdr",
  #                 "current_2-5arcmin_aridityIndexThornthwaite.prj")
  unzip(zipfile = "data/envirem/NAmerica_current_2.5arcmin_generic.zip",
        files = to_extract)
  # Move files of interest to appropriate data folder
  new_files <- paste0("data/envirem/", to_extract)
  file.rename(from = to_extract, to = new_files)
  # Remove the zip archive (leaving it for now, though)
  # file.remove("data/envirem/NAmerica_current_2.5arcmin_generic.zip")
}

# Load aridity data into memory
aridity_data <- raster::raster(x = aridity_file)
# thornthwaite <- raster::raster(x = aridity_file)

# For each species, extract aridity data for each cell of observations and 
# calculate a mean aridity score
aridity_means <- data.frame(species = insect_species$species_name,
                            aridity_mean = NA,
                            aridity_median = NA)
aridity_means_file <- "output/aridity/aridity-means.csv"
for (i in 1:nrow(insect_species)) {
  species_name <- insect_species$species_name[i]
  nice_name <- insect_species$nice_name[i]
  
  # Need observation data for this species, so load that
  obs_file <- paste0("data/gbif/",
                     nice_name,
                     "-gbif.csv")
  if (!file.exists(obs_file)) {
    warning(paste0("No observation data found for ", species_name))
  } else {
    obs <- read.csv(file = obs_file)
    obs <- obs %>%
      dplyr::select(longitude, latitude)
    # Extract aridity from cells with > 1 observation
    sp_arid_raster <- raster::extract(x = aridity_data, 
                                      y = obs)
    # Calculate a mean aridity score for each species
    mean_aridity <- mean(sp_arid_raster, na.rm = TRUE)
    median_aridity <- median(sp_arid_raster, na.rm = TRUE)
    cat(species_name, ": ", mean_aridity, ", ", median_aridity, "\n", sep = "")
    aridity_means$aridity_mean[i] <- mean_aridity
    aridity_means$aridity_median[i] <- median_aridity
  }
}

write.csv(x = aridity_means, 
          file = aridity_means_file,
          row.names = FALSE)

########################################
# do regression between aridity & percent change

# TODO: Make this iterative
# model <- "svm"
model <- "glm"
change_file <- paste0("output/ranges/range-areas-", model, ".csv")

range_changes <- read.csv(file = change_file)
aridity_means <- read.csv(file = aridity_means_file)

# Merge range information with aridity information
range_changes <- range_changes %>%
  inner_join(aridity_means)

# Calculate deltas, with and without considering plants
range_changes <- range_changes %>%
  mutate(area_change_total = GFDL.ESM4_RCP45_area - current_area,
         area_change_overlap = GFDL.ESM4_RCP45_overlap_area - current_overlap_area) %>%
  mutate(perc_change_total = area_change_total / current_area,
         perc_change_overlap = area_change_overlap / current_overlap_area)

# Run regression
model_total <- lm(perc_change_total ~ aridity_mean, data = range_changes)
summary(model_total)
model_total <- lm(perc_change_total ~ aridity_median, data = range_changes)
summary(model_total)

model_overlap <- lm(perc_change_overlap ~ aridity_mean, data = range_changes)
summary(model_overlap)
model_overlap <- lm(perc_change_overlap ~ aridity_median, data = range_changes)
summary(model_overlap)

# Quick plot
plot_data <- range_changes %>%
  select(species, perc_change_total, perc_change_overlap, aridity_mean) %>%
  pivot_longer(cols = -c(species, aridity_mean),
               names_to = "measurement",
               values_to = "value")

ggplot(data = plot_data, mapping = aes(x = aridity_mean, y = value, color = measurement)) +
  geom_point() + 
  geom_smooth(method = "lm")
