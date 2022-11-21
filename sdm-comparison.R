# Evaluate output from different SDMs
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-11-17

# Before running _this_ script, bash scripts to generate R scripts for 
# individual species will need to be run. These three scripts are 
# in the src/bash folder and can be run from a bash command line:
# src/bash/build-scripts-SDM.sh
# src/bash/build-scripts-prediction.sh
# src/bash/build-scripts-overlap-raster.sh

library(dplyr)
library(stringr)
library(raster)
library(terra)
library(tidyr)
library(parallel)

source(file = "load_functions.R")

# insect_names <- c("Papilio rumiko", "Papilio cresphontes")
sdm_names <- c("brt", "gam", "glm", "lasso", "maxent-notune", "maxent-tune")
insect_names <- c("Papilio rumiko")

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
# Data retrieval, filtering, background point generation is done for all species 
# by the gibf-().R scripts in the src/data/ folder. 
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
# unlist(s)
stopCluster(cl = clust)

message(paste0("Finished estimating SDMs for ", nrow(all_species), 
               " species and ", length(sdm_names), " models."))

########################################
# do current distributions for bug (all bugs)
# do current distributions for plants (all plants)
# do forecast distributions for bug (all bugs, ssp370 only)
# do forecast distributions for plants (all plants, ssp370 only)

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
    prediction_filename <- paste0("src/indiv/", nice_name, "-distribution-",
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
message(paste0("Finished predicting probabilities and distributions for ", 
               nrow(all_species) - length(missing_predictions), 
               " species and ", length(sdm_names), " models."))
if (length(missing_predictions > 0)) {
  warning(paste0("Predictions for ", length(missing_predictions), 
                 " species not made, due to missing prediction scripts."))
}

########################################
# start sdm comparison
# For each species, create table with model fit stats and other summaries for 
# each sdm_method.  
# Create list containing rasters with predicted probabilities
# Create list containing rasters with predicted distributions (binary values)

climate_models <- read.csv("data/climate-models.csv")
# Restrict to just current and ssp370
climate_models <- climate_models %>%
  filter(is.na(ssp) | ssp == 370)

# Table that will contain model-specific values/summaries for each species
sdm_table <- expand.grid(method = sdm_names,
                         species = all_species$species_name)
sdm_table <- sdm_table %>%
  mutate(threshold = NA,
         AUC_test = NA,
         MAE_test = NA)
cols_area <- paste0("area_", c("current", "2041", "2071"))
cols_current <- paste0(c("lon_min", "lon_max", "lat_min", "lat_max"), "_current")
cols_2041 <- paste0(c("lon_min", "lon_max", "lat_min", "lat_max"), "_2041")
cols_2071 <- paste0(c("lon_min", "lon_max", "lat_min", "lat_max"), "_2071")
sdm_table[,c(cols_area, cols_current, cols_2041, cols_2071)] <- NA

for (i in 1:nrow(all_species)) {
  species_name <- all_species$species_name[i]
  nice_name <- all_species$nice_name[i]

  # Read in pres-abs dataset
  pa_file <- paste0("data/gbif/presence-absence/", nice_name, "-pa.csv")
  pa <- read.csv(pa_file)
  # Restrict to just testing dataset
  pa <- pa %>%
    filter(fold == 1)

  # Create lists that will hold rasters w/ predicted probabilities, distributions
  preds_list <- list()
  dist_list <- list()  
  
  for (sdm_name in sdm_names) {
    
    row_ind <- which(sdm_table$species == species_name & 
                       sdm_table$method == sdm_name)
    
    # Read in sdm_model and fill in table
    sdm_file <- paste0("output/SDMs/", nice_name, "-", sdm_name, ".rds")
    sdm_model <- readRDS(sdm_file)
    sdm_table$threshold[row_ind] <- sdm_model$thresh
    sdm_table$AUC_test[row_ind] <- sdm_model$evaluation@auc    
    
    for (j in 1:nrow(climate_models)) {
      
      scen <- climate_models$name[j]
      scen_yr <- ifelse(scen != "current", str_sub(scen,-4,-1), scen)    
    
      # Read in raster with predicted probabilities
      preds_file <- paste0("output/predicted-probabilities/", nice_name, 
                           "-pred-probs-", sdm_name, "-", scen, ".rds")
      preds <- readRDS(preds_file)
      preds <- rast(preds) 

      if (scen == "current") {

        # Attach model predictions to pa dataset and 
        probs <- terra::extract(x = preds, 
                                y = pa[, c("x", "y")],
                                cells = FALSE, 
                                ID = FALSE)
        pa$probs <- probs$layer
        
        # Calculate absolute error (predicted probability - observed) for the 
        # testing dataset
        pa$abs_error <- abs(pa$probs - pa$pa)

        # Add mean absolute error (MAE) to sdm_table
        sdm_table$MAE_test[row_ind] <- mean(pa$abs_error, na.rm = TRUE)
      }
 
      # Create the distribution raster
      dist <- 1 * (preds > sdm_table$threshold[row_ind])
        
      # Calculate predicted total area in km2
      range <- terra::ifel(dist == 0, NA, dist)
      area <- terra::expanse(range, unit = "km")
      sdm_table[row_ind, paste0("area_", scen_yr)] <- area
        
      # Calculate predicted longitudinal/latitudinal limits
      range <- terra::trim(range)
      sdm_table[row_ind, get(paste0("cols_", scen_yr))] <- ext(range)
        
      # Add preds and dist rasters to lists
      preds_list <- c(preds_list, preds)
      names(preds_list)[length(preds_list)] <- paste0(sdm_name, "_", scen_yr)
      dist_list <- c(dist_list, dist)
      names(dist_list)[length(dist_list)] <- paste0(sdm_name, "_", scen_yr)
        
    } # end climate scenarios loop (j)
    
  } # end sdm_method loop
  
  # Save correlation matrix and rasters for each climate scenario
  for (j in 1:nrow(climate_models)) {
    
    scen <- climate_models$name[j]
    scen_yr <- ifelse(scen != "current", str_sub(scen,-4,-1), scen)    
    
    # Extract probability raster for climate scenario
    preds_sub <- preds_list[grepl(scen_yr, names(preds_list))]
    names(preds_sub) <- sdm_names
    # Combine probability rasters into a single SpatRaster
    preds_rast <- rast(preds_sub)
    
    # Create a correlation matrix (Spearman's rho better than Pearson?)
    # (note that as.data.frame removes cells with NAs)
    corrs <- cor(as.data.frame(preds_rast), method = "spearman")
    
    # Save object with species and scenario/yr in name
    # (eg, corr_papilio_rumiko_2071)
    assign(paste0("corr_", nice_name, "_", scen_yr), corrs)
    
    # Save probability rasters (as a single multi-layer SpatRaster)
    assign(paste0("preds_", nice_name, "_", scen_yr), preds_rast)
    
    # Subset distribution rasters and save
    dist_sub <- dist_list[grepl(scen_yr, names(dist_list))]
    names(dist_sub) <- sdm_names
    dist_rast <- rast(dist_sub)
    assign(paste0("dists_", nice_name, "_", scen_yr), dist_rast)
    
  } # end climate scenarios loop (j)

} # end species loop (i)

# TODO decide whether we want to save some of these objects to file

# Viewing some results
  # First, pick a species
  species <- all_species$nice_name[1]
  
  # View distribution maps for a climate scenario/yr (one panel per model)
  plot(get(paste0("dists_", species, "_current")))
  plot(get(paste0("dists_", species, "_2041")))
  plot(get(paste0("dists_", species, "_2071")))
  
  # Another way to view model agreement/disagreement 
  # (map with number of models predicting occurrence)
  plot(as.factor(sum(get(paste0("dists_", species, "_current")))))
  plot(as.factor(sum(get(paste0("dists_", species, "_2041")))))
  plot(as.factor(sum(get(paste0("dists_", species, "_2071")))))

# Table with summaries:
  sdm_table %>%
    mutate(across(threshold:lat_max_2071, ~ round(.x, 2)))
  
# Summarizing correlations
  corrs <- expand.grid(species = all_species$species_name,
                       scenario = c("current", "2041", "2071"),
                       sdm_1 = sdm_names,
                       sdm_2 = sdm_names)
  corrs <- corrs %>%
    filter(sdm_1 != sdm_2) %>%
    rowwise() %>%
    mutate(sdm1 = head(sort(c(sdm_1, sdm_2)),1),
           sdm2 = tail(sort(c(sdm_1, sdm_2)),1)) %>%
    dplyr::select(-c(sdm_1, sdm_2)) %>%
    unique %>%
    arrange(species, scenario, sdm1) %>%
    data.frame
  
  corrs$nice_name <- all_species$nice_name[match(corrs$species, all_species$species_name)]
  for (i in 1:nrow(corrs)) {
    corr_mat <- get(paste0("corr_", corrs$nice_name[i], "_", corrs$scenario[i]))
    corrs$corr[i] <- round(corr_mat[rownames(corr_mat) == corrs$sdm1[i],
                                    colnames(corr_mat) == corrs$sdm2[i]], 2)
  }
  corrs$nice_name <- NULL
  
  # Summarize by method
  corrs %>%
    group_by(sdm1, sdm2) %>%
    summarize(mn_corr = round(mean(corr), 2),
              min_corr = min(corr),
              max_corr = max(corr),
              sd_corr = round(sd(corr), 2)) %>%
    arrange(desc(mn_corr)) %>%
    data.frame
  
  # Summarize by species
  corrs %>%
    group_by(species) %>%
    summarize(mn_corr = round(mean(corr), 2),
              min_corr = min(corr),
              max_corr = max(corr),
              sd_corr = round(sd(corr), 2)) %>%
    arrange(desc(mn_corr)) %>%
    data.frame
  
  # Summarize by scenario
  corrs %>%
    group_by(scenario) %>%
    summarize(mn_corr = round(mean(corr), 2),
              min_corr = min(corr),
              max_corr = max(corr),
              sd_corr = round(sd(corr), 2)) %>%
    arrange(desc(mn_corr)) %>%
    data.frame
  