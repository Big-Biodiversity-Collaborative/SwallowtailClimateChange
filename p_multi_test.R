# Proof of concept for part of workflow: Papilio multicaudata & hosts
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-05-18

library(raster) # also loads sp
library(dplyr)  # load *after* raster for easier use of select
library(stringr)
library(tidyr)
library(dismo) # requires sp, too
library(maptools) # for some QA plotting, may not be necessary
library(kernlab)

# Load up the functions from the functions folder
function_files <- list.files(path = "./functions", 
                             pattern = ".R$", 
                             full.names = TRUE)
for(fun_file in function_files) {
  message(paste0("Loading function from ", fun_file))
  source(file = fun_file)
}

# TODO: Consider creating a list of lists, where each element in the top-level list 
# is a two-element list with elements
#   + type: character vector with "insect" or "host"
#   + obs:  data frame of observations
# This way, iterating over the list elements and doing all the same things 
# (filtering out NAs, selecting columns, running contemporary & forecast models)
# becomes easier; can also start attaching things like 
#   + background: data frame of pseudo-absence points

# Papilio multicaudata data downloaded 2021-05-18 from GBIF
# https://doi.org/10.15468/dl.v2puuk
insect_obs <- clean_gbif(file = "data/papilio_multicaudata.csv")

# Hosts
# Vauquelinia californica, Arizona rosewood
# Fraxinus anomala, single-leaf ash
# Prunus emarginata, bitter cherry
# Prunus virginiana, chokecherry
# Ptelea trifoliata, hoptree, 
# Platanus wrightii, Arizona sycamore
# WAIT (introduced to western US) Fraxinus pennsylvanica, green ash 

host_obs_list <- list()
# Starting with just three of the hosts
# Vauquelinia californica, Arizona rosewood
# https://doi.org/10.15468/dl.rfrynw
host_obs_list[[1]] <- clean_gbif(file = "data/vauquelinia_californica.csv")

# Fraxinus anomala, single-leaf ash
# https://doi.org/10.15468/dl.tgfk9n
host_obs_list[[2]] <- clean_gbif(file = "data/fraxinus_anomala.csv")

# Prunus emarginata, bitter cherry
# https://doi.org/10.15468/dl.djrf2z
host_obs_list[[3]] <- clean_gbif(file = "data/prunus_emarginata.csv")

########################################
# Get geographic extents of all organisms

# Get the geographic extents of the organism in question; will be used for 
# background point sampling
insect_extent <- get_extent(data = insect_obs)

host_extent <- list()
for (h in 1:length(host_obs_list)) {
  host_extent[[h]] <- get_extent(data = host_obs_list[[h]])
}

# Will use bioclim data files to create masks for sampling resolution
# Check for bioclim data and download if it isn't there
if (!file.exists("data/wc2-5/bio1.bil")) {
  bioclim.data <- getData(name = "worldclim",
                          var = "bio",
                          res = 2.5,
                          path = "data/")
}

# Using the first bil file to create a raster to use as mask for sampling 
# background points
bil_file <- list.files(path = "data/wc2-5", 
                       pattern = ".bil$", 
                       full.names = TRUE)[1]
mask <- raster(bil_file)

# Randomly sample points (same number as our observed points)
insect_background <- randomPoints(mask = mask,    # Provides resolution of sampling points
                           n = nrow(insect_obs),  # Number of random points
                           ext = insect_extent,   # Spatially restricts sampling
                           extf = 1.25)           # Expands sampling a little bit
# Will want to use this with dplyr::bind_rows, so convert to data frame
insect_background <- as.data.frame(insect_background)

# Need to do background sampling for each host
host_background <- list()
for(h in 1:length(host_obs_list)) {
  message(paste0("Sampling background points for host ", h))
  host_background[[h]] <- randomPoints(mask = mask, 
                                       n = nrow(host_obs_list[[h]]), 
                                       ext = host_extent[[h]], 
                                       extf = 1.25)
  host_background[[h]] <- as.data.frame(host_background[[h]])
}

# Will want to crop the predictors to the extent of _all_ background points for 
# quicker downstream processing; so long as background sampling extends beyond 
# the extent of observed occurrences (i.e. extf > 1 for randomPoints), we can 
# be sure the extent of all background points will include all predictor data 
# we need
all_backgrounds <- insect_background %>%
  dplyr::bind_rows(host_background) %>%
  dplyr::rename(longitude = x,
                latitude = y)

all_extent <- get_extent(data = all_backgrounds)

################################################################################
# SVM Model
# In large part modified from 
# https://rspatial.org/raster/sdm/6_sdm_methods.html#machine-learning-methods

# Grab worldclim data to use as predictors
predictors <- stack(list.files(path = "data/wc2-5",
                               pattern = ".bil$",
                               full.names = TRUE))

# Crop the predictors for faster subsequent processing
predictors <- raster::crop(x = predictors, y = all_extent)

################################################################################
# Run support vector machine model on insect
insect_svm <- run_svm(obs = insect_obs,
                      absence = insect_background,
                      predictors = predictors)

data("wrld_simpl")
# Setup graphical params for side-by-side plots
par(mfrow = c(1, 2))
# Plot the probabilities
plot(insect_svm$probs, main = "SVM Probabilities")
# Add the map
plot(wrld_simpl, 
     add = TRUE,
     border = "grey30")
# Add the presence absence plot
plot(insect_svm$probs > insect_svm$thresh, main = "Presence/Absence")
# Add geopolitical borders
plot(wrld_simpl, 
     add = TRUE,
     border = "grey30")
# Throw on all our observed points
points(x = insect_obs$longitude, 
       y = insect_obs$latitude,
       pch = "+",
       cex = 0.1)
# Reset graphical parameters
par(mfrow = c(1, 1))

################################################################################
# Run support vector machine SDM on host 1
host1_svm <- run_svm(obs = host_obs_list[[1]],
                     absence = host_background[[1]],
                     predictors = predictors)

data("wrld_simpl")
# Setup graphical params for side-by-side plots
par(mfrow = c(1, 2))
# Plot the probabilities
plot(host1_svm$probs, main = "SVM Probabilities")
# Add the map
plot(wrld_simpl, 
     add = TRUE,
     border = "grey30")
# Add the presence absence plot
plot(host1_svm$probs > host1_svm$thresh, main = "Presence/Absence")
# Add geopolitical borders
plot(wrld_simpl, 
     add = TRUE,
     border = "grey30")
# Throw on all our observed points
points(x = host_obs_list[[1]]$longitude, 
       y = host_obs_list[[1]]$latitude,
       pch = "+",
       cex = 0.1)
# Reset graphical parameters
par(mfrow = c(1, 1))
