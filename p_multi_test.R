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
# Remember, GBIF is a dick and provides TAB-separated files (not CSV)
insect_obs <- read.delim(file = "data/papilio_multicaudata.csv")

# Drop unused columns (most everything)
# TODO: if standard, convert to function
insect_obs <- insect_obs %>%
  filter(countryCode %in% c("CA", "MX", "US")) %>%
  filter(str_detect(issue, pattern = "ZERO_COORDINATE", negate = TRUE)) %>%
  dplyr::select(gbifID, species, decimalLongitude, decimalLatitude, 
         year, month, day) %>%
  drop_na() # Need complete records for all these


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
host_obs_list[[1]] <- read.delim(file = "data/vauquelinia_californica.csv")
host_obs_list[[1]] <- host_obs_list[[1]] %>%
  filter(countryCode %in% c("CA", "MX", "US")) %>%
  filter(str_detect(issue, pattern = "ZERO_COORDINATE", negate = TRUE)) %>%
  dplyr::select(gbifID, species, decimalLongitude, decimalLatitude, 
       year, month, day) %>%
  drop_na()

# Fraxinus anomala, single-leaf ash
# https://doi.org/10.15468/dl.tgfk9n
host_obs_list[[2]] <- read.delim(file = "data/fraxinus_anomala.csv")
host_obs_list[[2]] <- host_obs_list[[2]] %>%
  filter(countryCode %in% c("CA", "MX", "US")) %>%
  filter(str_detect(issue, pattern = "ZERO_COORDINATE", negate = TRUE)) %>%
  dplyr::select(gbifID, species, decimalLongitude, decimalLatitude, 
         year, month, day) %>%
  drop_na()

# Prunus emarginata, bitter cherry
# https://doi.org/10.15468/dl.djrf2z
host_obs_list[[3]] <- read.delim(file = "data/prunus_emarginata.csv")
host_obs_list[[3]] <- host_obs_list[[3]] %>%
  filter(countryCode %in% c("CA", "MX", "US")) %>%
  filter(str_detect(issue, pattern = "ZERO_COORDINATE", negate = TRUE)) %>%
  dplyr::select(gbifID, species, decimalLongitude, decimalLatitude, 
                year, month, day) %>%
  drop_na()


########################################
# Background points
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

# Get the geographic extent of the organism in question
insect_extent <- extent(x = c(floor(min(insect_obs$decimalLongitude)),
                              ceiling(max(insect_obs$decimalLongitude)),
                              floor(min(insect_obs$decimalLatitude)),
                              ceiling(max(insect_obs$decimalLatitude))))


# Randomly sample points (same number as our observed points)
insect_background <- randomPoints(mask = mask,    # Provides resolution of sampling points
                           n = nrow(insect_obs),  # Number of random points
                           ext = insect_extent,   # Spatially restricts sampling
                           extf = 1.25)           # Expands sampling a little bit

host_extent <- list()
host_background <- list()
# Host 1
host_extent[[1]] <- extent(x = c(floor(min(host_obs_list[[1]]$decimalLongitude)),
                              ceiling(max(host_obs_list[[1]]$decimalLongitude)),
                              floor(min(host_obs_list[[1]]$decimalLatitude)),
                              ceiling(max(host_obs_list[[1]]$decimalLatitude))))

host_background[[1]] <- randomPoints(mask = mask, 
                                      n = nrow(host_obs_list[[1]]), 
                                      ext = host_extent[[1]], 
                                      extf = 1.25)

# Host 2
host_extent[[2]] <- extent(x = c(floor(min(host_obs_list[[2]]$decimalLongitude)),
                                 ceiling(max(host_obs_list[[2]]$decimalLongitude)),
                                 floor(min(host_obs_list[[2]]$decimalLatitude)),
                                 ceiling(max(host_obs_list[[2]]$decimalLatitude))))

host_background[[2]] <- randomPoints(mask = mask, 
                                     n = nrow(host_obs_list[[2]]), 
                                     ext = host_extent[[2]], 
                                     extf = 1.25)

# Host 3
host_extent[[3]] <- extent(x = c(floor(min(host_obs_list[[3]]$decimalLongitude)),
                                 ceiling(max(host_obs_list[[3]]$decimalLongitude)),
                                 floor(min(host_obs_list[[3]]$decimalLatitude)),
                                 ceiling(max(host_obs_list[[3]]$decimalLatitude))))

host_background[[3]] <- randomPoints(mask = mask, 
                                     n = nrow(host_obs_list[[3]]), 
                                     ext = host_extent[[3]], 
                                     extf = 1.25)


################################################################################
# QA/QC on background sampling
# Skip to next horizontal line if unnecessary

data("wrld_simpl")

# Plot the base map
host_number <- 3
plot(wrld_simpl, 
     xlim = host_extent[[host_number]][c(1, 2)],
     ylim = host_extent[[host_number]][c(3, 4)],
     axes = TRUE, 
     col = "grey95",
     main = "Presence and pseudo-absence points")

# Add the observations
points(x = host_obs_list[[host_number]]$decimalLongitude,
       y = host_obs_list[[host_number]]$decimalLatitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)

# Draw a box around the extent we used for randomPoints restriction
plot(host_extent[[host_number]], 
     add = TRUE, 
     col = "red")

# Add the background points
points(host_background[[host_number]],
       col = "grey30",
       pch = 3,
       cex = 0.5)

################################################################################
# SVM Model
# In large part modified from 
# https://rspatial.org/raster/sdm/6_sdm_methods.html#machine-learning-methods

# If using bioclim data from the dismo package to use as predictors
# predictors <- stack(list.files(file.path(system.file(package = "dismo"), "ex"),
#                                pattern = "grd$",
#                                full.names = TRUE))

# Grab worldclim data to use as predictors
predictors <- stack(list.files(path = "data/wc2-5",
                               pattern = ".bil$",
                               full.names = TRUE))

# Only need geo coordinates, so extract those (in x, y order)
insect_coords <- insect_obs %>%
  select(decimalLongitude, decimalLatitude)

# Use the observed points to pull out relevant predictor values?
# Takes a little while when using worldclim instead of dismo
insect_presence <- raster::extract(x = predictors, y = insect_coords)
insect_absence <- raster::extract(x = predictors, y = insect_background)

# Make a vector of appropriate length with 0/1 values for 
# (pseudo)absence/presence
insect_pa_data <- c(rep(1, nrow(insect_presence)), rep(0, nrow(insect_absence)))
# Combine our presence absence vector with environmental data we extracted
insect_df <- data.frame(cbind(pa = insect_pa_data,
                              rbind(insect_presence, insect_absence)))
# No es necessario if using worldclim data
# insect_df[, "biome"] <- as.factor(insect_df[, "biome"])

# Need to create 80/20 training/testing split (could be more efficient, perhaps)
pres_df <- insect_df %>%
  filter(pa == 1)
pres_group <- dismo::kfold(pres_df, 5)
pres_train <- pres_df[pres_group != 1, ]
pres_test <- pres_df[pres_group == 1, ]

back_df <- insect_df %>%
  filter(pa == 0)
back_group <- dismo::kfold(back_df, 5)
back_train <- back_df[back_group != 1, ]
back_test <- back_df[back_group == 1, ]

# Add presence and pseudoabsence training data into single data frame
sdmtrain <- rbind(pres_train, back_train)
sdmtest <- rbind(pres_test, back_test)

# Run an SVM model, specifying model with standard formula syntax
svm_model <- kernlab::ksvm(pa ~ bio1 + bio5 + bio6 + bio7 + bio8 + bio12 +
                             bio16 + bio17,
                           data = sdmtrain)
svm_eval <- dismo::evaluate(pres_test, back_test, svm_model)
svm_eval
# class          : ModelEvaluation 
# n presences    : 648 
# n absences     : 421 
# AUC            : 0.9254182 
# cor            : 0.7737041 
# max TPR+TNR at : 0.7831999 

# Do the predictions so we can map things (takes a few seconds with worldclim)
probs <- predict(predictors, 
                 svm_model, 
                 ext = insect_extent)

# Calculate threshold so we can include a P/A map
pres_threshold <- threshold(svm_eval, "spec_sens")

# Setup graphical params for side-by-side plots
par(mfrow = c(2, 1))
# Plot the probabilities
plot(probs, main = "SVM Probabilities")
# Add the map
plot(wrld_simpl, 
     add = TRUE,
     border = "grey30")
# Add the presence absence plot
plot(probs > pres_threshold, main = "Presence/Absence")
# Add geopolitical borders
plot(wrld_simpl, 
     add = TRUE,
     border = "grey30")
# Throw on all our observed points
points(insect_coords, 
       pch = "+",
       cex = 0.1)
# Reset graphical parameters
par(mfrow = c(1, 1))
