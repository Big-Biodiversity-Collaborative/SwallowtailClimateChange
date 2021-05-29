# Proof of concept for part of workflow: Papilio multicaudata & hosts
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-05-18

# Using require in place of library for easier grepping later on
require(raster) # also loads sp
require(dplyr)  # load *after* raster for easier use of select
require(stringr)
require(tidyr)
require(dismo) # requires sp, too
require(maptools) # for some QA plotting, may not be necessary
require(kernlab)

# Load up the functions from the functions folder
function_files <- list.files(path = "./functions", 
                             pattern = ".R$", 
                             full.names = TRUE)
for(fun_file in function_files) {
  message(paste0("Loading function from ", fun_file))
  source(file = fun_file)
}

# Will use bioclim data files to create masks for sampling resolution of 
# background points
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

# Eventually be replaced...somehow...
obs_data <- data.frame(type = c("insect", rep(x = "host", times = 3)),
                       name = c("Papilio multicaudata", "Vauquelinia californica",
                                "Fraxinus anomala", "Prunus emarginata"))

# List to hold data for easier iterations
obs_list <- list()
for (i in 1:nrow(obs_data)) {
  message(paste0("\nSetting up list for ", obs_data$name[i]))
  # A compute-friendly name
  nice_name <- tolower(gsub(pattern = " ", 
                            replacement = "_",
                            x = obs_data$name[i]))
  obs_file <- paste0("data/",
                     nice_name,
                     ".csv")
  obs_list[[i]] <- list(type = obs_data$type[i],
                        name = obs_data$name[i],
                        obs = clean_gbif(file = obs_file))
  # Add the extent for this beast to the list element
  obs_list[[i]][["extent"]] <- get_extent(data = obs_list[[i]][["obs"]])

  message(paste0("Sampling pseudo-absence points for ", obs_data$name[i]))
  # Do background point sampling while we're here
  # Randomly sample points (same number as our observed points)
  background_points <- randomPoints(mask = mask,    # Provides resolution of sampling points
                                    n = nrow(obs_list[[i]][["obs"]]),  # Number of random points
                                    ext = obs_list[[i]][["extent"]],   # Spatially restricts sampling
                                    extf = 1.25)           # Expands sampling a little bit
  # Will want to use this with dplyr::bind_rows, so convert to data frame
  obs_list[[i]][["background"]] <- as.data.frame(background_points)
  
  # Use the compute friendly name for the name of this list element
  names(obs_list)[i] <- nice_name
}

# Will want to crop the predictors to the extent of _all_ background points for 
# quicker downstream processing; so long as background sampling extends beyond 
# the extent of observed occurrences (i.e. extf > 1 for randomPoints), we can 
# be sure the extent of all background points will include all predictor data 
# we need
all_backgrounds <- lapply(obs_list, "[[", "background") %>%
  dplyr::bind_rows() %>%
  rename(longitude = x,
         latitude = y)

all_extent <- get_extent(data = all_backgrounds)

# Grab worldclim data to use as predictors
predictors <- stack(list.files(path = "data/wc2-5",
                               pattern = ".bil$",
                               full.names = TRUE))

# Crop the predictors for faster subsequent processing
predictors <- raster::crop(x = predictors, y = all_extent)

# Run all support vector machine models
for (i in 1:length(obs_list)) {
  message(paste0("\n*****  Running SVM on ", obs_list[[i]][["name"]], "  *****"))
  obs_list[[i]][["svm_model"]] <- run_svm(obs = obs_list[[i]][["obs"]],
                                        absence = obs_list[[i]][["background"]],
                                        predictors = predictors)
}

# Want to stack the rasters of plants for P/A of hosts, regardless of species
# Start by figuring out which of the list elements is the insect and which 
# are the hosts
# Need to start by creating presence/absence rasters for each beast
for (i in 1:length(obs_list)) {
  obs_list[[i]][["pa_raster"]] <- obs_list[[i]]$svm_model$probs >
    obs_list[[i]]$svm_model$thresh
  
}

################################################################################
# Plotting below here
# Returns a named vector; each (named) element indicates host/insect
types <- unlist(lapply(obs_list, "[[", "type"))

# Extract the insect presence/absence raster
insect <- which(types == "insect")
insect_pa <- obs_list[[insect]]$pa_raster

# Create stack of host presence/absence rasters
hosts <- which(types == "host")
host_pa_rasters <- lapply(obs_list[hosts], "[[", "pa_raster")
host_pa_stack <- stack_rasters(r = host_pa_rasters, out = "binary")

# Get creative with raster so easier to see host vs insect
host_pa_stack[host_pa_stack >= 1] <- 2

# Want to add a third plot showing overlap; will need to get creative so we 
# see three colors:
# + 1 where insect only occurs
# + 2 where plants only occur
# + 3 where insects and plants overlap
trio_stack <- stack_rasters(r = list(insect_pa, host_pa_stack))

data("wrld_simpl")
# We'll need three colors: insect, plants, insect+plants
plot_colors <- hcl.colors(n = 3, palette = "Cividis")
# Setup graphical params for side-by-side plots
par(mfrow = c(1, 3))
# Plot insect
insect_title <- paste0(obs_list[[insect]]$name, " P/A")
plot(insect_pa, main = insect_title, col = c(NA, plot_colors[1]))
# Add the map
plot(wrld_simpl, 
     add = TRUE,
     border = "grey30")
# Plot plant
plot(host_pa_stack, main = "Host P/A", col = c(NA, NA, plot_colors[2]))
# Add the map
plot(wrld_simpl, 
     add = TRUE,
     border = "grey30")
plot(trio_stack, main = "Overlap", col = c(NA, plot_colors))
# Add the map
plot(wrld_simpl, 
     add = TRUE,
     border = "grey30")
par(mfrow = c(1, 1))



# For some QA/QC, let's 




################################################################################
##### Original implementation below here
################################################################################
# TODO: Consider creating a list of lists, where each element in the top-level list 
# is a two-element list with elements
#   + type: character vector with "insect" or "host"
#   + obs:  data frame of observations
# This way, iterating over the list elements and doing all the same things 
# (filtering out NAs, selecting columns, running contemporary & forecast models)
# becomes easier; can also start attaching things like 
#   + background: data frame of pseudo-absence points

# Each list element would have:
#   + type: character vector with "insect" or "host"
#   + obs:  data frame of observations
#   + name: the species name
#   + background: background points (added later)
#   + svm_model: support vector machine model stuff (output of run_svm)

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
# Run support vector machine SDM on hosts
host_svms <- list()
for (h in 1:length(host_obs_list)) {
  message(paste0("\nRunning host ", h, " of ", length(host_obs_list)))
  host_svms[[h]] <- run_svm(obs = host_obs_list[[h]],
                            absence = host_background[[h]],
                            predictors = predictors)
}

data("wrld_simpl")
# Setup graphical params for side-by-side plots
par(mfrow = c(3, 2))
for (h in 1:length(host_svms)) {
  # Plot the probabilities
  plot(host_svms[[h]]$probs, main = "SVM Probabilities")
  # Add the map
  plot(wrld_simpl, 
       add = TRUE,
       border = "grey30")
  # Add the presence absence plot
  plot(host_svms[[h]]$probs > host_svms[[h]]$thresh, main = "Presence/Absence")
  # Add geopolitical borders
  plot(wrld_simpl, 
       add = TRUE,
       border = "grey30")
  # Throw on all our observed points
  points(x = host_obs_list[[h]]$longitude, 
         y = host_obs_list[[h]]$latitude,
         pch = "+",
         cex = 0.1)
}
# Reset graphical parameters
par(mfrow = c(1, 1))
