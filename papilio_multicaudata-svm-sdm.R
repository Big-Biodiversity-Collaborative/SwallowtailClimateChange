# Proof of concept for part of workflow: Papilio multicaudata & hosts
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-05-18

# Using require in place of library for easier grepping later on
require(raster) # also loads sp
require(dplyr)  # load *after* raster for easier use of select
# require(tidyr)
require(dismo)    # requires sp, too
require(maptools) # for some QA plotting

# Load up the functions from the functions folder
source(file = "load_functions.R")

# Will use bioclim data files to create masks for sampling resolution of 
# background points
# Using the first bil file to create a raster to use as mask for sampling 
# background points
bil_file <- list.files(path = "data/wc2-1", 
                       pattern = ".tif$", 
                       full.names = TRUE)[1]
mask <- raster(bil_file)

# For pipeline implementation, this information is read in from the file 
# data/insect-hosts.csv
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
  obs_file <- paste0("data/gbif/",
                     nice_name,
                     ".csv")
  if (!file.exists(obs_file)) {
    unzip(zipfile = "data/gbif.zip")
  }
  obs_list[[i]] <- list(type = obs_data$type[i],
                        name = obs_data$name[i],
                        nice_name = nice_name,
                        obs = clean_gbif(file = obs_file))
  # Add the extent for this beast to the list element
  obs_list[[i]][["extent"]] <- get_extent(data = obs_list[[i]][["obs"]])

  message(paste0("Sampling pseudo-absence points for ", obs_data$name[i]))
  # Do background point sampling while we're here
  # Randomly sample points (same number as our observed points)
  background_points <- dismo::randomPoints(mask = mask,    # Provides resolution of sampling points
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
  dplyr::rename(longitude = x,
                latitude = y)

all_extent <- get_extent(data = all_backgrounds)

# Grab worldclim data to use as predictors
predictors <- raster::stack(list.files(path = "data/wc2-1",
                                       pattern = ".tif$",
                                       full.names = TRUE))

# Crop the predictors for faster subsequent processing
predictors <- raster::crop(x = predictors, y = all_extent)

# Run all support vector machine models; while we are here save those models
# to file as well as predicted presence / absence 
for (i in 1:length(obs_list)) {
  start_message <- paste0("\n*****  Running SVM on ", 
                          obs_list[[i]][["name"]], 
                          " [species ", i, " of ", length(obs_list), "]",
                          "  *****")
  message(start_message)
  obs_list[[i]][["svm_model"]] <- run_svm(obs = obs_list[[i]][["obs"]],
                                          absence = obs_list[[i]][["background"]],
                                          predictors = predictors)
  # We'll need presence / absence predicted by model for a variety of 
  # downstream applications, so compute the prediction and save to file.
  obs_list[[i]][["pa_raster"]] <- obs_list[[i]]$svm_model$probs >
    obs_list[[i]]$svm_model$thresh

  # Save the model to file in output/models/
  model_file <- paste0("output/models/",
                       obs_list[[i]][["nice_name"]],
                       "-model-svm-current.rds")
  saveRDS(object = obs_list[[i]][["svm_model"]],
          file = model_file)

  # Save these rasters for later compilation of maps in output/distributions/
  pa_raster_file <- paste0("output/distributions/",
                           obs_list[[i]][["nice_name"]],
                           "-distribution-svm-current.rds")
  saveRDS(object = obs_list[[i]][["pa_raster"]],
          file = pa_raster_file)
  
  message(paste0("SVM model for ", obs_list[[i]][["name"]], 
                 " complete; saved to ", model_file))
}

################################################################################
# Plotting below here
# Want to stack the rasters of plants for P/A of hosts, regardless of species
# Start by figuring out which of the list elements is the insect and which 
# are the hosts
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

################################################################################
# Forecast modeling

# Do forecast model, with GFDL-ESM4_RCP45 as example forecast data
# Load forecast data, download if it does not exist
gfdl_data <- raster::getData(name = "CMIP5",
                             var = "bio",
                             res = 2.5,
                             rcp = 45,
                             model = "GD",
                             year = 70,
                             path = "data/")

# Need to rename variables in forecast climate data so our predictions work 
# (these are the same names as the bioclim data, used for the creation of our 
# species distribution model)
names(gfdl_data) <- paste0("bio", 1:19)

# Need to transform temperature variables, which come in from CMIP5 as degrees
# C x 10.
temp_biovars <- c("bio1", "bio2", "bio4", "bio5", "bio6", "bio7", "bio8", 
                  "bio9", "bio10", "bio11")
for (temp_biovar in temp_biovars) {
  gfdl_data[[temp_biovar]] <- 0.1 * gfdl_data[[temp_biovar]]
}

# For each species, use the SVM model to predict probabilities then use the 
# previously identified threshold to calculate a presence / absence raster
for (i in 1:length(obs_list)) {
  message("***** Forecasting for ", obs_list[[i]]$name, " [", 
          i, " of ", length(obs_list), " species] *****")
  obs_list[[i]][["future_probs"]] <- predict(gfdl_data, 
                                          obs_list[[i]][["svm_model"]]$model, 
                                          ext =  obs_list[[i]][["extent"]])
  obs_list[[i]][["future_pa"]] <- obs_list[[i]][["future_probs"]] > 
    obs_list[[1]]$svm_model$thresh
  
  # Save the p/a raster for later compilation of maps in output/distributions/
  pa_raster_file <- paste0("output/distributions/",
                           obs_list[[i]][["nice_name"]],
                           "-distribution-svm-GFDL-ESM4_RCP45.rds")
  saveRDS(object = obs_list[[i]][["future_pa"]],
          file = pa_raster_file)
  
  message(paste0("SVM model forecast predictions for ", obs_list[[i]][["name"]], 
                 " complete; saved to ", pa_raster_file))
}

# Plot the results of the forecast modeling
insect_pa <- obs_list[[insect]]$future_pa

# Create stack of host presence/absence rasters
hosts <- which(types == "host")
host_pa_rasters <- lapply(obs_list[hosts], "[[", "future_pa")
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
insect_title <- paste0(obs_list[[insect]]$name, " Forecast P/A")
plot(insect_pa, main = insect_title, col = c(NA, plot_colors[1]))
# Add the map
plot(wrld_simpl, 
     add = TRUE,
     border = "grey30")
# Plot plant
plot(host_pa_stack, main = "Forecast Host P/A", col = c(NA, NA, plot_colors[2]))
# Add the map
plot(wrld_simpl, 
     add = TRUE,
     border = "grey30")
plot(trio_stack, main = "Forecast Overlap", col = c(NA, plot_colors))
# Add the map
plot(wrld_simpl, 
     add = TRUE,
     border = "grey30")
par(mfrow = c(1, 1))