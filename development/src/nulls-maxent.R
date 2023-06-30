# Explore the use of null models to evaluate MAXENT models
# Erin Zylstra
# ezylstra@arizona.edu
# 2023-06-29

# Using the approach of Bohl et al. 2019 and Kass et al. 2020, which is now
# implemented in ENMeval 2.0 (Kass et al. 2021)

require(stringr)
require(ENMeval)
require(raster)
require(terra)
require(ggplot2)
require(dplyr)

# Identify which species to include (just insects for now) 
spp_data <- read.csv("data/gbif-pa-summary.csv", header = TRUE)
insect_data <- spp_data %>%
  filter(str_detect(species, "Papilio") & pa_csv == "yes") %>%
  select(species, n_filtered)

nice_names <- insect_data %>%
  select(species) %>%
  unlist() %>%
  str_replace(pattern = " ", replacement = "_") %>%
  tolower()

# Important!: Running models and null simulations for species with lots
# of records can take a very long time (eg, ENMevaluate() for P. cresphontes
# can take > 20 min; null simulations could take a couple hours or longer)

# For now, run one species at a time
nice_name <- nice_names[1]

  # Load in presence/absence data
  pa_file <- paste0("data/gbif/presence-absence/",
                    nice_name,
                    "-pa.csv")
  # If dataset isn't in presence-absence folder, unzip gbif-pa
  if (!file.exists(pa_file)) {
    unzip(zipfile = "data/gbif-pa.zip")
  }
  pa_data <- read.csv(file = pa_file)
  
  # Dataframes with lat/long (in that order) for occurrence and background locs
  occs <- pa_data %>%
    filter(pa == 1) %>%
    select(x, y)
  bg <- pa_data %>%
    filter(pa == 0) %>%
    select(x, y)
  
  # Grab worldclim data to use as predictors
  predictors <- rast(list.files(path = "data/wc2-1",
                                pattern = ".tif$",
                                full.names = TRUE))

  # Get shapefile for geographic extent (to crop predictor rasters)
  shapefile_name <- paste0("data/gbif/shapefiles/",
                           nice_name, 
                           "-buffered-mcp.shp")
  # If species' shapefile isn't in shapefiles folder, unzip gbif-shapefiles
  if (!file.exists(shapefile_name)) {
    unzip(zipfile = "data/gbif-shapefiles.zip")
  }
  buffered_mcp <- vect(shapefile_name)
  
  # Get list of climate variables to consider for the SDM
  all_climate_vars <- read.csv("data/climate-variables.csv")
  climate_vars <- all_climate_vars$variable[all_climate_vars$include == TRUE]
  
  # Extract only those climate variables we need
  predictors <- terra::subset(predictors, climate_vars)
  
  # Crop and mask predictor rasters
  pred_mask <- terra::crop(predictors, buffered_mcp)
  pred_mask <- terra::mask(pred_mask, buffered_mcp) 
  # Create RasterStack
  pred_rs <- raster::stack(pred_mask)
  
  # Not going to use our custom run_maxent_tune function because want to use  
  # ENMeval to create spatial blocks (k = 4) and explore other options in the
  # ENMevaluate() function
  
  # Look at spatial blocks created with ENM eval (creates 4 blocks via lat/long
  # with relatively even number of occurrences in each partition)
  block <- get.block(occs, bg)
  # Check number of occurrences in each block
  table(block$occs.grp)
  # Visualize blocks with occurrence data
  evalplot.grps(pts = occs, pts.grp = block$occs.grp, envs = pred_rs)
  # Visualize blocks with background data
  evalplot.grps(pts = bg, pts.grp = block$bg.grp, envs = pred_rs)
  
  # Evaluate how different the environment associated with each partition is 
  # from that of all the others using MESS. First we need to extract the 
  # predictor variable values at our occurrence and background localities.
  occs.z <- cbind(occs, raster::extract(pred_rs, occs))
  bg.z <- cbind(bg, raster::extract(pred_rs, bg))
  evalplot.envSim.hist(sim.type = "mess", 
                       ref.data = "occs", 
                       occs.z = occs.z, 
                       bg.z = bg.z, 
                       occs.grp = block$occs.grp, 
                       bg.grp = block$bg.grp)  

  # Set parameter values and arguments for ENMevaluate()
  feature_classes <- c("L", "LQ", "H", "LQH")
  multipliers <- 1:3
  tune.args <- list(fc = feature_classes, rm = multipliers)
  os <- list(validation.bg = "partition")  
    # By default, validation AUC is calculated with respect to the full 
    # background (training + validation). Assigning other.settings$validation.bg 
    # to "partition" will calculate AUC with respect to the validation 
    # background only (see Radosavljevic & Anderson_2014). Note that when we 
    # create nulls based on a model with this setting, we'll get some warnings, 
    # though I don't think it's relevant, and it doesn't seem to actually cause 
    # problems. Results are similar to those from a model with the default
    # (validation.bg = "full")
  
  # For parallel processing, use two fewer cores than are available
  num_cores <- parallel::detectCores() - 2  
  
  # Use maxent.jar:
  # mod_mj <- ENMevaluate(occs = occs, 
  #                       bg = bg, 
  #                       envs = pred_rs,
  #                       algorithm = "maxent.jar",
  #                       partitions = "block",
  #                       tune.args = tune.args,
  #                       other.settings = os,
  #                       parallel = TRUE,
  #                       numCores = num_cores)
  # mod_mj
  # mod_mj@results
  
  # Use maxnet:
  mod_mn <- ENMevaluate(occs = occs, 
                        bg = bg, 
                        envs = pred_rs,
                        algorithm = "maxnet",
                        partitions = "block",
                        tune.args = tune.args,
                        other.settings = os,
                        parallel = TRUE,
                        numCores = num_cores)
  mod_mn
  mod_mn@results
  # maxnet models have fewer estimated coefficients than maxent.jar models

  # Run null simulations with 100 iterations
  # For an example, picking best model based on 10% omission rate
  best_index <- which(mod_mn@results$or.10p.avg == min(mod_mn@results$or.10p.avg))
  best_settings <- list(fc = as.character(mod_mn@results$fc[best_index]),
                        rm = as.numeric(mod_mn@results$rm[best_index]))
  mod_null <- ENMnulls(e = mod_mn, 
                       mod.settings = best_settings,
                       no.iter = 100)

  # Summary table
  null.emp.results(mod_null) %>%
    mutate_if(is.numeric, round, 3)
  
  # Look at results from each iteration and each iteration and fold
  # null.results(mod_null) %>% head()
  # null.results.partitions(mod_null) %>% head()
  
  # Visualize results
  evalplot.nulls(mod_null, 
                 stats = c("or.10p", "auc.val"), 
                 plot.type = "histogram")

  
# Warning when running ENMnulls() with a model that had validation.bg = "partition":
  # Please make sure that the user-specified partitions are spatial, else 
  # validation.bg should be set to "full". The "partition" option only makes 
  # sense when partitions represent different regions of the study extent. 
# Not worried about this since we have partitions = "block"
  
