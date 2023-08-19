# Running new maxent models for butterfly species
# Erin Zylstra
# ezylstra@arizona.edu
# 2023-08-18

require(stringr)
require(ENMeval)
require(raster)
require(terra)
require(dplyr)
require(glmnet)

# Load up the functions from the functions folder
source(file = "load_functions.R")

# Will be creating new data partitions, so need to alter info in 
# presence-absence file

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

# Grab worldclim data to use as predictors
predictors <- terra::rast(list.files(path = "data/wc2-1",
                                     pattern = ".tif$",
                                     full.names = TRUE))

# For now, will use only 9 of the worldclim variables (this includes the 6 vars
# that Low et al. 2020 used, plus a few varaibles that assess seasonal or daily
# variation in temp/precip)
climate_vars <- paste0("bio", c(1, 2, 4:6, 12:15))

# Crop and mask predictor rasters
predictors <- terra::subset(predictors, climate_vars)

# Loop through species
# for (i in 1:nrow(insect_data)) {
i = 9
  nice_name <- nice_names[i]

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
  
  # Get shapefile for geographic extent (to crop predictor rasters)
  shapefile_name <- paste0("data/gbif/shapefiles/",
                           nice_name, 
                           "-buffered-mcp.shp")
  # If species' shapefile isn't in shapefiles folder, unzip gbif-shapefiles
  if (!file.exists(shapefile_name)) {
    unzip(zipfile = "data/gbif-shapefiles.zip")
  }
  buffered_mcp <- vect(shapefile_name)
  
  # Crop and mask predictor rasters
  pred_mask <- terra::crop(predictors, buffered_mcp)
  pred_mask <- terra::mask(pred_mask, buffered_mcp) 
  # Create RasterStack
  pred_rs <- raster::stack(pred_mask)

  # Create spatial blocks with ENM eval (creates 4 blocks via lat/long
  # with relatively even number of occurrences in each partition)
  block <- get.block(occs, bg)
  # Check number of occurrences, bg points in each block
    table(block$occs.grp)
    table(block$bg.grp)
  # Attach block numbers to pa data (doing this to show how it should be 
  # implemented in the src/data/gbif-3-presence-absence.R)
  pa_data$block <- c(block$occs.grp, block$bg.grp)
  
  # Visualize blocks
    # plot(pred_rs[[1]])
    # points(y ~ x, data = filter(pa_data, block == 1 & pa == 1),
    #        pch = 19, col = "red")
    # points(y ~ x, data = filter(pa_data, block == 2 & pa == 1),
    #        pch = 19, col = "blue")
    # points(y ~ x, data = filter(pa_data, block == 3 & pa == 1),
    #        pch = 19, col = "green")
    # points(y ~ x, data = filter(pa_data, block == 4 & pa == 1),
    #        pch = 19, col = "purple")

  # Maxent --------------------------------------------------------------------#
  # Set parameter values and arguments for ENMevaluate()
  feature_classes <- c("L", "LQ", "H", "LQH")
  multipliers <- 1:3
  tune.args <- list(fc = feature_classes, rm = multipliers)
  user.grp <- list(occs.grp = pa_data$block[pa_data$pa == 1],
                   bg.grp = pa_data$block[pa_data$pa == 0])
  os <- list(validation.bg = "partition")  
  # Assigning other.settings$validation.bg to "partition" will calculate AUC 
  # with respect to the validation background only 
  # (see Radosavljevic & Anderson 2014). 
  
  # For parallel processing, use two fewer cores than are available
  num_cores <- parallel::detectCores() - 2  
  
  # Run maxent models:
  mod_max <- ENMevaluate(occs = occs, 
                         bg = bg, 
                         envs = pred_rs,
                         algorithm = "maxnet",
                         partitions = "user",
                         user.grp = user.grp,
                         tune.args = tune.args,
                         other.settings = os,
                         parallel = TRUE,
                         numCores = num_cores)
  
  model_eval <- evaluate_lasso(p = as.matrix(sdmtest[sdmtest$pa == 1, -1]), 
                               a = as.matrix(sdmtest[sdmtest$pa == 0, -1]), 
                               model = model_fit,
                               s = model_fit$lambda.1se,
                               type = "response")
  
    # Look at results
    # mod_max@results
    # mod_max@results.partitions
  
  # Save to file
  sdm_file <- paste0("development/output/SDMs/",
                     nice_name, "-sdm-maxent-9var-CV.rds")
  saveRDS(mod_max, sdm_file)
  
  # Prep data for all other SDMs ----------------------------------------------#  
  predictors_df <- terra::extract(x = pred_mask,
                                  y = pa_data[, c("x", "y")],
                                  xy = FALSE) %>%
    dplyr::select(-ID)
  pa_data <- cbind(pa_data, predictors_df) 
  
  # LASSO ---------------------------------------------------------------------#
  # Loop through each training dataset
  for (j in 1:4) {
    sdmtrain <- pa_data %>%
      filter(block != j)
    sdmtest <- pa_data %>%
      filter(block == j)
    
    # Calculate means, SDs for standardizing covariates
    stand_obj <- save_means_sds(sdmtrain, cols = climate_vars, verbose = TRUE)
    # Standardize values in training dataset (to include quadratics, set quad = TRUE)
    sdmtrain_preds <- prep_predictors(stand_obj, sdmtrain, quad = TRUE) 
    sdmtrain <- cbind(pa = sdmtrain[,1], sdmtrain_preds)
    
    # Creating values to downweight background points (so total [summed] weight of 
    # background points is equal to the total weight of presence points)
    prNum <- sum(sdmtrain$pa == 1)
    bgNum <- sum(sdmtrain$pa == 0)
    wt <- ifelse(sdmtrain$pa == 1, 1, prNum / bgNum)
    
    # Run model
    model_fit <- glmnet::cv.glmnet(x = as.matrix(sdmtrain[,2:ncol(sdmtrain)]),
                                   y = sdmtrain$pa,
                                   family = "binomial",
                                   alpha = 1,
                                   weights = wt,
                                   standardize = FALSE)

    # Need an easy way to calculate evaluation metrics:
    
    # Prep testing dataset
    sdmtest_preds <- prep_predictors(stand_obj, sdmtest, quad = TRUE) 
    sdmtest <- cbind(pa = sdmtest[,1], sdmtest_preds)

    # Evaluate model performance with testing data
    model_eval <- evaluate_lasso(p = as.matrix(sdmtest[sdmtest$pa == 1, -1]), 
                                 a = as.matrix(sdmtest[sdmtest$pa == 0, -1]), 
                                 model = model_fit,
                                 s = model_fit$lambda.1se,
                                 type = "response")
    
    library(flexsdm)
    test <- sdm_eval(p = model_eval@presence,
                     a = model_eval@absence,
                     thr = "max_sens_spec")

    
    
  }
  

  
#------------------------------------------------------------------------------#
# Finding best model and evaluating (just for Maxent right now)
#------------------------------------------------------------------------------#
  
  # For each model, extract max or.10p.val and min AUC.val over 4 folds
    partition_summary <- mod_max@results.partitions %>%
      group_by(tune.args) %>%
      summarize(auc.val.min = min(auc.val),
                or.10p.max = max(or.10p),
                cbi.val.min = min(cbi.val)) %>%
      data.frame()
    maxent_evals <- left_join(mod_max@results[, c(1:5, 8:9, 10:13, 16, 19)],
                                partition_summary,
                                by = "tune.args") %>%
      relocate(or.10p.max, .after = or.10p.sd) %>%
      relocate(auc.val.min, .after = auc.val.sd) %>%
      relocate(cbi.val.min, .after = cbi.val.sd)

  # Pick best maxent model (tuning parameters) and save evaluation metrics
  # Use minimum average or.10p (and maximum average auc.val to break ties)
  optimal <- maxent_evals %>% 
    filter(or.10p.avg == min(or.10p.avg)) %>%
    filter(auc.val.avg == max(auc.val.avg))
  best_model <- mod_max@models[[optimal$tune.args]]
  
  # Make predictions and extract values for occurrence and bg locations
  preds <- predict(object = pred_rs,
                   model = best_model,
                   type = "cloglog")
  est_occ <- extract(preds, pa_data[pa_data$pa == 1, c("x", "y")])
  est_bg <- extract(preds, pa_data[pa_data$pa == 0, c("x", "y")])
  # Use evaluate function to get confusion matrices and calculate threshold
  # Note that AUC from dismo::evaluate() = auc.train in ENMeval output
  ev <- dismo::evaluate(est_occ, est_bg)
  thr <- dismo::threshold(ev, stat = "spec_sens")
  
  # Visualize
  par(mfrow = c(2, 1))
  plot(preds)
  plot(preds > thr)
