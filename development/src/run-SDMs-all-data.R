# Run SDMs using all data
# Erin Zylstra
# ezylstra@arizona.edu
# 2023-09-07

require(stringr)
require(ENMeval)
require(raster)
require(terra)
require(dplyr)
require(flexsdm)
require(ecospat)
require(glmnet)
require(mgcv)
require(randomForest)
require(dismo)
require(gbm)

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
# that Low et al. 2020 used, plus a few variables that assess seasonal or daily
# variation in temp/precip)
climate_vars <- paste0("bio", c(1, 2, 4:6, 12:15))

# Subset climate variables
predictors <- terra::subset(predictors, climate_vars)

# Load evaluation metrics from CV models
evals <- read.csv("development/output/evals-CV-insect.csv", header = TRUE)

# Looping through each species
for (i in 1:nrow(insect_data)) {
  nice_name <- nice_names[i]
  insect <- insect_data$species[i]
  
  # Load in presence/absence data
  pa_file <- paste0("data/gbif/presence-absence/",
                    nice_name,
                    "-pa.csv")
  # If dataset isn't in presence-absence folder, unzip gbif-pa
  if (!file.exists(pa_file)) {
    unzip(zipfile = "data/gbif-pa.zip")
  }
  pa_data <- read.csv(file = pa_file)
  
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
  # Create RasterStack (needed for maxent)
  pred_rs <- raster::stack(pred_mask) 
  
  # Extract value of predictors at each pres/bg location (needed for all except
  # maxent models)
  predictors_df <- terra::extract(x = pred_mask,
                                  y = pa_data[, c("x", "y")],
                                  xy = FALSE) %>%
    dplyr::select(-ID)
  pa_data <- cbind(pa_data, predictors_df)
  # Remove any rows with NAs
  anyNAs <- apply(pa_data[, climate_vars], 1, function(x) sum(is.na(x)))
  if (max(anyNAs) > 0) {
    pa_data <- pa_data[-which(anyNAs > 0),]
  }
  
  # Dataframes with lat/long (in that order) for occurrence and background locs
  occs <- pa_data %>%
    filter(pa == 1) %>%
    select(x, y)
  bg <- pa_data %>%
    filter(pa == 0) %>%
    select(x, y)
  
  # Create spatial blocks with ENM eval (creates 4 blocks via lat/long
  # with relatively even number of occurrences in each partition)
  set.seed(1234)
  block <- get.block(occs, bg)
  # Check number of occurrences, bg points in each block
  table(block$occs.grp)
  table(block$bg.grp)
  # Attach block numbers to pa data (doing this to show how it should be 
  # implemented in the src/data/gbif-3-presence-absence.R)
  pa_data$block <- c(block$occs.grp, block$bg.grp)
  
  # Calculate means, SDs for standardizing covariates
  stand_obj <- save_means_sds(pa_data, cols = climate_vars, verbose = TRUE)
  
  # Standardize values with quadratics
  pa_data_zq <- prep_predictors(stand_obj, pa_data, quad = TRUE) 
  pa_data_zq <- cbind(pa = pa_data[,1], pa_data_zq)
  
  # Standardize values without quadratics
  pa_data_z <- prep_predictors(stand_obj, pa_data, quad = FALSE) 
  pa_data_z <- cbind(pa = pa_data[,1], pa_data_z)
  
  # Creating values to downweight background points (so total [summed] weight of 
  # background points is equal to the total weight of presence points)
  prNum <- as.numeric(table(pa_data$pa)["1"])
  bgNum <- as.numeric(table(pa_data$pa)["0"])
  wt <- ifelse(pa_data$pa == 1, 1, prNum / bgNum)
  
  # Run (and save) BRT with all the data
    cat(paste0("Running BRT model for ", insect, ".\n"))
    brt_settings <- evals %>%
      filter(insect == insect_data$species[i], sdm == "BRT", fold == 1) %>%
      select(tune.args)
    tc <- as.numeric(str_sub(brt_settings, 16, 16))
    n_trees <- as.numeric(str_sub(brt_settings, 6, 9))
    lr <- as.numeric(str_sub(brt_settings, 21, nchar(brt_settings)))
    pred_columns <- which(colnames(pa_data) %in% climate_vars)
    brt_fit <- dismo::gbm.fixed(data = pa_data, 
                                gbm.x = pred_columns,
                                gbm.y = 1,
                                family = "bernoulli",
                                tree.complexity = tc,
                                learning.rate = lr,
                                n.trees = n_trees,
                                bag.fraction = 0.75,
                                verbose = FALSE)
    # Bind things together and return as list  
    brt_results <- list(model = brt_fit,
                        trees = n_trees,
                        climate_vars = climate_vars)
    # Save object to file
    brt_file <- paste0("development/output/SDMs/", nice_name, "-sdm-brt-9var.rds")
    saveRDS(brt_results, brt_file)
  
  # Run (and save) GAM with all the data
    cat(paste0("Running GAM model for ", insect, ".\n"))
    smooth <- "s"
    model_formula <- paste0(smooth, "(", climate_vars, "_1)")
    model_formula <- paste(model_formula, collapse = " + ")
    model_formula <- as.formula(paste0("pa ~ ", model_formula))  
    gam_fit <- mgcv::gam(model_formula,
                         data = pa_data_z,
                         family = binomial, 
                         method = 'REML', 
                         select = TRUE)
    # Bind things together and return as list  
    gam_results <- list(model = gam_fit,
                        standardize_objects = stand_obj,
                        quad = FALSE,
                        climate_vars = climate_vars)
    # Save object to file
    gam_file <- paste0("development/output/SDMs/", nice_name, "-sdm-gam-9var.rds")
    saveRDS(gam_results, gam_file)    
    
  # Run (and save) LASSO with all the data
    cat(paste0("Running LASSO model for ", insect, ".\n"))
    lasso_fit <- glmnet::cv.glmnet(x = as.matrix(pa_data_zq[,2:ncol(pa_data_zq)]),
                                   y = pa_data_zq$pa,
                                   family = "binomial",
                                   alpha = 1,
                                   weights = wt,
                                   standardize = FALSE)
    # Bind things together and return as list  
    lasso_results <- list(model = lasso_fit,
                          standardize_objects = stand_obj,
                          quad = TRUE,
                          climate_vars = climate_vars)
    # Save object to file
    lasso_file <- paste0("development/output/SDMs/", nice_name, "-sdm-lasso-9var.rds")
    saveRDS(lasso_results, lasso_file)  
    
  # Load Maxent object and save as a list (so it looks like other types of SDMs)
    cat(paste0("Saving MAXENT model for ", insect, ".\n"))
    max_file <- paste0("development/output/SDMs/", nice_name, "-sdm-maxent-9var.rds")
    max_model <- readRDS(max_file)
    max_results <- list(model = max_model,
                        climate_vars = climate_vars)
    # Save object to file
    saveRDS(max_results, max_file)  
    
  # Run (and save) RF with all the data
    cat(paste0("Running RF model for ", insect, ".\n"))
    model_formula <- paste(climate_vars, collapse = " + ")
    model_formula <- as.formula(paste0("pa ~ ", model_formula))
    # Generate equal sample sizes for presence, pseudo-absence data. Note that 
    # if we have more presences than absences in a fold (happens rarely), need 
    # to then sample the presences.
    if (prNum < bgNum) {
      smpsize <- c("0" = prNum, "1" = prNum)
    } else {
      smpsize <- c("0" = bgNum, "1" = bgNum)
    } 
    pa_data_f <- pa_data
    pa_data_f$pa <- as.factor(pa_data_f$pa)  
    rf_fit <- randomForest::randomForest(formula = model_formula,
                                         data = pa_data_f,
                                         ntree = 1000,
                                         sampsize = smpsize,
                                         replace = TRUE)
    # Bind everything together and return as list  
    rf_results <- list(model = rf_fit,
                       climate_vars = climate_vars)
    # Save object to file
    rf_file <- paste0("development/output/SDMs/", nice_name, "-sdm-rf-9var.rds")
    saveRDS(rf_results, rf_file)   
}
