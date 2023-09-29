# A template for tuning Maxent and BRT models and using CV to evaluate SDMs
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-09-27

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

rm(list = ls())

# Load up the functions from the functions folder
source(file = "load_functions.R")

# Logical to indicate whether to save ENMeval object that contains output from
# all models (and not just the model with "optimal" tuning parameters)
max_save <- FALSE

# genus <- "GENUS"
# species <- "SPECIES"
genus <- "Papilio"
species <- "appalachiensis"

# Name for reporting and looking up info in files
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

# Load in presence/pseudo-absence data
pa_file <- paste0("data/gbif/presence-absence/", nice_name, "-pa.csv")
# Check to see if file exists and what to do if not
# if (!file.exists(pa_file)) {
#   unzip(zipfile = "data/gbif-pa.zip")
# }
pa_data <- read.csv(file = pa_file)

# Get shapefile for geographic extent (to crop predictor rasters)
shapefile_name <- paste0("data/gbif/shapefiles/",
                         nice_name, 
                         "-buffered-mcp.shp")
# If species' shapefile isn't in shapefiles folder, unzip gbif-shapefiles
# if (!file.exists(shapefile_name)) {
#   unzip(zipfile = "data/gbif-shapefiles.zip")
# }
buffered_mcp <- vect(shapefile_name)

# Grab worldclim data to use as predictors
predictors <- terra::rast(list.files(path = "data/wc2-1",
                                     pattern = ".tif$",
                                     full.names = TRUE))

# Extract a subset of climate variables
all_climate_vars <- read.csv("data/climate-variables.csv", header = TRUE)
climate_vars <- all_climate_vars$variable[all_climate_vars$include == TRUE]
predictors <- terra::subset(predictors, climate_vars)

# Crop and mask predictor rasters
pred_mask <- terra::crop(predictors, buffered_mcp)
pred_mask <- terra::mask(pred_mask, buffered_mcp)  
# Create RasterStack (needed for MAXENT model)
pred_rs <- raster::stack(pred_mask) 

# Extract value of predictors at each presence/background location (needed for 
# all SDMs except MAXENT). 
  # For now, going through some extra steps to ensure that all presence and 
  # background point have climate data and no two presence or background points 
  # occur in the same raster cell. Once we've updated gbif-2-filter.R, we 
  # shouldn't need these checks and fixes.
predictors_df <- terra::extract(x = pred_mask,
                                y = pa_data[, c("x", "y")],
                                xy = FALSE,
                                cells = TRUE) %>%
  dplyr::select(-ID)
pa_data <- cbind(pa_data, predictors_df)
# Remove any rows with NAs
anyNAs <- apply(pa_data[, climate_vars], 1, function(x) sum(is.na(x)))
if (max(anyNAs) > 0) {
  pa_data <- pa_data[-which(anyNAs > 0),]
}
pa_data <- pa_data %>%
  distinct(pa, cell, .keep_all = TRUE) %>%
  select(-cell)

# Create a table to store evaluation metrics for each SDM and fold
sdms <- c("BRT", "GAM", "LASSO", "MAXENT", "RF")
nfolds <- max(pa_data$fold)
folds <- 1:nfolds
evals <- data.frame(sdm = rep(sdms, each = max(folds)),
                    tune.args = NA,
                    fold = rep(folds, length(sdms)),
                    AUC = NA,
                    CBI = NA,
                    threshold = NA,
                    OR = NA,
                    TSS = NA)

set.seed(20230927)

# Run MAXENT models -----------------------------------------------------------#

cat(paste0("Running Maxent models for ", species_name, ".\n"))

# TODO: move some of code below to the run_maxent function? The extent to which
# this happens depends on how much of the code we created above (to ensure that
# all SDMs are based on the exact same data) is needed.

# Dataframes with lat/long (in that order) for presence and background locations
occs <- pa_data %>%
  filter(pa == 1) %>%
  select(x, y)
bg <- pa_data %>%
  filter(pa == 0) %>%
  select(x, y)

# ENMeval settings
feature_classes <- c("L", "LQ", "H", "LQH")
multipliers <- 1:3
tune.args <- list(fc = feature_classes, rm = multipliers)
user.grp <- list(occs.grp = pa_data$fold[pa_data$pa == 1],
                 bg.grp = pa_data$fold[pa_data$pa == 0])
os <- list(validation.bg = "partition",
           pred.type = "cloglog")

# Calculate a few other evaluation metrics, so we can compare model performance
# with other SDMs. (Note that the CBI value ENMevaluate spits out is the same
# as what's produced with the ecospat package, with fit = predictions from
# occurrence and bg points in validation area, and nclass = 0.)
  # TSS = maxTSS
  # OR.mss = omission rate with the max(sens + spec) threshold
  # thr = max(sens + spec) threshold value
em <- function(vars) {
  em <- flexsdm::sdm_eval(p = vars$occs.val.pred, 
                          a = vars$bg.val.pred,
                          thr = "max_sens_spec")
  out <- data.frame(TSS = em$TSS,
                    OR.mss = em$OR,
                    thr = em$thr_value)
  return(out)
}

# For parallel processing, use two fewer cores than are available
num_cores <- parallel::detectCores() - 2  

# Run maxent models:
max_models <- ENMevaluate(occs = occs, 
                          bg = bg, 
                          envs = pred_rs,
                          algorithm = "maxnet",
                          partitions = "user",
                          user.grp = user.grp,
                          tune.args = tune.args,
                          other.settings = os,
                          user.eval = em,
                          parallel = TRUE,
                          numCores = num_cores)

# Save ENMeval object to file (contains 12 models with different tuning params)
if (max_save) {
  sdm_file <- paste0("output/SDMs/", nice_name, "-maxent-tuning.rds")
  saveRDS(max_models, sdm_file)
}

# Identify optimal tuning parameters
other_summaries <- max_models@results.partitions %>%
  group_by(tune.args) %>%
  summarize(n.partitions = length(fold)) %>%
  data.frame()
# Occasionally, there are no results for one fold (maybe the model didn't
# run ok?). Should remove this model from consideration. 
optimal <- max_models@results %>% 
  left_join(., other_summaries, by = "tune.args") %>%
  filter(n.partitions == 4)
# If all mean CBIs are negative (which is very rare), pick the model with the 
# highest mean CBI. If at least one mean CBI is positive, eliminate models with 
# negative mean CBIs. Of those remaining, use minimum average or.10p. Break any
# ties by selecting model with the maximum average auc.val
if (max(optimal$cbi.val.avg) <= 0) {
  optimal <- optimal %>%
    filter(cbi.val.avg == max(cbi.min)) %>%
    filter(or.10p.avg == min(or.10p.avg)) %>%
    filter(auc.val.avg == max(auc.val.avg))
} else {
  optimal <- optimal %>%
    filter(cbi.val.avg > 0) %>%
    filter(or.10p.avg == min(or.10p.avg)) %>%
    filter(auc.val.avg == max(auc.val.avg))
}

# Save best model to file
max_best <- max_models@models[[optimal$tune.args]]
max_file <- paste0("output/SDMs/", nice_name, "-maxent.rds")
saveRDS(max_best, max_file)  

# Extract evaluation metrics for the model with optimal tuning parameters
partitions <- max_models@results.partitions %>%
  filter(tune.args == optimal$tune.args)
eval_rows <- which(evals$sdm == "MAXENT")
evals$tune.args[eval_rows] <- as.character(partitions$tune.args)
evals$AUC[eval_rows] <- partitions$auc.val
evals$CBI[eval_rows] <- partitions$cbi.val
evals$threshold[eval_rows] <- partitions$thr
evals$OR[eval_rows] <- partitions$OR.mss
evals$TSS[eval_rows] <- partitions$TSS

# Tuning BRT models -----------------------------------------------------------#
# We're running a BRT model for each set of training data, identifying the 
# optimal learning rate and number of trees. We'll use the mean of these
# values when running CV models. 

cat(paste0("Tuning BRT models for ", species_name, ".\n"))

# Create empty vectors to hold optimal tuning parameters for each fold
learningrate_cv <-  rep(NA_real_, 4)
ntrees_cv <- rep(NA_integer_, 4)

# Set tree complexity
complexity <- 5

for (k in 1:nfolds) {
  sdmtrain <- pa_data %>%
    filter(fold != k)

  brt_fit <- run_brt(full_data = sdmtrain, step = TRUE, 
                     complexity = complexity, verbose = FALSE)  
  
  if (!is.null(brt_fit)) {
    ntrees_cv[k] <- brt_fit$gbm.call$best.trees
    learningrate_cv[k]<- brt_fit$gbm.call$learning.rate
  }
}    

# Identify best tuning parameters across folds
# (Most of the time the learning rate [lr] will be the same across folds. If
# it's not, then average only over those folds with the smaller lr)
lr_min <- min(learningrate_cv)
ntrees_cv <- ntrees_cv[which(learningrate_cv == lr_min)]
ntrees <- mean(ntrees_cv)

# Run CV models for BRT, GAM, LASSO, RF ---------------------------------------#

cat(paste0("Running CV models for ", species_name, ".\n"))

for (k in 1:nfolds) {
  # Create training and testing datasets for all SDMs
  sdmtrain <- pa_data %>%
    filter(fold != k)
  sdmtest <- pa_data %>%
    filter(fold == k)  
  
  # Calculate means, SDs for standardizing covariates
  stand_obj <- save_means_sds(sdmtrain, cols = climate_vars, verbose = TRUE)
  
  # TODO: Move stuff below to run_gam (_z) and run_lasso (_zq) functions
    # Create training/testing datasets with standardized variables
    sdmtrain_z <- prep_predictors(stand_obj, sdmtrain, quad = FALSE) 
    sdmtrain_z <- cbind(pa = sdmtrain$pa, sdmtrain_z)
    # Create training/testing datasets with linear and quadratic standardized variables
    sdmtrain_zq <- prep_predictors(stand_obj, sdmtrain, quad = TRUE) 
    sdmtrain_zq <- cbind(pa = sdmtrain$pa, sdmtrain_zq)

  # Run BRT and evaluate with test data
  brt_fit <- run_brt(full_data = sdmtrain, step = FALSE, ntrees = ntrees,
                     complexity = complexity, learning.rate = lr_min,
                     verbose = TRUE)
  
  if (!is.null(brt_fit)) {
    
    ev <- evaluate_sdm(test_data = sdmtest, 
                       model = brt_fit, 
                       sdm_method = "brt") 

    row_index <- which(evals$sdm == "BRT" & evals$fold == k)  
    tune.args <- paste0("tree.", ntrees, "_node.", complexity, "_lr.", lr_min)
    evals$tune.args[row_index] <- tune.args
    evals$AUC[row_index] <- ev$auc
    evals$CBI[row_index] <- ev$cbi
    evals$threshold[row_index] <- ev$threshold
    evals$OR[row_index] <- ev$or
    evals$TSS[row_index] <- ev$tss 
    # TODO: see whether there's a more efficient way to dump in table (create
    # a row and rbind it to existing?)
  }
  
  # Run GAM and evaluate with test data
  gam_fit <- run_gam(full_data = sdmtrain, 
                     stand_obj = stand_obj,
                     quad = FALSE, ....)
  
  ev <- evaluate_sdm(test_data = sdmtest, 
                     model = gam_fit, 
                     sdm_method = "gam",
                     stand_obj = stand_obj,
                     quad = FALSE) 
    
  row_index <- which(evals$sdm == "GAM" & evals$fold == k)  
  evals$AUC[row_index] <- ev$auc
  evals$CBI[row_index] <- ev$cbi
  evals$threshold[row_index] <- ev$threshold
  evals$OR[row_index] <- ev$or
  evals$TSS[row_index] <- ev$tss 
  
  # Run LASSO and evaluate with test data
  traindata <- sdmtrain_zq
  testdata <- sdmtest_zq
  
  lasso_fit <- run_lasso(full_data = sdmtrain, 
                         stand_obj = stand_obj,
                         quad = TRUE, ....)
  
  ev <- evaluate_sdm(test_data = testdata, 
                     model = lasso_fit, 
                     sdm_method = "lasso",
                     stand_obj = stand_obj,
                     quad = TRUE, ....) 
  
  row_index <- which(evals$sdm == "LASSO" & evals$fold == k)  
  evals$AUC[row_index] <- ev$auc
  evals$CBI[row_index] <- ev$cbi
  evals$threshold[row_index] <- ev$threshold
  evals$OR[row_index] <- ev$or
  evals$TSS[row_index] <- ev$tss 
  
  # Run RF and evaluate with test data
  rf_fit <- run_rf(full_data = sdmtrain..)
  
  ev <- evaluate_sdm(test_data = sdmtest, 
                     model = rf_fit, 
                     sdm_method = "rf") 
  
  row_index <- which(evals$sdm == "RF" & evals$fold == k)  
  evals$AUC[row_index] <- ev$auc
  evals$CBI[row_index] <- ev$cbi
  evals$threshold[row_index] <- ev$threshold
  evals$OR[row_index] <- ev$or
  evals$TSS[row_index] <- ev$tss 
  
}


