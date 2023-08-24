# Evaluating different thresholds for final selection criterion
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-08-24

# Rationale: how do we determine which models are appropriate to use for 
# predictions? May be interesting to look at different threshold metrics for 
# this purpose and decide on one of those + an omission rate. That is, are 
# there thresholds to use on the final model that we can then use with a 
# criteron for saying whether or not the model gets to be used for subsequent 
# predictions? e.g. using the max_spec_sens (threshold at which the sum of 
# sensitivity and specificity is maximized), require an omission rate of 10% or 
# less on all data?

# Worked POC on rumiko below

# TODO: Next steps would be to 
#    (1) Figure out how to estimate final model coefficients the right way
#    (2) Figure out criteria for selecting the right threshold criteria (think 
#        about what each threshold is and what our purposes are)
#    (3) Consider a (separate?) criterion that considers suitability for those 
#        pseudo-absence points, too

library(dplyr)
library(terra)
library(predicts) # To evaluate model (and get thresholds)
library(mgcv)     # For the actual GAM model
source(file = "load_functions.R")

# 1. Try with a gam on rumiko
rumiko_obs <- read.csv(file = "data/gbif/presence-absence/papilio_rumiko-pa.csv")

# Need to add in climate data for SDMs
predictors <- terra::rast(list.files(path = "data/wc2-1",
                                     pattern = ".tif$",
                                     full.names = TRUE))
# Extract bioclim data for presence/absence data; can take a moment
predictors <- terra::extract(x = predictors, 
                             y = rumiko_obs[, c("x", "y")], 
                             xy = FALSE) %>%
  dplyr::select(-ID)

# Join bioclim data with original full_data (which has pa and fold info),
# dropping x, y columns at the same time
rumiko_obs <- rumiko_obs %>%
  cbind(., predictors) %>%
  dplyr::select(c("pa", "fold", all_of(paste0("bio", 1:19))))

# For now, will use only 9 of the worldclim variables (this includes the 6 vars
# that Low et al. 2020 used, plus a few variables that assess seasonal or daily
# variation in temp/precip)
climate_vars <- paste0("bio", c(1, 2, 4:6, 12:15))

# Create separate data frames for testing and training presence data
presence_train <- rumiko_obs %>%
  filter(pa == 1) %>%
  filter(fold != 1) %>%
  dplyr::select(pa, fold, all_of(climate_vars))
presence_test <- rumiko_obs %>%
  filter(pa == 1) %>%
  filter(fold == 1) %>%
  dplyr::select(all_of(climate_vars))
# Create separate data frames for testing and training (pseudo)absence data
absence_train <- rumiko_obs %>%
  filter(pa == 0) %>%
  filter(fold != 1) %>%
  dplyr::select(pa, fold, all_of(climate_vars))
absence_test <- rumiko_obs %>%
  filter(pa == 0) %>%
  filter(fold == 1) %>%
  dplyr::select(all_of(climate_vars))

# Add presence and pseudoabsence training data into single data frame
sdmtrain <- rbind(presence_train, absence_train)

# Calculate (and save) means, SDs for standardizing covariates
stand_obj <- save_means_sds(sdmtrain, cols = climate_vars, verbose = TRUE)
# Standardize values in training dataset
sdmtrain_preds <- prep_predictors(stand_obj, sdmtrain, quad = FALSE) 
sdmtrain <- cbind(sdmtrain[,1:2], sdmtrain_preds)

# Select type of smooth functions
smooth <- "s"

# Create model formula
model_formula <- paste0(smooth, "(", climate_vars, "_1)")
model_formula <- paste(model_formula, collapse = " + ")
model_formula <- as.formula(paste0("pa ~ ", model_formula))

# Model below adds a double penalty to remove variables that don't add to the 
# model
model_fit <- mgcv::gam(model_formula,
                       data = sdmtrain,
                       family = binomial, 
                       method = 'REML', 
                       select = TRUE)

# Model is done, get ready for evaluation
# Prep testing dataset
presence_test <- prep_predictors(stand_obj, presence_test, quad = FALSE) 
absence_test <- prep_predictors(stand_obj, absence_test, quad = FALSE)

# Model evaluation
model_eval <- predicts::pa_evaluate(p = presence_test,
                                    a = absence_test,
                                    model = model_fit,
                                    type = "response")

# Extract the thresholds as a vector
model_thresh <- unlist(model_eval@thresholds[1, ])

# Go ahead and make predictions of suitability for all presence points
# Start with creating a presence-only dataset
presence_all <- rumiko_obs %>%
  filter(pa == 1) %>%
  dplyr::select(pa, fold, all_of(climate_vars))
presence_all <- prep_predictors(stand_obj, presence_all, quad = FALSE)

# TODO: This next step (predicting suitability values) would need attention, as 
# it currently uses only one of the k-fold models; ideally it would be a model 
# with coefficients estimated from all k models, or on all data (if that is the 
# way we go)

# Now make predictions for each of those points (predict.gam returns an array 
# by default, hence as.vector)
model_predict <- as.vector(predict(object = model_fit,
                                   newdata = presence_all,
                                   type = "response"))

# We now have vector of suitability values for each of our presence points. 
# Next steps are to find out whether value is >= each threshold then calculate 
# frequency of omissions for each threshold
booleans <- sapply(X = model_predict,
                   FUN = function(x) {
                     return(x >= model_thresh)
                   })
# Have to transpose if we want thresholds as columns (not necessary, but a 
# little easier to read & do reality checks)
booleans <- t(booleans)
colnames(booleans) <- names(model_thresh)

# Now calculate % omissions
omission_perc <- 1 - apply(X = booleans,
                           MARGIN = 2,
                           FUN = sum)/nrow(booleans)

# A little reality check - should see higher omission rates for higher 
# thresholds
plot(x = model_thresh,
     y = omission_perc,
     pch = 19)