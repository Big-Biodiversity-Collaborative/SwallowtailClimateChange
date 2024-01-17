# Variable importance
# Can we access this information from previously run SDMs (that are mostly 
# stored on the HPC)?

require(stringr)
require(raster)
require(terra)
require(ENMeval)
require(dplyr)
require(glmnet)
require(mgcv)
require(randomForest)
require(dismo)
require(gbm)

# It helps to install this package for calculating variable importance measures
# for maxent models that used the maxnet algorithm:
# remotes::install_github("peterbat1/fitMaxnet")
require(fitMaxnet)

# Load up the functions from the functions folder
source(file = "load_functions.R")

# Identify species of interest
genus <- "Papilio"
species <- "rumiko"
nice_name <- paste0(tolower(genus), "_", tolower(species))

# Get pres-abs locations and climate values
  pa_file <- paste0("data/gbif/presence-absence/", nice_name, "-pa.csv")
  pa_data <- read.csv(file = pa_file)

  # Grab worldclim data to use as predictors
  predictors <- terra::rast(list.files(path = "data/wc2-1",
                                       pattern = ".tif$",
                                       full.names = TRUE))
  
  # Extract a subset of climate variables
  all_climate_vars <- read.csv("data/climate-variables.csv", header = TRUE)
  climate_vars <- all_climate_vars$variable[all_climate_vars$include == TRUE]
  predictors <- terra::subset(predictors, climate_vars)

  # Extract value of predictors at each presence/background location 
  p <- pa_data %>%
    filter(pa == 1) %>%
    dplyr::select(x, y)
  predictors_p <- terra::extract(x = predictors, y = p, xy = FALSE) %>%
    dplyr::select(-ID) %>%
    mutate(Species = "Papilio rumiko",
           x = p$x,
           y = p$y) %>%
    relocate(Species, x, y)
  a <- pa_data %>%
    filter(pa == 0) %>%
    dplyr::select(x, y)
  predictors_a <- terra::extract(x = predictors, y = a, xy = FALSE) %>%
    dplyr::select(-ID) %>%
    mutate(Species = "Papilio rumiko",
           x = a$x,
           y = a$y) %>%
    relocate(Species, x, y)

# Load SDMs
maxent_list <- readRDS(paste0("output/SDMs/", nice_name, "-maxent.rds"))
brt_list <- readRDS(paste0("output/SDMs/", nice_name, "-brt.rds"))
gam_list <- readRDS(paste0("output/SDMs/", nice_name, "-gam.rds"))
lasso_list <- readRDS(paste0("output/SDMs/", nice_name, "-lasso.rds"))
rf_list <- readRDS(paste0("output/SDMs/", nice_name, "-rf.rds"))

mx <- maxent_list$model
brt <- brt_list$model
gam <- gam_list$model
lasso <- lasso_list$model
rf <- rf_list$model

# Load evaluation file
evals <- read.csv(paste0("output/eval-metrics/", nice_name, "-CVevals.csv"))

# Getting variable importance for Maxnet model (which is apparently NOT
# straightforward like it is for Maxent algorithms)
  max_vi <- fitMaxnet::varImportance(theModel = mx,
                                     occSWD = predictors_p,
                                     bkgSWD = predictors_a,
                                     responseType = "cloglog",
                                     numReplicates = 100)
  # This uses permutations. For each variable, values are permuted between rows
  # and a model prediction made for each row used the permuted data. For each 
  # permutation, a Pearson correlation is computed between reference predictions
  # and the predicted value from the shuffled data. Importance score is 
  # 1 - correlation coefficient. 
  max_vi <- data.frame(var = names(max_vi), max_vi)

# Getting variable importance for BRT model
  brt_vi <- summary.gbm(brt, plotit = FALSE) %>%
    arrange(match(var, climate_vars)) %>%
    rename(brt_vi = rel.inf)
  # This is a measure of "relative influence" (see Friedman 2001). Can't do a 
  # permutation test because we specified keep.data = F when running the model.
 
# Getting variable importance for GAM model
  # Using inverse of p-values for smoothing parameters (like others have done).
  # s.table contains approximate p-values for the null hypotheses that each smooth 
  # term is zero (edf = effective degrees of freedom, with 1 = linear term, 2 =
  # quad, etc; rest of columns are for testing that smooth term is zero)
  
  # Substituting very small value (1e-10) for p-values = 0
  ps <- data.frame(vars = rownames(summary.gam(gam)$s.table),
                   p = summary.gam(gam)$s.table[,"p-value"]) %>%
    mutate(vars = str_remove(vars, "s\\("),
           vars = str_remove(vars, "_1\\)"),
           p_adj = ifelse(p < 1e-10, 1e-10, p),
           p_inv = 1 / p_adj,
           vi = round(p_inv / sum(p_inv) * 100, 2))
  # This may not make sense in that variables with p-values of 1e-5 have no 
  # importance if at least one variable has a p-value < 1e-10. Instead, we could
  # make some arbitrary cutoff: p-values < 0.001 all equal?
  ps <- ps %>%
    mutate(p_cutoff = ifelse(p <= 0.001, 0.001, p),
           p_cutoff_inv = 1 / p_cutoff,
           cutoff_vi = round(p_cutoff_inv / sum(p_cutoff_inv) * 100, 2))
  gam_vi <- data.frame(var = ps$vars, gam_vi = ps$vi, gam_cutoff_vi = ps$cutoff_vi)

  # Tried to use a permutation approach, but things didn't work well.
  # library(iml)
  # predictors <- rbind(predictors_p, predictors_a) %>%
  #   mutate(pa = pa_data$pa)
  # stand_obj <- save_means_sds(predictors, cols = climate_vars, verbose = FALSE)
  # dat_z <- prep_predictors(stand_obj, predictors, quad = FALSE) 
  # dat_z <- cbind(pa = predictors$pa, dat_z)	
  # predictor <- Predictor$new(gam, data = dat_z[, -1], y = dat_z$pa)
  # system.time(imp <- FeatureImp$new(predictor, loss = "logLoss", n.repetitions = 20))
  # imp$results
  # plot(imp)
  # # logLoss should be appropriate loss function for binary outcomes
  # # BUT get warnings and errors related to NaNs.
  # # Did get results for MSE loss function but that seems odd for binary outcomes,
  # # plus it took forever to run (~ 15 min for only 20 reps)

# Getting variable importance for LASSO model
  # Using magnitude of regression coefficients (like vip::vi_model() and 
  # caret::varImp() do). No standardization of coefficients necessary because we 
  # standardized variables before running the model.

  # Get coefficients from model with "best" lambda:
  betas <- predict(lasso, s = lasso$lambda.1se, type = "coef")
  betas <- data.frame(coef = betas[,1]) %>%
    mutate(var_full = row.names(betas), .before = coef) %>%
    filter(var_full != "(Intercept)") %>%
    mutate(var = str_split_i(var_full, "_", i = 1), .before = coef) %>%
    mutate(coef_abs = abs(coef),
           lasso_vi = coef_abs / sum(coef_abs))
  # How to deal with quadratics? Could we use the maximum or sum of abs(coef) 
  # for each variable across linear and quadratic terms? There is no easy way to 
  # do calculations for variable groups unless we're writing permutation 
  # algorithms ourselves.
  lasso_vi <- betas %>% 
    group_by(var) %>%
    summarize(coef_max = max(coef_abs),
              coef_sum = sum(coef_abs)) %>%
    mutate(lasso_max_vi = round(coef_max / sum(coef_max) * 100, 2),
           lasso_sum_vi = round(coef_sum / sum(coef_sum) * 100, 2)) %>%
    data.frame() %>%
    dplyr::select(-c(coef_max, coef_sum)) %>%
    arrange(match(var, climate_vars))

# Getting variable importance for RF model  
  # Looks like we should have run RF models with importance = TRUE (default
  # is false). We can add that option to the run_rf() function so we can set 
  # importance = FALSE for CV runs and importance = TRUE for full model runs.
  
  # Create model formula
  model_formula <- paste(climate_vars, collapse = " + ")
  model_formula <- as.formula(paste0("pa ~ ", model_formula))
  
  # Generate equal sample sizes for presence, pseudo-absence data. Note that 
  # if we have more presences than absences in a fold (happens rarely), need 
  # to then sample the presences.
  full_data <- rbind(predictors_p, predictors_a) %>%
    mutate(pa = pa_data$pa)
  prNum <- as.numeric(table(full_data$pa)["1"])
  bgNum <- as.numeric(table(full_data$pa)["0"])
  if (prNum <= bgNum) {
    smpsize <- c("0" = prNum, "1" = prNum)
  } else {
    smpsize <- c("0" = bgNum, "1" = bgNum)
  } 
  
  # Convert pa column to a factor
  full_data$pa <- as.factor(full_data$pa)
  
  rf2 <- randomForest(formula = model_formula,
                      data = full_data,
                      ntree = 1000,
                      sampsize = smpsize,
                      importance = TRUE,
                      replace = TRUE)
  
  importance(rf2) # scales results by the SE of the measure (unless we set scale = F)
  rf2$importance # no scaling
  
  # Calling importance() on the original model object only provides the Gini
  # measure. I think we want the Mean Decrease in Accuracy instead (both 
  # provided when model run with importance = TRUE)
  
  rf_vi <- data.frame(importance(rf2)) %>%
    mutate(var = row.names(.)) %>%
    rename(mda = MeanDecreaseAccuracy) %>%
    mutate(rf_vi = round(mda / sum(mda) * 100, 2)) %>%
    dplyr::select(var, rf_vi) %>%
    arrange(match(var, climate_vars))

# Put everything together  
  vi <- left_join(max_vi, brt_vi, by = "var") %>%
    left_join(gam_vi, by = "var") %>%
    left_join(lasso_vi[, ], by = "var") %>%
    left_join(rf_vi, by = "var") %>%
    dplyr::select(var, brt_vi, gam_cutoff_vi, lasso_max_vi, max_vi, rf_vi) %>%
    rename(brt = brt_vi,
           gam = gam_cutoff_vi,
           lasso = lasso_max_vi,
           maxent = max_vi,
           rf = rf_vi)

  evals_avg <- evals %>%
    group_by(sdm, tune.args) %>%
    summarize(tss = mean(TSS),
              .groups = "keep") %>%
    data.frame()
  # Calculate SDM weights based on mean TSS
  evals_avg$tss_wt <- evals_avg$tss / sum(evals_avg$tss)
  
  # Calculate weighted average for each climate variable importance measure
  wts <- evals_avg$tss_wt
  names(wts) <- colnames(vi)[-1]
  vi <- vi %>%
    mutate(combo = rowSums(across(brt:rf, ~ .x * wts[cur_column()])))
    
  