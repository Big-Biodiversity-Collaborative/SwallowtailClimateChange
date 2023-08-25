# Running new maxent models for butterfly species
# Erin Zylstra
# ezylstra@arizona.edu
# 2023-08-25

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

# Create table to store evaluation metrics for each SDM and fold
insects <- insect_data$species
sdms <- c("BRT", "GAM", "LASSO", "MAXENT", "RF")
folds <- 1:4
evals <- expand.grid(insect = insects, sdm = sdms, fold = folds,
                     stringsAsFactors = FALSE) %>%
  mutate(tune.args = NA,
         AUC = NA, 
         CBI = NA, # Continuous Boyce index
         IMAE = NA, # Inverse mean absolute error
         thr.mss = NA, # Max(sens + spec) threshold value
         OR.mss = NA, # Omission rate, w/ thr = max(sens + spec)
         TSS.mss = NA # True skill statistic, w/ thr = max(sens + spec)
         ) %>%
  relocate(tune.args, .after = sdm) %>%
  arrange(insect, sdm, fold) %>%
  data.frame()

# Loop through species
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
  
    # plot(pred_rs[[1]])
    # points(y ~ x, data = filter(pa_data, block == 1 & pa == 0),
    #        pch = 19, col = "red")
    # points(y ~ x, data = filter(pa_data, block == 2 & pa == 0),
    #        pch = 19, col = "blue")
    # points(y ~ x, data = filter(pa_data, block == 3 & pa == 0),
    #        pch = 19, col = "green")
    # points(y ~ x, data = filter(pa_data, block == 4 & pa == 0),
    #        pch = 19, col = "purple")

  # Maxent --------------------------------------------------------------------#
  # Set tuning parameters for ENMevaluate()
  feature_classes <- c("L", "LQ", "H", "LQH")
  multipliers <- 1:3
  tune.args <- list(fc = feature_classes, rm = multipliers)
  user.grp <- list(occs.grp = pa_data$block[pa_data$pa == 1],
                   bg.grp = pa_data$block[pa_data$pa == 0])
  
  # Specify how AUC should be calculated. Assigning other.settings$validation.bg 
  # to "partition" will calculate AUC with respect to the validation background 
  # only (see Radosavljevic & Anderson 2014).
  os <- list(validation.bg = "partition")  

  # Calculate a few other evaluation metrics, so I can compare model performance
  # with other SDMs. Note that the CBI value ENMevaluate spits out is the same
  # as what's produced with the ecospat package, with fit = predictions from
  # occurrence and bg points in validation area, and nclass = 0.
  em <- function(vars) {
    em <- flexsdm::sdm_eval(p = vars$occs.val.pred, 
                            a = vars$bg.val.pred,
                            thr = "max_sens_spec")
    out <- data.frame(IMAE = em$IMAE,
                      TSS = em$TSS,
                      OR.mss = em$OR,
                      thr = em$thr_value)
    return(out)
  }
  
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
                         user.eval = em,
                         parallel = TRUE,
                         numCores = num_cores)

  # Pick best maxent model (tuning parameters) and save evaluation metrics
  # Eliminating any model that has one or more folds with a negative CBI.
  # Of those remaining, use minimum average or.10p (and maximum average auc.val 
  # to break ties; similar to Kass et al. 2022)
  other_summaries <- mod_max@results.partitions %>%
    group_by(tune.args) %>%
    summarize(cbi.min = min(cbi.val),
              auc.min = min(auc.val),
              n.partitions = length(fold)) %>%
    data.frame()
    # Occasionally, there are no results for one fold (maybe the model didn't
    # run ok?). Should remove this model from consideration. 
  optimal <- mod_max@results %>% 
    left_join(., other_summaries, by = "tune.args") %>%
    filter(n.partitions == 4)
  
  if (max(optimal$cbi.min) <= 0) {
    optimal <- optimal %>%
      filter(cbi.min == max(cbi.min)) %>%
      filter(or.10p.avg == min(or.10p.avg)) %>%
      filter(auc.val.avg == max(auc.val.avg))
  } else {
    optimal <- optimal %>%
      filter(cbi.min > 0) %>%
      filter(or.10p.avg == min(or.10p.avg)) %>%
      filter(auc.val.avg == max(auc.val.avg))
  }
  
  partitions <- mod_max@results.partitions %>%
    filter(tune.args == optimal$tune.args)
  eval_rows <- which(evals$insect == insect & evals$sdm == "MAXENT")
  evals$tune.args[eval_rows] <- as.character(partitions$tune.args)
  evals$AUC[eval_rows] <- partitions$auc.val
  evals$CBI[eval_rows] <- partitions$cbi.val
  evals$IMAE[eval_rows] <- partitions$IMAE  
  evals$thr.mss[eval_rows] <- partitions$thr
  evals$OR.mss[eval_rows] <- partitions$OR.mss
  evals$TSS.mss[eval_rows] <- partitions$TSS

  # Save ENMeval object to file (contains all 12 models w/diff tuning params)
  sdm_file <- paste0("development/output/SDMs/",
                     nice_name, "-sdm-maxent-9var-CV.rds")
  saveRDS(mod_max, sdm_file)
  # Note: I'm not saving the other SDM types for now...
  
  # Prep data for all other SDMs ----------------------------------------------#  
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
  
  # Other SDMs ----------------------------------------------------------------#
  # Loop through each training dataset
  for (j in 1:4) {
    sdmtrain <- pa_data %>%
      filter(block != j)
    sdmtest <- pa_data %>%
      filter(block == j)
    
    # Calculate means, SDs for standardizing covariates
    stand_obj <- save_means_sds(sdmtrain, cols = climate_vars, verbose = TRUE)
    
    # Standardize values in training, testing datasets with quadratics
    sdmtrain_zq <- prep_predictors(stand_obj, sdmtrain, quad = TRUE) 
    sdmtrain_zq <- cbind(pa = sdmtrain[,1], sdmtrain_zq)
    sdmtest_zq <- prep_predictors(stand_obj, sdmtest, quad = TRUE) 
    sdmtest_zq <- cbind(pa = sdmtest[,1], sdmtest_zq)
    
    # Standardize values in training, testing datasets without quadratics
    sdmtrain_z <- prep_predictors(stand_obj, sdmtrain, quad = FALSE) 
    sdmtrain_z <- cbind(pa = sdmtrain[,1], sdmtrain_z)
    sdmtest_z <- prep_predictors(stand_obj, sdmtest, quad = FALSE) 
    sdmtest_z <- cbind(pa = sdmtest[,1], sdmtest_z)
    
    # Creating values to downweight background points (so total [summed] weight of 
    # background points is equal to the total weight of presence points)
    prNum <- as.numeric(table(sdmtrain$pa)["1"])
    bgNum <- as.numeric(table(sdmtrain$pa)["0"])
    wt <- ifelse(sdmtrain$pa == 1, 1, prNum / bgNum)
    
    # Run BRT and save evaluation metrics
      # Set tree complexity (number of nodes in tree; only 1 if few records)
      tc <- ifelse(prNum < 40, 1, 5)
      # Learning rate (weights applied to individual trees; start at 0.01)
      poss_lr_values <- c(0.001, 0.01, 0.05, 0.10)
      lr_index <- 2
      lr <- poss_lr_values[lr_index] 
      # Number of initial trees (50 is default)
      n_trees <- 100
      # Number of trees to add at each step (50 is default)
      step_size <- 100
      # Maximum number of trees to fit before stopping (default is 10000)
      max_trees <- 10000
      # Number of folds for cross-validation (to find optimal number of trees)
      n_folds <- 5
      
      # Note: using try() function so if model fails to fit the loop will continue
      brt_fit <- NULL  
      opt_trees <- 0 
      no_model <-  FALSE
      while (is.null(brt_fit) | opt_trees < 1000 | opt_trees == max_trees)  {
        # Run gbm.step
        # Note: set plot arguments below to FALSE to avoid creating an extraneous 
        # file when running BRT models in parallel
        try(
          brt_fit <- dismo::gbm.step(data = sdmtrain,
                                     gbm.x = 6:ncol(sdmtrain), # Columns with predictor data
                                     gbm.y = 1,                # Column with pa data
                                     family = "bernoulli",
                                     tree.complexity = tc,
                                     learning.rate = lr,
                                     n.trees = n_trees,
                                     step.size = step_size,
                                     max.trees = max_trees,
                                     n.folds = n_folds,
                                     verbose = FALSE, 
                                     silent = TRUE,
                                     plot.main = FALSE,  
                                     plot.folds = FALSE)
        )
        
        # Extract the optimal number of trees
        opt_trees <- ifelse(is.null(brt_fit), 0, brt_fit$gbm.call$best.trees)
        
        # If the algorithm is unable to find an optimum number with the fastest 
        # learning rate (0.10) within 10,000 trees, then exit. 
        if (lr_index == 4 & opt_trees == max_trees) {
          no_model <- TRUE
          message("Unable to find optimal number of trees with learning rate = 0.1 and max trees = 10,000.")
          break
        }
        
        # If the optimal number of trees is < 1000 with a learning rate of 0.001, 
        # save this model and exit.
        if (lr_index == 1 & opt_trees < 1000) {
          message("Optimal number of trees < 1000 with learning rate = 0.001. ",
                  "Saving model with < 1000 trees.")
          break      
        }
        
        # Adjust the learning rate if the optimal number of trees is < 1000 or 
        # optimal number couldn't be identified (ie, equal to max_trees) 
        lr_index <- ifelse(opt_trees < 1000, 
                           max(lr_index - 1, 1), 
                           ifelse(opt_trees == max_trees, 
                                  min(lr_index + 1, 4),
                                  lr_index))
        lr <- poss_lr_values[lr_index]
      }
      
      if (!is.null(brt_fit)) {
        ntree <- brt_fit$gbm.call$best.trees
        tree.complex <- brt_fit$gbm.call$tree.complexity
        learning <- brt_fit$gbm.call$learning.rate
        tune.args <- paste0("tree.", ntree, "_node.", tree.complex, "_lr.", learning)
      
        brt_eval <- dismo::evaluate(p = sdmtest[sdmtest$pa == 1, climate_vars],
                                    a = sdmtest[sdmtest$pa == 0, climate_vars],
                                    model = brt_fit,
                                    type = "response")
        brt_em <- flexsdm::sdm_eval(p = brt_eval@presence,
                                    a = brt_eval@absence,
                                    thr = "max_sens_spec") 
        brt_boyce <- ecospat::ecospat.boyce(fit = c(brt_eval@presence, 
                                                    brt_eval@absence),
                                            obs = brt_eval@presence,
                                            nclass = 0,
                                            PEplot = FALSE)      
        # Save evaluation metrics
        row_index <- which(evals$insect == insect & evals$sdm == "BRT" & evals$fold == j)
        evals$tune.args[row_index] <- tune.args
        evals$AUC[row_index] <- brt_em$AUC
        evals$CBI[row_index] <- brt_boyce$cor
        evals$IMAE[row_index] <- brt_em$IMAE  
        evals$thr.mss[row_index] <- brt_em$thr_value
        evals$OR.mss[row_index] <- brt_em$OR
        evals$TSS.mss[row_index] <- brt_em$TSS   
      }
        
    # Run GAM and save evaluation metrics
      # Create model formula
      smooth <- "s"
      model_formula <- paste0(smooth, "(", climate_vars, "_1)")
      model_formula <- paste(model_formula, collapse = " + ")
      model_formula <- as.formula(paste0("pa ~ ", model_formula))  
      # Model below adds a double penalty to remove variables that don't help
      gam_fit <- mgcv::gam(model_formula,
                           data = sdmtrain_z,
                           family = binomial, 
                           method = 'REML', 
                           select = TRUE)
      gam_eval <- dismo::evaluate(p = sdmtest_z[sdmtest_z$pa == 1,],
                                  a = sdmtest_z[sdmtest_z$pa == 0,],
                                  model = gam_fit,
                                  type = "response")
      gam_em <- flexsdm::sdm_eval(p = gam_eval@presence,
                                  a = gam_eval@absence,
                                  thr = "max_sens_spec")  
      gam_boyce <- ecospat::ecospat.boyce(fit = c(gam_eval@presence, 
                                                  gam_eval@absence),
                                          obs = gam_eval@presence,
                                          nclass = 0,
                                          PEplot = FALSE)   
      # Save evaluation metrics
      row_index <- which(evals$insect == insect & evals$sdm == "GAM" & evals$fold == j)
      evals$tune.args[row_index] <- NA
      evals$AUC[row_index] <- gam_em$AUC
      evals$CBI[row_index] <- gam_boyce$cor
      evals$IMAE[row_index] <- gam_em$IMAE  
      evals$thr.mss[row_index] <- gam_em$thr_value
      evals$OR.mss[row_index] <- gam_em$OR
      evals$TSS.mss[row_index] <- gam_em$TSS
    
    # Run LASSO and save evaluation metrics
      lasso_fit <- glmnet::cv.glmnet(x = as.matrix(sdmtrain_zq[,2:ncol(sdmtrain_zq)]),
                                     y = sdmtrain_zq$pa,
                                     family = "binomial",
                                     alpha = 1,
                                     weights = wt,
                                     standardize = FALSE)
      lasso_eval <- evaluate_lasso(p = as.matrix(sdmtest_zq[sdmtest_zq$pa == 1, -1]), 
                                   a = as.matrix(sdmtest_zq[sdmtest_zq$pa == 0, -1]), 
                                   model = lasso_fit,
                                   s = lasso_fit$lambda.1se,
                                   type = "response")
      lasso_em <- flexsdm::sdm_eval(p = lasso_eval@presence,
                                    a = lasso_eval@absence,
                                    thr = "max_sens_spec")  
      lasso_boyce <- ecospat::ecospat.boyce(fit = c(lasso_eval@presence, 
                                                    lasso_eval@absence),
                                            obs = lasso_eval@presence,
                                            nclass = 0,
                                            PEplot = FALSE)
      
      # Save evaluation metrics
      row_index <- which(evals$insect == insect & evals$sdm == "LASSO" & evals$fold == j)
      evals$tune.args[row_index] <- "lambda.1se"
      evals$AUC[row_index] <- lasso_em$AUC
      evals$CBI[row_index] <- lasso_boyce$cor
      evals$IMAE[row_index] <- lasso_em$IMAE  
      evals$thr.mss[row_index] <- lasso_em$thr_value
      evals$OR.mss[row_index] <- lasso_em$OR
      evals$TSS.mss[row_index] <- lasso_em$TSS
    
    # Run RF and save evaluation metrics
      # Create model formula
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
      sdmtrain_f <- sdmtrain
      sdmtrain_f$pa <- as.factor(sdmtrain_f$pa)

      # Run model
      rf_fit <- randomForest::randomForest(formula = model_formula,
                                           data = sdmtrain_f,
                                           ntree = 1000,
                                           sampsize = smpsize,
                                           replace = TRUE)
      # For RF, which is a classification model, need to use predict with type 
      # = "prob" before dismo::evaluate()
      rf_preds <- predict(rf_fit, sdmtest[,climate_vars], type = "prob")[, 2]
      rf_eval <- dismo::evaluate(p = rf_preds[which(sdmtest$pa == 1)],
                                 a = rf_preds[which(sdmtest$pa == 0)])
      rf_em <- flexsdm::sdm_eval(p = rf_eval@presence,
                                 a = rf_eval@absence,
                                 thr = "max_sens_spec")  
      rf_boyce <- ecospat::ecospat.boyce(fit = c(rf_eval@presence,
                                                 rf_eval@absence),
                                         obs = rf_eval@presence,
                                         nclass = 0,
                                         PEplot = FALSE)

      # Save evaluation metrics
      row_index <- which(evals$insect == insect & evals$sdm == "RF" & evals$fold == j)
      evals$tune.args[row_index] <- paste0("mtry = ",rf_fit$mtry)
      evals$AUC[row_index] <- rf_em$AUC
      evals$CBI[row_index] <- rf_boyce$cor
      evals$IMAE[row_index] <- rf_em$IMAE  
      evals$thr.mss[row_index] <- rf_em$thr_value
      evals$OR.mss[row_index] <- rf_em$OR
      evals$TSS.mss[row_index] <- rf_em$TSS
  }
}  

# Save table with evaluation metrics
# write.csv(evals, 
#           file = "development/output/evals-CV-insect-SDMs.csv", 
#           row.names = FALSE)

# If run previously, load evals data.frame
evals <- read.csv("development/output/evals-CV-insect-SDMs.csv", header = TRUE)

# Run new BRT models with mean number of trees across folds:
  brt <- filter(evals, sdm == "BRT") %>%
    mutate(tree = as.numeric(str_sub(tune.args, 6, 9)),
           lr = as.numeric(str_sub(tune.args, 21, nchar(tune.args))),
           node = as.numeric(str_sub(tune.args, 16, 16)))
  brt_sp <- brt %>%
    group_by(insect) %>%
    summarize(lr.min = min(lr),
              lr.max = max(lr)) %>%
    data.frame()
  brt <- brt %>%
    left_join(., brt_sp, by = "insect") %>%
    filter(lr == lr.min)
  brt_sp <- brt %>%
    group_by(insect) %>%
    summarize(tree = mean(tree),
              lr = mean(lr),
              node = mean(node)) %>%
    data.frame()
  
  for (i in 1:nrow(insect_data)) {
    nice_name <- nice_names[i]
    insect <- insect_data$species[i]
    
    # Load in presence/absence data
    pa_file <- paste0("data/gbif/presence-absence/",
                      nice_name,
                      "-pa.csv")
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
    buffered_mcp <- vect(shapefile_name)
    
    # Crop and mask predictor rasters
    pred_mask <- terra::crop(predictors, buffered_mcp)
    pred_mask <- terra::mask(pred_mask, buffered_mcp) 
  
    # Create spatial blocks with ENM eval (creates 4 blocks via lat/long
    # with relatively even number of occurrences in each partition)
    block <- get.block(occs, bg)
    # Attach block numbers to pa data
    pa_data$block <- c(block$occs.grp, block$bg.grp)
    
    # Prep predictor data
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
    
    # Set BRT parameters
    tc <- brt_sp$node[i]
    lr <- brt_sp$lr[i]
    n_trees <- brt_sp$tree[i]
    
    for (j in 1:4) {
      sdmtrain <- pa_data %>%
        filter(block != j)
      sdmtest <- pa_data %>%
        filter(block == j)
      try(
        brt_fit <- dismo::gbm.fixed(data = sdmtrain,
                                   gbm.x = 6:ncol(sdmtrain),
                                   gbm.y = 1, 
                                   family = "bernoulli",
                                   tree.complexity = tc,
                                   learning.rate = lr,
                                   n.trees = n_trees,
                                   bag.fraction = 0.75,
                                   verbose = TRUE)
      )
      if (!is.null(brt_fit)) {
        tune.args <- paste0("tree.", n_trees, "_node.", tc, "_lr.", lr)
        brt_eval <- dismo::evaluate(p = sdmtest[sdmtest$pa == 1, climate_vars],
                                    a = sdmtest[sdmtest$pa == 0, climate_vars],
                                    model = brt_fit,
                                    type = "response")
        brt_em <- flexsdm::sdm_eval(p = brt_eval@presence,
                                    a = brt_eval@absence,
                                    thr = "max_sens_spec") 
        brt_boyce <- ecospat::ecospat.boyce(fit = c(brt_eval@presence, 
                                                    brt_eval@absence),
                                            obs = brt_eval@presence,
                                            nclass = 0,
                                            PEplot = FALSE)      
        # Save evaluation metrics
        row_index <- which(evals$insect == insect & evals$sdm == "BRT" & evals$fold == j)
        evals$tune.args[row_index] <- tune.args
        evals$AUC[row_index] <- brt_em$AUC
        evals$CBI[row_index] <- brt_boyce$cor
        evals$IMAE[row_index] <- brt_em$IMAE  
        evals$thr.mss[row_index] <- brt_em$thr_value
        evals$OR.mss[row_index] <- brt_em$OR
        evals$TSS.mss[row_index] <- brt_em$TSS   
      }
    }
  }

# Save new version of evals dataframe with BRT models that use the same tuning
# parameters
# write.csv(evals, 
#           file = "development/output/evals-CV-insect-SDMs-meanBRT.csv", 
#           row.names = FALSE)

# -----------------------------------------------------------------------------#
# Load MAXENT (ENMeval) objects for each species and compile results
  for (i in 1:nrow(insect_data)) {
    nice_name <- nice_names[i]
    insect <- insect_data$species[i]
    max <- readRDS(paste0("development/output/SDMs/",
                          nice_name,
                          "-sdm-maxent-9var-CV.rds"))
    results <- max@results %>%
      mutate(insect = insect) %>%
      select(c(insect, tune.args, auc.train, cbi.train, 
               auc.val.avg, auc.val.sd, cbi.val.avg, cbi.val.sd, IMAE.avg,
               or.10p.avg, thr.avg, OR.mss.avg, TSS.avg, ncoef)) %>%
      rename(thr = thr.avg) %>%
      mutate(across(auc.train:TSS.avg, function(x) round(x, 3)))
    partitions <- max@results.partitions
    rownames(partitions) <- NULL
    partitions <- partitions %>%
      mutate(insect = insect) %>%
      select(c(insect, tune.args, fold, auc.val, cbi.val, IMAE, or.10p, thr,
               OR.mss, TSS)) %>%
      mutate(across(auc.val:thr, function(x) round(x, 3)))
    if (i == 1) {
      maxent_results <- results
      maxent_partitions <- partitions
    } else {
      maxent_results <- rbind(maxent_results, results)
      maxent_partitions <- rbind(maxent_partitions, partitions)
    }
  }
  
  # Write maxent result tables to file:
  # write.csv(maxent_results, 
  #           "development/output/evals-maxent-insect.csv",
  #           row.names = FALSE)
  # write.csv(maxent_partitions, 
  #           "development/output/evals-byfold-maxent-insect.csv",
  #           row.names = FALSE)  
  
# -----------------------------------------------------------------------------#  
# Summarize evaluation metrics across SDMs and species
  
# If run previously, load evals data.frame with new BRT models
evals <- read.csv("development/output/evals-CV-insect-SDMs-meanBRT.csv", 
                  header = TRUE)

# Identify problematic evaluation metrics:
evals <- evals %>%
  mutate(prob.AUC = ifelse(AUC < 0.5, 1, 0),
         prob.CBI = ifelse(CBI < 0, 1, 0),
         prob = ifelse(prob.AUC + prob.CBI > 0, 1, 0))

# Summary of CBI problems:
  cbi <- evals %>%
    filter(prob.CBI == 1) %>%
    count(., insect, sdm)
  cbi.issues <- data.frame(insect = insect_data$species, 
                           BRT = 0, GAM = 0, LASSO = 0, MAXENT = 0, RF = 0)
  for (i in 1:nrow(cbi)) {
    cbi.issues[cbi.issues$insect == cbi$insect[i], cbi$sdm[i]] <- 1
  }
  cbi.issues$n.SDMs <- 5 - rowSums(cbi.issues[, -1])
  cbi.issues
  apply(cbi.issues[, 2:6], 2, sum)
  # Remember: this is just with 9 climate variables

# Correlations between evaluation metrics
round(cor(evals[, c("AUC", "CBI", "IMAE", "OR.mss", "TSS.mss")]), 2)
plot(evals[, c("AUC", "CBI", "IMAE", "OR.mss", "TSS.mss")])
# AUC and maxTSS highly correlated (0.95)
# AUC and CBI correlated (0.58)
# CBI and maxTSS weakly correlated (0.44)
# IMAE isn't correlated with others (-0.17 < r 0.22)
# OR isn't correlated with others (-0.33 < r < 0.04)

# Could remove TSS without losing much information:
round(cor(evals[, c("AUC", "CBI", "IMAE", "OR.mss")]), 2)
plot(evals[, c("AUC", "CBI", "IMAE", "OR.mss")])

evals_avg <- evals %>%
  group_by(insect, sdm) %>%
  summarize(auc.avg = mean(AUC),
            auc.min = min(AUC),
            cbi.avg = mean(CBI),
            cbi.min = min(CBI),
            imae.avg = mean(IMAE),
            imae.min = min(IMAE),
            thr.avg = mean(thr.mss),
            or.avg = mean(OR.mss),
            or.max = max(OR.mss),
            .groups = "keep") %>%
  mutate(across(auc.avg:or.max, function(x) round(x, 3))) %>%
  data.frame()

# Look at distributions of evaluation metrics (across species and models)
par(mfrow = c(2, 1))
hist(evals_avg$auc.avg, breaks = 20)
abline(v = 0.5, col = "blue", lty = 2)
hist(evals_avg$auc.min, breaks = 20)
abline(v = 0.5, col = "blue", lty = 2)

hist(evals_avg$cbi.avg, breaks = 20)
abline(v = 0, col = "blue", lty = 2)
hist(evals_avg$cbi.min, breaks = 20)
abline(v = 0, col = "blue", lty = 2)

hist(evals_avg$imae.avg, breaks = 20)
hist(evals_avg$imae.min, breaks = 20)

hist(evals_avg$or.avg, breaks = 20)
hist(evals_avg$or.max, breaks = 20)


