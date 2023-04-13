# Explore different metrics for assessing SDM "performance"
# Erin Zylstra
# ezylstra@arizona.edu
# 2023-04-13

require(stringr)
require(dplyr)
require(dismo)
require(flexsdm)
  # # To install flexsdm, need version ≥ 1.5-12 of terra 
  # # Then:
  # install.packages("remotes")
  # # For Windows and Mac OS operating systems
  # remotes::install_github("sjevelazco/flexsdm")
  # # For Linux operating system
  # remotes::install_github("sjevelazco/flexsdm@HEAD")
  # # See: https://github.com/sjevelazco/flexsdm
require(modEvA)

# Identify which insects to include (For now, just those species for which I 
# have SDM model objects created in the last month. Note: future predictions 
# for each of these species are suspect in some way or another)
insects <- c("Papilio appalachiensis",
             "Papilio brevicauda",
             "Papilio glaucus",
             "Papilio palamedes")
nice_names <- insects %>%
  str_replace(pattern = " ", replacement = "_") %>%
  tolower()

# List of SDMs to include
sdms <- c("brt", "gam", "lasso", "maxent-tune", "rf")

# Use flexsdm::sdm_eval() to calculate most metrics. For this need:
  # p = predicted suitability for presences
  # a = predicted suitability for absences
  # bg = predicted suitability for background points (for Boyce)
  # thr = threshold criterion (prob use: "equal_sens_spec" or "max_sens_spec")

# Create table with evaluation metrics for each species, model
metrics <- data.frame(expand.grid(SDM = sdms, species = insects)) %>%
  mutate(n_presences = NA,
         n_absences = NA, 
         min_suit_a = NA,
         max_suit_a = NA,     
         threshold = "max_sens_spec",
         thr_value = NA,
         sens = NA,
         spec = NA,
         Sorenson = NA,
         Jaccard = NA,
         omissionrate = NA,
         TSS = NA,
         AUC = NA,
         IMAE = NA,
         Boyce = NA,
         Boyce_modEvA = NA,
)

sdm_folder <- "output/SDMs/"

for (i in 1:length(insects)) {
  for (sdm in sdms) {
    sdm_filename <- paste0(sdm_folder, nice_names[i], "-", sdm, ".rds")
    model <- readRDS(sdm_filename)
    
    real <- c(rep(1, model$evaluation@np), rep(0, model$evaluation@na))
    np <- sum(real == 1)
    na <- sum(real == 0)
    pred <- c(model$evaluation@presence, model$evaluation@absence)
    
    flex_metrics <- flexsdm::sdm_eval(p = model$evaluation@presence,
                                      a = model$evaluation@absence,
                                      thr = "max_sens_spec")
    ind <- which(metrics$species == insects[i] & metrics$SDM == sdm)
    
    metrics[ind, "n_presences"] <- flex_metrics[, c("n_presences")]
    metrics[ind, "n_absences"] <- flex_metrics[, c("n_absences")]
    metrics[ind, "min_suit_a"] <- round(min(pred[(np + 1):(na + np)]), 2)
    metrics[ind, "max_suit_a"] <- round(max(pred[(np + 1):(na + np)]), 2)
    metrics[ind, "thr_value"] <- round(flex_metrics[, c("thr_value")], 2)
    metrics[ind, "sens"] <- round(flex_metrics[, c("TPR")], 2)
    metrics[ind, "spec"] <- round(flex_metrics[, c("TNR")], 2)
    metrics[ind, "Sorenson"] <- round(flex_metrics[, c("SORENSEN")], 2)
    metrics[ind, "Jaccard"] <- round(flex_metrics[, c("JACCARD")], 2)   
    metrics[ind, "omissionrate"] <- round(flex_metrics[, c("OR")], 2)
    metrics[ind, "TSS"] <- round(flex_metrics[, c("TSS")], 2)
    metrics[ind, "AUC"] <- round(flex_metrics[, c("AUC")], 2)
    metrics[ind, "IMAE"] <- round(flex_metrics[, c("IMAE")], 2)
    metrics[ind, "Boyce_flexsdm"] <- round(flex_metrics[, c("BOYCE")], 2)
    metrics[ind, "Boyce_modEvA"] <- round(Boyce(obs = real, 
                                                pred = pred, 
                                                plot = FALSE)$Boyce, 2)
  }
}  

metrics %>%
  dplyr::select(-threshold)

# Summarize by species
metrics %>%
  dplyr::select(-threshold) %>%
  group_by(species, n_presences) %>%
  summarize(across(min_suit_a:Boyce_modEvA, median), .groups = "keep") %>%
  data.frame()

# Summarize by SDM type
metrics %>%
  dplyr::select(-threshold) %>%
  group_by(SDM) %>%
  summarize(across(min_suit_a:Boyce_modEvA, median), .groups = "keep") %>%
  data.frame()

#------------------------------------------------------------------------------#
# Looking at modEvA::Boyce() function

sdm_filename <- paste0(sdm_folder, "papilio_appalachiensis", "-brt.rds")
model <- readRDS(sdm_filename)
real <- c(rep(1, model$evaluation@np), rep(0, model$evaluation@na))
np <- sum(real == 1)
na <- sum(real == 0)
pred <- c(model$evaluation@presence, model$evaluation@absence)
real <- c(rep(1, model$evaluation@np), rep(0, model$evaluation@na))
pred <- c(model$evaluation@presence, model$evaluation@absence)
Boyce(obs = real, pred = pred)

# For P. palamedes, RF: results from this similar to that output from flexsdm

# For P. appalachiensis, BRT: value totally different (-0.06) but with a warning
# that "Some bins (plotted in red) have less than 30 values, so their result may 
# not be meaningful (see 'bin.N' column in console output)."
# That's because almost all of the background/pseudo-abs 2000 suitability values 
# are super low (<0.03) and the max is 0.22. So, many of the bins have only 1 or 
# 2 values, so you'd expect almost no presences to be in there regardless of the
# bin range. 

#------------------------------------------------------------------------------#

# Calculating metrics on my own from model object to see whether they are the 
# same or very similar to that output by the flexsdm package

thr <- model$thresh
ind <- which(model$evaluation@t == thr)
# Confusion matrix
conf <- model$evaluation@confusion[ind,]

# Threshold-dependent metrics (calc at max(sens + spec))
tp <- conf["tp"]
fp <- conf["fp"]
fn <- conf["fn"]
tn <- conf["tn"]
sens <- tp / (tp + fn) # dismo::evaluate TPR (true positive rate)
spec <- tn / (tn + fp) # dismo::evaluate TNR (true negative rate)
TSS <- sens + spec - 1
Jaccard <- tp / (fn + tp + fp)    # from Leroy et al. 2018
Soren <- 2*tp / (fn + 2 *tp + fp) # form Leroy et al. 2018

# Threshold-independent metrics
real <- c(rep(1, model$evaluation@np), rep(0, model$evaluation@na))
pred <- c(model$evaluation@presence, model$evaluation@absence)
auc <- model$evaluation@auc
IMAE <- 1 - (sum(abs(real - pred))/length(pred))

flexsdm_eval <- sdm_eval(p = model$evaluation@presence,
                         a = model$evaluation@absence,
                         thr = "max_sens_spec")
as.data.frame(flexsdm_eval)

mycalcs <- c(thr, sens, spec, Soren, Jaccard, TSS, auc, IMAE)
# These all look pretty much the same.

#------------------------------------------------------------------------------#
# Use flexsdm package to fit models (and assess performance)?
# Probably worth doing to see how similar results are. And maybe worth talking
# about since this package may do everything we're doing (plus some things like 
# ensemble modeling and eval metrics) with the exception of the LASSO model.
# But could use a GLM with polynomials instead... 

  # In this example, partition the data using the k-fold method
  
  abies2 <- part_random(
    data = abies,
    pr_ab = "pr_ab",
    method = c(method = "kfold", folds = 5)
  )
  
  # Build a generalized additive model using fit_gam
  
  gam_t1 <- fit_gam(
    data = abies2,
    response = "pr_ab",
    predictors = c("aet", "ppt_jja", "pH", "awc", "depth"),
    predictors_f = c("landform"),
    partition = ".part",
    thr = c("max_sens_spec", "equal_sens_spec", "max_sorensen")
  )
  
  gam_t1$performance
  
  # Build a generalized linear model using fit_glm
  
  glm_t1 <- fit_glm(
    data = abies2,
    response = "pr_ab",
    predictors = c("aet", "ppt_jja", "pH", "awc", "depth"),
    predictors_f = c("landform"),
    partition = ".part",
    thr = c("max_sens_spec", "equal_sens_spec", "max_sorensen"),
    poly = 0,
    inter_order = 0
  )
  
  glm_t1$performance
  
  # Build a tuned random forest model using tune_raf
  
  tune_grid <-
    expand.grid(mtry = seq(1, 7, 1))
  
  rf_t1 <-
    tune_raf(
      data = abies2,
      response = "pr_ab",
      predictors = c(
        "aet", "cwd", "tmin", "ppt_djf",
        "ppt_jja", "pH", "awc", "depth"
      ),
      predictors_f = c("landform"),
      partition = ".part",
      grid = tune_grid,
      thr = c("max_sens_spec", "equal_sens_spec", "max_sorensen"),
      metric = "TSS",
    )
  
  rf_t1$performance
  
  # Merge sdm performance tables
  
  merge_df <- sdm_summarize(models = list(gam_t1, glm_t1, rf_t1))
  merge_df
#------------------------------------------------------------------------------#

