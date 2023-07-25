# Investigate background points influence on model evaluation
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-07-19

# Rationale: Are background points that co-occur in cells of observations 
# (presence points) causing models to do poorly? By definintion, if there are 
# two points (one presence, one pseudo-absence) in a single raster cell, one of 
# those points will be guaranteed to be predicted wrong in our model.

# Specific questions
# 1. How big of an issue is this? What proportion of presence observations share 
#    a cell with a background point?
# 2. How do they affect model performance metrics, like AUC? That is, what is 
#    AUC when we include all background points vs AUC when we exclude any 
#    background points that co-occur with a presence point in a raster cell?

# Addressing questions
# 1. Identify species and methods to test this on
# 2. Find way of determining co-occurrence. Could use bit math. Rasterize 
#    observations with presence = 1 & absence = 2, so sum = 3. Proportion is 
#    then C1 / (C1 + C3) (counts of cells with 1 (C1) and 3 (C3)). Addresses 
#    **Question 1**
# 3. Find way of removing any background points from the data frame that 
#    co-occur with presence points. Then run model and compare metrics. Scripts 
#    that run models will write list to file and have an `evaluation` element.
#    Addresses **Question 2**

library(dplyr)
library(terra)
source(file = "load_functions.R")
set.seed(20230721)

################################################################################
# Iteration 1: Starting with our local rumiko
rumiko_obs <- read.csv(file = "data/gbif/presence-absence/papilio_rumiko-pa.csv")

# Question 1: Proportion overlap
# Counting cells
# Get one climate raster
bio1 <- terra::rast(x = "data/wc2-1/bio1.tif")
# Just want lat & long for rumiko, along with pa
rumiko_xyz <- rumiko_obs[, c("x", "y", "pa")]

# Gets the cell id for each point in rumiko_xyz
rumiko_cells <- terra::cellFromXY(bio1, as.matrix(rumiko_xyz[, c("x", "y")]))

# Calculate how many cells have two points
two_points <- length(rumiko_cells) - length(unique(rumiko_cells))
# 115 for rumiko

# Calculate proportion of absences that share a cell
prop_shared <- two_points/sum(rumiko_obs$pa)
# Only 5% for rumiko

# Question 2: Affect on AUC
# Want to create two datasets: one with overlap and one with no overlap. Both 
# should have the same number of observations, even the one with overlaps.

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

# Identify cells that have both presence and pseudo-absence points; add a 
# column in the observation data frame indicating thus
rumiko_obs$cell_id <- rumiko_cells
rumiko_obs <- rumiko_obs %>%
  arrange(desc(pa))  # order so presence points have lower indexes
rumiko_obs$shared <- duplicated(rumiko_obs$cell_id)
# sum(rumiko_obs$shared)

# Create the "no overlap" dataset by removing all background points that share 
# a cell with a presence point
rumiko_no_overlap <- rumiko_obs %>%
  filter(!shared)

# Create the "overlap" dataset by removing random background points, but 
# **not** any of those background points that are in a cell with presence point
# Start by identifying those background points that are in a cell without a 
# presence point
solo_background <- which(rumiko_obs$pa == 0 & !rumiko_obs$shared)
to_skip <- sample(x = solo_background, 
                  size = sum(rumiko_obs$shared))
rumiko_overlap <- rumiko_obs[-to_skip, ]

# Run SDM on both datasets
# Drop columns not used by SDM
rumiko_no_overlap <- rumiko_no_overlap %>%
  dplyr::select(pa, fold, all_of(paste0("bio", 1:19)))
rumiko_overlap <- rumiko_overlap %>%
  dplyr::select(pa, fold, all_of(paste0("bio", 1:19)))

no_overlap_lasso <- run_lasso(full_data = rumiko_no_overlap,
                              quad = TRUE,
                              verbose = TRUE)

overlap_lasso <- run_lasso(full_data = rumiko_overlap,
                           quad = TRUE,
                           verbose = TRUE)

# Extract evaluation metric and compare
no_overlap_lasso$evaluation@auc - overlap_lasso$evaluation@auc
# [1] 0.00535957
# Not really any different

################################################################################
# Iteration 2: reduce hard-coding to single string
species_name <- "Papilio troilus"
nice_name <- tolower(x = gsub(x = species_name,
                              pattern = " ",
                              replacement = "_"))

obs <- read.csv(file = paste0("data/gbif/presence-absence/",
                              nice_name,
                              "-pa.csv"))

# Question 1: Proportion overlap
# Counting cells
# Get one climate raster
bio1 <- terra::rast(x = "data/wc2-1/bio1.tif")
# Just want lat & long for rumiko, along with pa
obs_xyz <- obs[, c("x", "y", "pa")]

# Gets the cell id for each point in obs_xyz
obs_cell_id <- terra::cellFromXY(bio1, as.matrix(obs_xyz[, c("x", "y")]))

# Calculate how many cells have two points
two_points <- length(obs_cell_id) - length(unique(obs_cell_id))
# 115 for rumiko
# 553 for troilus

# Calculate proportion of absences that share a cell
prop_shared <- two_points/sum(obs$pa)
# Only 5% for rumiko
# 6% for troilus

# Question 2: Affect on AUC
# Want to create two datasets: one with overlap and one with no overlap. Both 
# should have the same number of observations, even the one with overlaps.

# Need to add in climate data for SDMs
predictors <- terra::rast(list.files(path = "data/wc2-1",
                                     pattern = ".tif$",
                                     full.names = TRUE))

# Extract bioclim data for presence/absence data; can take a moment
predictors <- terra::extract(x = predictors, 
                             y = obs[, c("x", "y")], 
                             xy = FALSE) %>%
  dplyr::select(-ID)

# Join bioclim data with original full_data (which has pa and fold info),
# dropping x, y columns at the same time
obs <- obs %>%
  cbind(., predictors) %>%
  dplyr::select(c("pa", "fold", all_of(paste0("bio", 1:19))))

# Identify cells that have both presence and pseudo-absence points; add a 
# column in the observation data frame indicating thus
obs$cell_id <- obs_cell_id
obs <- obs %>%
  arrange(desc(pa))  # order so presence points have lower indexes
obs$shared <- duplicated(obs$cell_id)
# sum(obs$shared)

# Create the "no overlap" dataset by removing all background points that share 
# a cell with a presence point
obs_no_overlap <- obs %>%
  filter(!shared)

# Create the "overlap" dataset by removing random background points, but 
# **not** any of those background points that are in a cell with presence point
# Start by identifying those background points that are in a cell without a 
# presence point
solo_background <- which(obs$pa == 0 & !obs$shared)
to_skip <- sample(x = solo_background, 
                  size = sum(obs$shared))
obs_overlap <- obs[-to_skip, ]

# Run SDM on both datasets
# Drop columns not used by SDM
obs_no_overlap <- obs_no_overlap %>%
  dplyr::select(pa, fold, all_of(paste0("bio", 1:19)))
obs_overlap <- obs_overlap %>%
  dplyr::select(pa, fold, all_of(paste0("bio", 1:19)))

no_overlap_lasso <- run_lasso(full_data = obs_no_overlap,
                              quad = TRUE,
                              verbose = TRUE)

overlap_lasso <- run_lasso(full_data = obs_overlap,
                           quad = TRUE,
                           verbose = TRUE)

# Extract evaluation metric and compare
no_overlap_lasso$evaluation@auc - overlap_lasso$evaluation@auc
# rumiko ranges from 0-0.005 (not much difference)
# troilus ~ 0.017

################################################################################
# Iteration 3: Do a bunch of papilio
papilio_species <- c("Papilio appalachiensis", "Papilio brevicauda",
                     "Papilio canadensis", "Papilio cresphontes", 
                     "Papilio eurymedon", "Papilio glaucus",
                     "Papilio indra", "Papilio multicaudata", 
                     "Papilio palamedes", "Papilio polyxenes",
                     "Papilio rumiko", "Papilio rutulus",
                     "Papilio troilus", "Papilio zelicaon")
eval_results <- data.frame(species_name = papilio_species,
                           num_present = NA_integer_,
                           num_overlap = NA_integer_,
                           prop_overlap = NA_real_,
                           auc_no_overlap = NA_real_,
                           auc_overlap = NA_real_)

# Need to add in climate data for SDMs
clim_data <- terra::rast(list.files(path = "data/wc2-1",
                                    pattern = ".tif$",
                                    full.names = TRUE))

for (i in 1:nrow(eval_results)) {
  species_name <- eval_results$species_name[i]
  nice_name <- tolower(x = gsub(x = species_name,
                                pattern = " ",
                                replacement = "_"))
  
  obs <- read.csv(file = paste0("data/gbif/presence-absence/",
                                nice_name,
                                "-pa.csv"))

  eval_results$num_present[i] <- sum(obs$pa)
    
  # Question 1: Proportion overlap
  # Counting cells
  # Just want lat & long for rumiko, along with pa
  obs_xyz <- obs[, c("x", "y", "pa")]
  
  # Gets the cell id for each point in obs_xyz
  obs_cell_id <- terra::cellFromXY(clim_data[[1]], 
                                   as.matrix(obs_xyz[, c("x", "y")]))
  
  # Calculate how many cells have two points
  eval_results$num_overlap[i] <- length(obs_cell_id) - length(unique(obs_cell_id))
  

  # Calculate proportion of absences that share a cell
  eval_results$prop_overlap[i] <- eval_results$num_overlap[i]/sum(obs$pa)
  
  # Question 2: Affect on AUC
  # Want to create two datasets: one with overlap and one with no overlap. Both 
  # should have the same number of observations, even the one with overlaps.
  
  # Extract bioclim data for presence/absence data; can take a moment
  predictors <- terra::extract(x = clim_data, 
                               y = obs[, c("x", "y")], 
                               xy = FALSE) %>%
    dplyr::select(-ID)
  
  # Join bioclim data with original full_data (which has pa and fold info),
  # dropping x, y columns at the same time
  obs <- obs %>%
    cbind(., predictors) %>%
    dplyr::select(c("pa", "fold", all_of(paste0("bio", 1:19))))
  
  # Identify cells that have both presence and pseudo-absence points; add a 
  # column in the observation data frame indicating thus
  obs$cell_id <- obs_cell_id
  obs <- obs %>%
    arrange(desc(pa))  # order so presence points have lower indexes
  obs$shared <- duplicated(obs$cell_id)

  # Create the "no overlap" dataset by removing all background points that share 
  # a cell with a presence point
  obs_no_overlap <- obs %>%
    filter(!shared)
  
  # Create the "overlap" dataset by removing random background points, but 
  # **not** any of those background points that are in a cell with presence point
  # Start by identifying those background points that are in a cell without a 
  # presence point
  solo_background <- which(obs$pa == 0 & !obs$shared)
  to_skip <- sample(x = solo_background, 
                    size = sum(obs$shared))
  obs_overlap <- obs[-to_skip, ]
  
  # Run SDM on both datasets
  # Drop columns not used by SDM
  obs_no_overlap <- obs_no_overlap %>%
    dplyr::select(pa, fold, all_of(paste0("bio", 1:19)))
  obs_overlap <- obs_overlap %>%
    dplyr::select(pa, fold, all_of(paste0("bio", 1:19)))
  
  message("Running no overlap lasso on ", species_name)
  no_overlap_lasso <- run_lasso(full_data = obs_no_overlap,
                                quad = TRUE,
                                verbose = FALSE)
  
  message("Running overlap lasso on ", species_name)
  overlap_lasso <- run_lasso(full_data = obs_overlap,
                             quad = TRUE,
                             verbose = FALSE)
  eval_results$auc_no_overlap[i] <- no_overlap_lasso$evaluation@auc
  eval_results$auc_overlap[i] <- overlap_lasso$evaluation@auc
}

# Calculate difference in AUC between no overlap and overlap datasets
eval_results$auc_delta <- eval_results$auc_no_overlap - eval_results$auc_overlap
