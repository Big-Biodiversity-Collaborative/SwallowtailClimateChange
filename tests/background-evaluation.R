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

# Starting with our local rumiko
rumiko_obs <- read.csv(file = "data/gbif/presence-absence/papilio_rumiko-pa.csv")

# Counting cells
# Get one climate raster
bio1 <- terra::rast(x = "data/wc2-1/bio1.tif")
# Gets the cell id for each point in rumiko_xyz
rumiko_cells <- terra::cellFromXY(bio1, as.matrix(rumiko_xyz[, c("x", "y")]))
# Calculate how many cells have two points
two_points <- length(rumiko_cells) - length(unique(rumiko_cells))
# 115 for rumiko
# Calculate proportion of absences that share a cell
prop_shared <- two_points/sum(rumiko_obs$pa)
# Only 5% for rumiko
