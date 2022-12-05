# Calculate percent of contemporary range predicted to be suitable in forecasts
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-10-19

# Placholder for script that will calculate the percent of current (predicted)
# range that is predicted to be occupied (present) for under each of the 
# forecast climate models. Differs from calculate-range-sizes and 
# calculate-range-changes in that it does not just calculate total area for 
# each time period / climate model and compare area values, but rather uses
# contemporary predicted range as reference group and calculates how much of 
# that area is predicted to be occupied under different forecast conditions.

# Should read in rasters from output/overlaps/ and base and do comparisons for 
# (1) total insect area (values of 1 and 3 in raster) and (2) area in which 
# insect overlaps with at least on host (value of 3 in raster) (see 
# documentation in functions/overlap_raster.R for explanation of raster values)

sdm_method <- "glm"


# Load list of insects

# Load climate model data frame
climate_models <- read.csv(file = "data/climate-models.csv")

# Make data frame with one column for insect names, one column for shortened 
# forecast climate model names, one column which will hold the value name (such 
# as "total", "with_plant"), and one column for the values called "proportion"

# TODO: how to generate this data frame before hand, where the only thing 
# missing is the proportion? Could be a tad more efficient than row binding, 
# but is it worth it?

# Iterate over insects

# Make nice names that are used in file names

# Check to see if predicted values for the current climate is there; if so 
# proceed with comparisons

# Load current climate model overlap file for this insect

# Convert raster values to:
# 0, 2 -> 0  None present or only plant present
# 1 -> 1     Insect present, plant absent
# 3 -> 2     Insect and plant present

# Calculate total area of (1 + 3) and of 3
insect_total_current <- NA
insect_plant_current <- NA

# Iterate over all forecast climate models

# Load forecast climate model overlap file
# Convert raster values to:
# 0, 2 -> 0  None present or only plant present
# 1 -> 4     Insect present, plant absent
# 3 -> 8     Insect and plant present

# Add rasters together and calculate
# (1) total area of current range of insect (including areas with and without 
#     plants) occupied in forecast by insect (including areas with and without 
#     plants). These are values: (1 + 4) and (2 + 8), where parentheses each 
#     have a current raster value (the first number) and a forecast raster 
#     value (the second number). So this value will be the total area of the 
#     cells with values of 5 and 10
# (2) total area of current range of insect + plant occupied in forecast by 
#     insect + plant. These are values (2 + 8) = 10 in the summed raster.
insect_total_forecast <- NA
insect_plant_forecast <- NA

# Do proportion calculations 
proportion_total <- insect_total_forecast / insect_total_current
proportion_plant <- insect_plant_forecast / insect_plant_current

# Store these proportions in results data frame for this insect

# No predictions for insect under current climate model, move on to next insect

# Write results file to output/areas
# TODO: Do we want one file, or do we want one file per insect species?