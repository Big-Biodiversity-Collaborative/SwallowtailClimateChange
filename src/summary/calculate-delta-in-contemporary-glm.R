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
