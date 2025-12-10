# Do rarefaction curves for four types of protected areas
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-12-01

# Would like to build rarefaction curves, to compare (estimated) richness 
# between current climate and forecast conditions. Essentially, would like a 
# plot for each forecast climate model and each of the four protection levels, 
# with a curve for current richness and a curve for the forecast richness. 

# There is some question about what the sample unit is for the plot - it could 
# be grid cells, which is probably the "easiest" as our data are currently in 
# that form. I wonder though if square kilometers (area) would be a little 
# easier to understand.

# Read in protected areas shapefile

# Read in insect information

# Read in climate models data frame

# Need to create a SpatRaster of all current suitability rasters
# May be able to avoid species-level iteration, if we can create a vector of 
# filenames to pass to terra::rast()

# Iterate over insect species

# Load in current binary raster

# Extend multi-species raster extent (and vice-versa)

# Add this species' binary raster as layer to SpatRaster

# End iterating over insect species (the first time)

# Use protected areas SpatVector to generate current climate rarefaction 
# data (for curve). And how the hell am I actually going to *do* this?

# Grab a protection level (say, "State")
# Randomly sample 10%-90% of grid cells
# Calculate richness of that percentage
# Do the process again. How many times? 


# Do much of the above, for each of the forecast climate models

# Now plot curves