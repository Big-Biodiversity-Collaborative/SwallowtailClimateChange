# Testing forecast prediction for P. multicaudata
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-05-31

require(raster) # also loads sp
require(dplyr)  # load *after* raster for easier use of select
require(tidyr)

# Check for forecast data, download if it does not exist
if (!file.exists("data/cmip5/2_5m/gf45bi701.tif")) {
  gfdl_data <- raster::getData(name = "CMIP5",
                               var = "bio",
                               res = 2.5,
                               rcp = 45,
                               model = "GD",
                               year = 70,
                               path = "data/")
}

# Incomplete; currently forecast is in p_multi_test.R. Some serious thought and 
# likely rewiring required. Going to need to potentially save background 
# points to prevent having to do that sampling repeatedly