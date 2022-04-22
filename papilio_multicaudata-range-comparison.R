# Example of comparing % of insect's range within host(s) range
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-01

require(raster)
require(dplyr)

# Load up the functions from the functions folder
source(file = "load_functions.R")

# Eventually be replaced...somehow...
obs_data <- data.frame(type = c("insect", rep(x = "host", times = 3)),
                       name = c("Papilio multicaudata", "Vauquelinia californica",
                                "Fraxinus anomala", "Prunus emarginata"))

obs_list <- list()
# Pull in all distributions and store in the list
for (i in 1:nrow(obs_data)) {
  nice_name <- tolower(gsub(pattern = " ", 
                            replacement = "_",
                            x = obs_data$name[i]))
  
  # Make a list element, which is a list with useful information
  obs_list[[i]] <- list(type = obs_data$type[i],
                        name = obs_data$name[i],
                        nice_name = nice_name)
  
  # Read in the presence / absence raster for each, storing in this list element
  current_pa_file <- paste0("output/distributions/", 
                       nice_name, "-distribution-svm-current.rds")
  obs_list[[i]][["current_pa"]] <- readRDS(file = current_pa_file)
  
  # While we are here do the same for forecast presence / absence
  forecast_pa_file <- paste0("output/distributions/", 
                             nice_name, "-distribution-svm-GFDL-ESM4_RCP45.rds")
  obs_list[[i]][["forecast_pa"]] <- readRDS(file = forecast_pa_file)
  
  # Use the compute friendly name for the name of this list element
  names(obs_list)[i] <- nice_name
}

# Returns a named vector; each (named) element indicates host/insect
# This makes it easier to extract portions of lists
types <- unlist(lapply(obs_list, "[[", "type"))
insect <- which(types == "insect")
hosts <- which(types == "host")

# Extract insect presence / absence raster
insect_pa <- obs_list[[insect]]$current_pa

# Combine all plants into a single raster, with binary output
host_pa_rasters <- lapply(obs_list[hosts], "[[", "current_pa")
host_pa_stack <- stack_rasters(r = host_pa_rasters, out = "binary")

# Change all values of 1 in plant stack to 2 to make it easier to do 
# calculations on host vs insect
host_pa_stack[host_pa_stack >= 1] <- 2

# Combine plant and insect raster, cells will have following values:
#   + 1: insect only
#   + 2: plant only
#   + 3: both
all_pa <- stack_rasters(r = list(insect_pa, host_pa_stack),
                        out = "total")

# Calculate frequencies of all possible pixel values
pixel_freqs <- data.frame(raster::freq(all_pa))

# Count how many pixels are insect only (== 1)
insect_only <- pixel_freqs$count[pixel_freqs$value == 1]

# Count how many pixels are plant AND insect (== 3)
insect_plant <- pixel_freqs$count[pixel_freqs$value == 3]

# Calculate proportion of insect overlapping with plant relative to insect total 
# insect only / (insect + plant AND insect)
# This is the proportion of the insect's range that overlaps with host range
perc_overlap <- insect_plant / (insect_only + insect_plant)

################################################################################
# Now do the same thing with forecast presence / absence
# Extract insect presence / absence raster
insect_forecast_pa <- obs_list[[insect]]$forecast_pa

# Combine all plants into a single raster, with binary output
host_forecast_pa_rasters <- lapply(obs_list[hosts], "[[", "forecast_pa")
host_forecast_pa_stack <- stack_rasters(r = host_forecast_pa_rasters, 
                                        out = "binary")

# Change all values of 1 in plant stack to 2 to make it easier to do 
# calculations on host vs insect
host_forecast_pa_stack[host_forecast_pa_stack >= 1] <- 2

# Combine plant and insect raster, cells will have following values:
#   + 1: insect only
#   + 2: plant only
#   + 3: both
all_forecast_pa <- stack_rasters(r = list(insect_forecast_pa, host_forecast_pa_stack),
                        out = "total")

# Calculate frequencies of all possible pixel values
forecast_pixel_freqs <- data.frame(raster::freq(all_forecast_pa))

# Count how many pixels are insect only (== 1)
forecast_insect_only <- forecast_pixel_freqs$count[forecast_pixel_freqs$value == 1]

# Count how many pixels are plant AND insect (== 3)
forecast_insect_plant <- forecast_pixel_freqs$count[forecast_pixel_freqs$value == 3]

# Calculate proportion of insect overlapping with plant relative to insect total 
# insect only / (insect + plant AND insect)
# This is the proportion of the insect's range that overlaps with host range
forecast_perc_overlap <- forecast_insect_plant / (forecast_insect_only + forecast_insect_plant)

# Calculate predicted change
delta <- forecast_perc_overlap - perc_overlap
