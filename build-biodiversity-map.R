# Create a biodiversity map, with current & forecast distributions of all insect
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-06

require(raster)
require(ggplot2)

# Load up the functions from the functions folder
function_files <- list.files(path = "./functions", 
                             pattern = ".R$", 
                             full.names = TRUE)
for(fun_file in function_files) {
  source(file = fun_file)
}

# Indicates whether or not to write output ggplot maps to file
save_maps <- TRUE

insects_hosts <- read.csv(file = "data/insect-host.csv")

# Identify unique species of insects
insect_species <- unique(insects_hosts$insect)

# Grab the rasters for the current climate
current_rasters <- list()
forecast_rasters <- list()
for (species_name in insect_species) {
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species_name))
  current_file <- paste0("output/distributions/",
                         nice_name, 
                         "-distribution-svm-current.rds")
  forecast_file <- paste0("output/distributions/",
                         nice_name, 
                         "-distribution-svm-GFDL-ESM4_RCP45.rds")
  current_rasters[[nice_name]] <- readRDS(current_file)
  forecast_rasters[[nice_name]] <- readRDS(forecast_file)
}

# Make each plot
current_map <- biodiversity_map(r = current_rasters,
                                predictor = "current")
ggsave(filename = "output/maps/biodiversity-current.pdf",
       plot = current_map)
forecast_map <- biodiversity_map(r = forecast_rasters,
                                predictor = "GFDL-ESM4_RCP45")
ggsave(filename = "output/maps/biodiversity-GFDL-ESM4_RCP45.pdf")
