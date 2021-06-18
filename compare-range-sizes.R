# Compare contemporary vs. forecast range areas for insect species
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-18

require(raster)
require(ggplot2)

model <- "glm"
output_file <- paste0("output/ranges/", model, "-range-areas.csv") 

# Load up the functions from the functions folder
function_files <- list.files(path = "./functions", 
                             pattern = ".R$", 
                             full.names = TRUE)
for(fun_file in function_files) {
  source(file = fun_file)
}

# Need to see if the output file exists yet; create empty data frame if not
if (file.exists(output_file)) {
  range_areas <- read.csv(file = output_file)
} else {
  range_areas <- data.frame(species = character(0),
                            current_area = numeric(0),
                            forecast_area = numeric(0))
}

insects_hosts <- read.csv(file = "data/insect-host.csv")

# identify unique species of insects
insect_species <- unique(insects_hosts$insect)

# iterate over each species of insects
for (species_name in insect_species) {
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species_name))
  current_area <- NA
  current_file <- paste0("output/distributions/", 
                         nice_name, "-distribution-", 
                         model, "-current.rds")
  if (file.exists(current_file)) {
    current_raster <- readRDS(file = current_file)
    current_area <- range_area(r = current_raster)
  }

  forecast_area <- NA
  forecast_file <- paste0("output/distributions/", 
                            nice_name, "-distribution-", 
                          model, "-GFDL-ESM4_RCP45.rds")
  if (file.exists(forecast_file)) {
    forecast_raster <- readRDS(file = forecast_file)
    forecast_area <- range_area(r = forecast_raster)
  }
  
  # Area calculations complete, update output data frame
  if (species_name %in% range_areas$species) {
    range_areas$current_area[range_areas$species == species_name] <- current_area
    range_areas$forecast_area[range_areas$species == species_name] <- forecast_area
  } else {
    # No row for this species yet, so add it
    range_areas <- rbind(range_areas,
                   list(species = species_name,
                        current_area = current_area,
                        forecast_area = forecast_area))
  }
} # end iterating over all insect species

write.csv(x = range_areas,
          file = output_file,
          row.names = FALSE)
