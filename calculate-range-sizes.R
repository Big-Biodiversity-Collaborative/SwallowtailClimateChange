# Compare contemporary vs. forecast range areas for insect species
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-18

require(raster)

# TODO: Needs significant revision with pending deprecation of create_overlaps

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
  range_areas <- data.frame(species = character(0))
                            # current_area = numeric(0),
                            # current_overlap = numeric(0),
                            # forecast_area = numeric(0),
                            # forecast_overlap = numeric(0))
}

insects_hosts <- read.csv(file = "data/insect-host.csv")

# identify unique species of insects
insect_species <- unique(insects_hosts$insect)

predictors <- c("current", "GFDL-ESM4_RCP45")

# iterate over each species of insects, doing pairs of calculations:
# 1. Calculate the area of the insect
# 2. Calculate the area of the insect's range that overlaps with >= 1 host
# Do each of the above for current and forecast predictions
for (species_name in insect_species) {
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species_name))

  # Iterate over all predictors, naming output columns accordingly
  for (predictor in predictors) {
    overlap_file <- paste0("output/ranges/",
                           nice_name, "-overlap-",
                           model, 
                           "-",
                           predictor,
                           ".rds")
    
    if (file.exists(overlap_file)) {
      overlap <- readRDS(file = overlap_file)
      
      # Now do calculations for overlaps, using raster::area, we get 
      # (approximate) km2 of each cell value
      cell_areas <- tapply(X = raster::area(overlap),
                           INDEX = overlap[],
                           FUN = sum)
      
      # Pull out area of those cells for insect only (== 1)
      insect_only <- cell_areas["1"]
      # In case where there are NO pixels of insect only, need to set this to 0
      if (length(insect_only) == 0) {
        insect_only <- 0
      }
      
      # Pull out area of those cells for plant AND insect (== 3)
      insect_plant <- cell_areas["3"]
      # If there are no pixels with both, set to 0
      if (length(insect_plant) == 0) {
        insect_plant <- 0
      }
      
      total_insect <- insect_only + insect_plant
      
      # Now to add the values of total_insect and insect_plant to data frame,
      # naming the columns 
      # <predictor>_area (total_insect), 
      # <predictor>_overlap_area (insect_plant), and 
      # <predictor>_alone_area
      
      
      
      
    } else {
      # Overlap file not found
      # TODO: Add a warning or something...
    }
  }
  
  # start with the 
  
  
    
  # Start by using distribution rasters to get the range areas of the insect 
  # itself (ignoring whether or not it overlaps with host(s))
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

  # Now use overlap function to do calculations and use value from area_overlap
  current_overlap <- create_overlaps(species_name = species_name,
                                     predictor = "current",
                                     model = model,
                                     crop_to_insect = FALSE)
  current_overlap_area <- current_overlap$overlap_area
  
  forecast_overlap <- create_overlaps(species_name = species_name,
                                      predictor = "GFDL-ESM4_RCP45",
                                      model = model,
                                      crop_to_insect = TRUE)  
  forecast_overlap_area <- forecast_overlap$overlap_area
    
  # Area calculations complete, update output data frame
  if (species_name %in% range_areas$species) {
    range_areas$current_area[range_areas$species == species_name] <- current_area
    range_areas$current_overlap[range_areas$species == species_name] <- current_overlap_area
    range_areas$forecast_area[range_areas$species == species_name] <- forecast_area
    range_areas$forecast_overlap[range_areas$species == species_name] <- forecast_overlap_area
  } else {
    # No row for this species yet, so add it
    range_areas <- rbind(range_areas,
                   list(species = species_name,
                        current_area = current_area,
                        current_overlap = current_overlap_area,
                        forecast_area = forecast_area,
                        forecast_overlap = forecast_overlap_area))
  }
} # end iterating over all insect species

write.csv(x = range_areas,
          file = output_file,
          row.names = FALSE)
