# Create maps from SVM predictions for each insect species and its hosts
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-03

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

# Script will do three things (or call functions to do three things) for each 
# insect species:
# 1. Create a map of the insect and host plants for current distribution 
#    estimates
# 2. Create a map of the insect and host plants for forecast distribution 
#    estimate (currently only based on one forecast climate model)
# 3. For each of above maps, calculate the percentage of the insect's range 
#    that overlaps with the host(s)' range and report that to the output file
#    output/overlaps/svm-overlaps.csv

insects_hosts <- read.csv(file = "data/insect-host.csv")
output_file <- "output/overlaps/svm-overlaps.csv"

# identify unique species of insects
insect_species <- unique(insects_hosts$insect)

# iterate over each species of insects
for (species_name in insect_species) {
  message(paste0("\nCreating overlap maps & calculations for ", species_name))
  
  current_overlap <- create_overlaps(species_name = species_name,
                                     predictor = "current",
                                     model = "svm",
                                     crop_to_insect = TRUE)

  forecast_overlap <- create_overlaps(species_name = species_name,
                                      predictor = "GFDL-ESM4_RCP45",
                                      model = "svm",
                                      crop_to_insect = TRUE)  

  # If requested, save the maps
  if (save_maps) {
    nice_name <- tolower(x = gsub(pattern = " ",
                                  replacement = "_",
                                  x = species_name))
    if (!is.null(current_overlap$overlap_map)) {
      current_filename <- paste0("output/maps/", nice_name, 
                                 "-overlap-svm-current.pdf")
      ggsave(filename = current_filename, plot = current_overlap$overlap_map)
    }
    if (!is.null(forecast_overlap$overlap_map)) {
      forecast_filename <- paste0("output/maps/", nice_name, 
                                 "-overlap-svm-GFDL-ESM4_RCP45.pdf")
      ggsave(filename = forecast_filename, plot = forecast_overlap$overlap_map)
    }
  }
  
  # Convert NULL to NA in overlaps, for easier record keeping
  if (is.null(current_overlap$prop_overlap)) {
    current_overlap$prop_overlap <- NA
  }
  if (is.null(forecast_overlap$prop_overlap)) {
    forecast_overlap$prop_overlap <- NA
  }

  # Only proceed if at least one is not NA
  if (!is.na(current_overlap$prop_overlap) | !is.na(forecast_overlap$prop_overlap)) {
    # Add those to the file (create it if it doesn't exist)
    if (file.exists(output_file)) {
      overlaps <- read.csv(file = output_file)
      # If there is a row for this species already, update it
      if (species_name %in% overlaps$species) {
        overlaps$current[overlaps$species == species_name] <- current_overlap$prop_overlap
        overlaps$forecast[overlaps$species == species_name] <- forecast_overlap$prop_overlap
      } else {
        # No row for this species yet, so add it
        overlaps <- rbind(overlaps,
                          list(species = species_name,
                               current = current_overlap$prop_overlap,
                               forecast = forecast_overlap$prop_overlap))
      }
    } else {
      overlaps <- data.frame(species = species_name,
                             current = current_overlap$prop_overlap,
                             forecast = forecast_overlap$prop_overlap)
    }
    write.csv(x = overlaps,
              file = output_file,
              row.names = FALSE)    
  }
}
