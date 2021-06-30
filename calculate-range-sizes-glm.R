# Compare contemporary vs. forecast range areas for insect species for GLM
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-30

require(raster)
require(parallel)
require(dplyr)

model <- "glm"
output_file <- paste0("output/ranges/range-areas-", model, ".csv") 
predictors <- c("current", "GFDL-ESM4_RCP45")

# Load up the functions from the functions folder
function_files <- list.files(path = "./functions", 
                             pattern = ".R$", 
                             full.names = TRUE)
for(fun_file in function_files) {
  source(file = fun_file)
}

# Function to perform calculations for each species of insect; allows 
# vectorization and list output that we'll turn into a data frame later
range_calcs <- function(species_name, model, predictors) {

  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species_name))

  return_list <- NULL
  # Iterate over all predictors, naming output accordingly
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
      
      # Removing names for easier downstream processing
      names(insect_only) <- NULL
      names(insect_plant) <- NULL
      
      total_insect <- insect_only + insect_plant

      # Now to add the values of total_insect and insect_plant to list, naming
      # elements:
      # <predictor>_area (total_insect), 
      # <predictor>_overlap_area (insect_plant), and 
      # <predictor>_alone_area
      if (is.null(return_list)) {
        return_list <- list()
        return_list[["insect"]] <- species_name
      }
      
      name_area <- paste0(predictor, "_area")
      name_overlap <- paste0(predictor, "_overlap_area")
      name_alone <- paste0(predictor, "_alone_area")

      return_list[[name_area]] <- total_insect
      return_list[[name_overlap]] <- insect_plant
      return_list[[name_alone]] <- insect_only
      
    } else {
      # Overlap file not found
      warning(paste0("No overlap file '", overlap_file, 
                     "' found for ", species_name))
    }
  }
  return(return_list)
}

insects_hosts <- read.csv(file = "data/insect-host.csv")

# identify unique species of insects
insect_species <- unique(insects_hosts$insect)
insect_species_list <- as.list(insect_species)

# For parallel processing, do two fewer cores or eight (whichever is lower)
num_cores <- parallel::detectCores() - 2
if (num_cores > 8) {
  num_cores <- 8
}

ranges_list <- parallel::mclapply(X = insect_species_list,
                                  FUN = range_calcs,
                                  mc.cores = num_cores,
                                  model = model,
                                  predictors = predictors)

# Bind all the elements into a data frame
ranges_df <- dplyr::bind_rows(ranges_list)

write.csv(x = ranges_df, 
          file = output_file,
          row.names = FALSE)
