# Test create_overlaps
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-15

require(raster)
require(dplyr)
require(ggplot2)

# Load up the functions from the functions folder
function_files <- list.files(path = "./functions", 
                             pattern = ".R$", 
                             full.names = TRUE)
for(fun_file in function_files) {
  source(file = fun_file)
}

species_name <- "Papilio eurymedon"
predictor <- "current"
model <- "glm"
crop_to_insect <- FALSE

overlaps <- create_overlaps(species_name = species_name,
                            predictor = predictor,
                            model = model,
                            crop_to_insect = FALSE)
overlaps$overlap_map
overlaps$prop_overlap
