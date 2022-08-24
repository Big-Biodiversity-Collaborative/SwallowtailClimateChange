# Extract and store variable importance metrics from MaxEnt models
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-08-23

require(dplyr)
require(stringr)
require(dismo)

# Identify files with MaxEnt output
maxent_files <- list.files(path = "./output/SDMs/",
                           pattern = "*-maxent-notune.rds",
                           full.names = TRUE)

# Create empty dataframe to store variable contributions from each model:
var_impt <- data.frame(matrix(NA, nrow = length(maxent_files), ncol = 21))
colnames(var_impt) <- c("species", 
                        "SDM", 
                        paste0("bio", 1:19))

# Loop through files
for (i in 1:length(maxent_files)) {
  
  # Extract species name from filename
  nice_name <- str_split(maxent_files[i], pattern = "/", simplify = TRUE)[1,4]
  nice_name <- sub('-([^-]*)-([^-]*)$', '', nice_name)
  genus <- str_split(nice_name, pattern = "_", simplify = TRUE)[1,1]
  species <- str_split(nice_name, pattern = "_", simplify = TRUE)[1,2]
  species_name <- paste0(str_to_title(genus), " ", species)

  # Read in .rds file
  model_object <- readRDS(maxent_files[i])
  
  # Extract measures of variable importance from maxent model 
  # (using permutation methods: see 
  # https://biodiversityinformatics.amnh.org/open_source/maxent/Maxent_tutorial_2021.pdf)
  all_metrics <- model_object[["model"]]@results
  permute_impt <- all_metrics[grepl("permutation", row.names(all_metrics)),]
  names(permute_impt) <- str_replace(names(permute_impt), 
                                     pattern = ".permutation.importance",
                                     replacement = "") 
  permute_df <- data.frame(variable = names(permute_impt), 
                           permutation_impt = permute_impt, 
                           row.names = NULL)
  permute_df <- left_join(data.frame(variable = paste0("bio", 1:19)),
                          permute_df,
                          by = "variable")
  
  # Put everything in dataframe
  var_impt[i,1] <- species_name
  var_impt[i,2] <- "maxent-notune"
  var_impt[i,3:21] <- permute_df$permutation_impt
  
}

# Save contributions to file
write.csv(contributions, 
          file = "output/variable-importance-maxent-notune.csv",
          row.names = FALSE)
  