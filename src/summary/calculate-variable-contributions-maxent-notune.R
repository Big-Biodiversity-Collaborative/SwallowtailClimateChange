# Extract and store variable contributions from MaxEnt models
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-08-23

require(stringr)
require(dismo)

# Identify files with MaxEnt output
maxent_files <- list.files(path = "./output/SDMs/",
                           pattern = "*-maxent-notune.rds",
                           full.names = TRUE)

# Create empty dataframe to store variable contributions from each model:
contributions <- data.frame(matrix(NA, nrow = length(maxent_files), ncol = 21))
colnames(contributions) <- c("species", 
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
  
  # Extract values from plot of variable contributions
  model_contrib <- plot(model_object[["model"]])
  model_contrib <- model_contrib[paste0("bio", 1:19)]
  
  # Put everything in dataframe
  contributions[i,1] <- species_name
  contributions[i,2] <- "maxent-notune"
  contributions[i,3:21] <- model_contrib
  
}

# Save contributions to file
write.csv(contributions, 
          file = "output/variable-contributions-maxent-notune.csv",
          row.names = FALSE)
  