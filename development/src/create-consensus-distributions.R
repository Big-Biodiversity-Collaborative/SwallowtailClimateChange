# Create consensus distributions for a set of species and climate scenarios
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-11-29

require(stringr)
require(terra)

rm(list = ls())

# Load insect-host file
ih <- read.csv("data/insect-host.csv")

# Load information about samples sizes for each species
species_pa <- read.csv("data/gbif-pa-summary.csv")

# Include all butterflies?
all_insects <- FALSE
# If not, identify which butterflies (and their host plants) to include
insects <- c("Papilio rumiko")

# Extract all species names
if (all_insects) {
  species <- unique(c(ih$insect, ih$host_accepted))
} else {
  plants <- ih$host_accepted[ih$insect %in% insects]
  species <- unique(c(insects, plants))
}

# Load list of climate models
climate_models <- read.csv(file = "data/climate-models.csv")

# List of SDMs to include
sdms <- c("brt", "gam", "lasso", "maxent-tune", "rf")

all_dist_files <- list.files("output/distributions/", full.names = TRUE)

for (spp in species) {
  
  if (species_pa$pa_csv[species_pa$species == spp] == "no") {
    warning("No distribution files for ", spp, ". Too few occurrence records.")
  } else {
  
    nice_name <- tolower(spp)
    nice_name <- str_replace(nice_name, " ", "_")
    
    for (i in 1:nrow(climate_models)) {
      
      model_name <- climate_models$name[i]
      
      dist_files <- str_subset(all_dist_files, nice_name)
      dist_files <- str_subset(dist_files, model_name)
      dist_files <- str_subset(dist_files, paste(sdms, collapse = "|"))
      
      if (length(dist_files) < length(sdms)) {
        warning("Missing at least one SDM distribution file for ", spp, " and ",
                model_name, " climate model. Did not create consensus distribution.")
      } else {
        dist_list <- list()
        
        for (j in 1:length(dists)) {
          dist_list[[j]] <- readRDS(dist_files[j])
          dist_list[[j]] <- rast(dist_list[[j]])
        }
        consensus <- rast(dist_list)
        
        consensus <- sum(consensus)
        consensus <- consensus > 3
        
        consensus_file <- paste0("output/distributions-consensus/",
                                 nice_name, 
                                 "-distrib-consensus-",
                                 model_name,
                                 ".rds")
        saveRDS(object = consensus, 
                file = consensus_file)
      }
    }
  }
}

