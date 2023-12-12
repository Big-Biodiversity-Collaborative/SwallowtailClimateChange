# Create delta maps for insect species
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2023-12-12

require(terra)
require(dplyr)
require(stringr)
require(ggplot2)
require(tidyterra)

# Load up the functions from the functions folder
source(file = "load_functions.R")

# Logical indicating whether to replace an overlap map if one already exists
replace <- TRUE

# Logicals indicating whether to create and save delta maps for any areas 
# predicted suitable for the insect and/or areas predicted suitable for the 
# insect and one or more hosts
totalinsect <- FALSE
insecthost <- TRUE

# Type of image file (extension)
file_ext <- "png"

# Load information about data availability for each species
species_info <- read.csv("data/gbif-pa-summary.csv")
insects <- species_info %>%
  filter(grepl("Papilio", species)) %>%
  filter(pa_csv == "yes")

# Load list of climate models
climate_models <- read.csv(file = "data/climate-models.csv")

# Logical indicating whether to create overlap maps for all insects or just a 
# subset of insects
all_insects <- FALSE

# Extract insect names
if (all_insects) {
  insects <- insects$species
} else {
  # If not all insects, identify which insects to include
  insects <- c("Papilio rumiko")
}

# Vector with types of distributions/delta maps
distributions <- c("totalinsect", "insecthost")

for (insect in insects) {
  
  nice_name <- insect %>%
    str_replace(pattern = " ", replacement = "_") %>%
    tolower()
  
  for (i in 1:length(distributions)) {
    
    if (get(distributions[i]) == FALSE) next
    
    for (j in 2:nrow(climate_models)) {
      
      clim_model <- climate_models$name[j]
      clim_short <- str_remove(clim_model, "ensemble_")
      
      map_file <- paste0("output/maps/", nice_name, "-delta-",
                         distributions[i], "-", clim_short, ".", file_ext)
      
      if (file.exists(map_file) & replace == FALSE) next
      
      cat(paste0("Saving delta map for ", insect, ", distribution = ",
                 distributions[i], ", ", clim_short, ".\n"))

      delta_file <- paste0("output/deltas/", nice_name, "-delta-", 
                           distributions[i], "-", clim_short, ".rds")  
      deltas <- readRDS(delta_file)
      
      # Values in raster are 0:3, with 
      # 0 = Area unsuitable* in current and forecast climate
      # 1 = Area suitable in current climate only (= loss)
      # 2 = Area suitable in forecast climate only (= gain)
      # 3 = Area suitable in current and forecast climate (= stable)
      
      map_object <- delta_map(species_name = insect, 
                              delta_raster = deltas,
                              clim_model = clim_model,
                              include_legend = TRUE,
                              horizontal_legend = TRUE,
                              prediction_area = TRUE,
                              boundaries = TRUE,
                              obs_points = FALSE,
                              full_title = TRUE)

      ggsave(filename = map_file,
             plot = map_object, 
             width = 6, 
             height = 6,
             units = "in")
      
    }
  } 
}

