# Create maps of filtered observations
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-12-22

require(stringr)

file_ext <- "png"
# Load up the functions (really just needed to load the overlap_map function)
source(file = "load_functions.R")

# Load insect-host file
ih <- read.csv("data/insect-host.csv")

# Logical indicating whether to create overlap rasters for all species or just a 
# subset of insects
all_insects <- FALSE

# Extract species names
if (all_insects) {
  insects <- unique(ih$insect)
} else {
  # If not all insects, identify which insects to include
  insects <- c("Papilio cresphontes", "Papilio glaucus", "Papilio rumiko",
               "Papilio rutulus")
}

# Loop through insect species to create a map of observations
for (i in 1:length(insects)) {

  insect <- insects[i]
  insect_nice_name <- insect %>%
    str_replace(pattern = " ", replacement = "_") %>%
    tolower()
  
  map_object <- observations_map(species_name = insect)

  map_file <- paste0("output/maps/",
                     insect_nice_name, 
                     "-observations",
                     ".", file_ext)
  
  ggsave(filename = map_file,
         plot = map_object, 
         width = 6, 
         height = 6,
         units = "in")
}
