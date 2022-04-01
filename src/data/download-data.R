# Download data for all species from GBIF
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-02

# require(dplyr) # currently only needed for testing subset

# Load up the functions from the functions folder
function_files <- list.files(path = "./functions", 
                             pattern = ".R$", 
                             full.names = TRUE)
for(fun_file in function_files) {
  message(paste0("Loading function from ", fun_file))
  source(file = fun_file)
}

gbif_data <- read.csv(file = "data/gbif-reconcile.csv")
replace <- FALSE

# For testing with subset
# gbif_data <- gbif_data %>%
#   dplyr::filter(species %in% c("brevicauda", "multicaudata"))

# Loop over all entries in gbif data and do queries
for (i in 1:nrow(gbif_data)) {
  species_name <- paste0(gbif_data$genus[i], " ", gbif_data$species[i])
  gbif_name <- gbif_data$gbif_name[i]
  download_gbif(species_name = species_name,
                gbif_name = gbif_name,
                replace = replace,
                verbose = TRUE)
  # Adding a 2 second sleep to slow things down.
  Sys.sleep(time = 2)
}
