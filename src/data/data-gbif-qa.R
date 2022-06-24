# GBIF data QA/QC
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-06-21

require(terra)
require(dplyr)

# Need to check each gbif file to make sure 
#     observations are restricted to CA, MX, US
#     in locations where climate data are available

# Logical indicating whether or not to update data files, removing any 
# observations that are out of bounds
remove_oob <- TRUE

# Get list of files with gbif data
gbif_files <- list.files(path = "data/gbif",
                         pattern = "*-gbif.csv",
                         full.names = TRUE)

# Load a tif file with climate data
tif_file <- list.files(path = "data/wc2-1", 
                       pattern = ".tif$", 
                       full.names = TRUE)[1]
clim_data <- rast(tif_file)

# Create a table that will summarize the number of excluded records per spp
gbif_obs <- as.data.frame(matrix(NA, nrow = length(gbif_files), ncol = 3))
colnames(gbif_obs) <- c("species", "n_orig", "n_filtered")

for (i in 1:length(gbif_files)) {
  
  data <- read.csv(file = gbif_files[i])
  gbif_obs$species[i] <- data$accepted_name[1]
  gbif_obs$n_orig[i] <- nrow(data)
  
  # Extract climate data associated with each gbif record location
  data$climate <- extract(clim_data, data[,c('longitude','latitude')])[,2]
  
  # Identify any records that do not have corresponding climate data
  any_oob <- data %>%
    filter(is.na(climate))
  
  gbif_obs$n_filtered[i] <- nrow(data) - nrow(any_oob)
  
  if (nrow(any_oob) > 0 & remove_oob == TRUE) {

    # If appropriate, update files, removing records that are out of bounds
    data <- data %>%
      filter(!(gbifID %in% any_oob$gbifID)) %>%
      select(-climate)
    
    write.csv(x = data,
              file = gbif_files[i],
              row.names = FALSE)
  }
}

# How many records were excluded?
gbif_obs$n_excluded <- gbif_obs$n_orig - gbif_obs$n_filtered
gbif_obs$perc_excluded <- round(gbif_obs$n_excluded / gbif_obs$n_orig * 100, 2)
arrange(gbif_obs, desc(perc_excluded))
  # Papilio brevicauda: 7.23% (46 records) excluded
  # All other species with < 5% of records excluded 
  # 93% of species with < 1% of records excluded

# Create a zip file of all the gbif flies (first removing previous archive)
zipfile <- "data/gbif.zip"
if (file.exists(zipfile)) {
  invisible(file.remove(zipfile))
}
zip(zipfile = "data/gbif.zip",
    files = gbif_files)
