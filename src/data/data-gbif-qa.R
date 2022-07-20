# GBIF data QA/QC
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-06-21

require(MASS)
require(terra)
require(dplyr)

# Need to check each gbif file to make sure 
#     observations are restricted to CA, MX, US
#     in locations where climate data are available

# Logical indicating whether or not to update data files, removing any 
# observations that are out of bounds
remove_oob <- TRUE

# Logical indicating whether or not to apply envelope filtering; set to TRUE
# to filter by kernel density estimate envelop; EXPERIMENTAL!!!
envelope_filter <- FALSE
# The cutoff for envelope; proportion of observations that defines envelope; 
# i.e. value of 0.98 will remove observations falling outside the 98% density 
# ellipse
envelope_cutoff <- 0.98

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
  # terra::extract will return a two-column data frame in this case, but we 
  # only need the values from column 2 (the value of the bio1 variable)
  data$climate <- terra::extract(x = clim_data, 
                                 y = data[,c('longitude','latitude')])[,2]
  
  # Identify any records that do not have corresponding climate data
  any_oob <- data %>%
    filter(is.na(climate))

  # TODO: Need to update this with envelope considerations in mind  
  gbif_obs$n_filtered[i] <- nrow(data) - nrow(any_oob)

  # Will use this logical to decide if we need to update the file on disk
  removed_any <- FALSE  
  if (nrow(any_oob) > 0 & remove_oob == TRUE) {
    # If appropriate, update files, removing records that are out of bounds
    data <- data %>%
      filter(!(gbifID %in% any_oob$gbifID))
    
    removed_any <- TRUE
  }
  # Drop the climate column (some collisions possible on select function)
  data <- data %>%
    dplyr::select(-climate)
  
  if (envelope_filter) {
    # Calculate density envelope
    # TODO: Need to convert to km before anything else...
    # TODO: n parameter should be something smarter to allow for the same
    # grid cell width in km
    obs_kde <- MASS::kde2d(x = data$longitude,
                           y = data$latitude,
                           n = c(abs(max(data$longitude) - min(data$longitude)),
                                 abs(max(data$latitude) - min(data$latitude))))
    # Transform this to a raster; not quite sure why, but need to do 90 degree 
    # counter-clockwise rotation of the z matrix...
    kde_raster <-raster::raster(x = apply(t(obs_kde$z),2,rev),
                       xmn = min(obs_kde$x), 
                       xmx = max(obs_kde$x),
                       ymn = min(obs_kde$y), 
                       ymx = max(obs_kde$y), 
                       crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    # From https://mhallwor.github.io/_pages/activities_GenerateTerritories
    # Set zeros to NA
    kde_raster[kde_raster == 0] <- NA
    # Get the values as a vector
    kde_values <- raster::getValues(kde_raster)
    # Sort all the not missing values
    sorted_values <- sort(kde_values[!is.na(kde_values)], 
                          decreasing = TRUE)
    # Create cumulative sum of those sorted values
    summed_values <- cumsum(x = sorted_values)
    # Find index of those sorted values for the cutoff
    cutoff_index <- sum(summed_values <= envelope_cutoff * summed_values[length(summed_values)])
    # Set the values of the raster to 0 or 1 based on that cutoff
    kde_envelope <- raster::setValues(kde_raster, 
                                      kde_values >= sorted_values[cutoff_index])

    plot(kde_envelope)
    points(x = data$longitude, y = data$latitude)

    # Pull out values from the kde_envelope raster and add them to data; 
    # 0 = outside envelope, 1 = inside envelope
    data$envelope <- terra::extract(x = kde_envelope, 
                                    y = data[,c('longitude','latitude')])
    # Do data reduction as necessary
    if (any(data$envelope == 0)) {
      data <- data %>%
        dplyr::filter(envelope == 1) %>%
        dplyr::select(-envelope)
      removed_any <- TRUE
    }
  }
  
  if (removed_any) {
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
