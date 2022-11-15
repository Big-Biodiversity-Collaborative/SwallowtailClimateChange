# GBIF data QA/QC
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-06-21

require(MASS)    # kernel density estimation
require(terra)   # extracting observations with climate data
require(raster)  # you know, raster stuff
require(dplyr)   # data wrangling

# TODO: Resolution of envelope is based on 0.5 degrees, but climate data are in 
#       ~ 0.04 degree resolution (2.5 min, ~ 4.5km). Should we make higher
#       resolution envelope?

# Filter observations for each species, so observations:
#     occur between 2000-2022
#     are in locations where climate data are available
#     are thinned to a max of X observations per grid cell (of climate raster)
#     are inside the 95% contour of observations
# Filters are applied in the order above.

# First extract the zip file that has downloaded data
unzip(zipfile = "data/gbif-downloaded.zip")

################################################################################
# FILTER SETTINGS

########################################
# Kernel density estimate envelope settings
# Logical indicating whether or not to apply envelope filtering; set to TRUE
# to filter by kernel density estimate envelope.
envelope_filter <- TRUE
# The cutoff for envelope; proportion of observations that defines envelope; 
# i.e. value of 0.95 will remove observations falling outside the 95% density 
# contour
envelope_cutoff <- 0.95
# Problems with kernel density envelope when few observations are present; skip
# the envelope filtering if number of observations is below this
envelope_min <- 100

########################################
# Date of observation settings
# Logical indicating whether or not to remove observations that are outside the 
# range of desired years
remove_old <- TRUE
year_range <- 2000:2022

########################################
# Thinning settings
# Logical indicating whether or not to remove excess observations of species 
# in the same grid cell (i.e., so we're left with a maximum of X observations 
# per grid cell)
thin <- TRUE
max_obs_per_cell <- 1

########################################
# Climate data filter settings
# Logical indicating whether or not to remove any observations that are out of 
# bounds as defined by those in locations with no (terrestrial) climate data
remove_oob <- TRUE

################################################################################
# FILE PROCESSING

# Get list of files with gbif data
gbif_files <- list.files(path = "data/gbif/downloaded",
                         pattern = "*-gbif.csv",
                         full.names = TRUE)

# Will need to create filename for filtered data. Going to cheat and just 
# replace "downloaded" with "filtered" in the filename
filtered_files <- gsub(pattern = "downloaded",
                       replacement = "filtered",
                       x = gbif_files)

# Load a tif file with climate data
tif_file <- list.files(path = "data/wc2-1", 
                       pattern = ".tif$", 
                       full.names = TRUE)[1]
clim_data <- terra::rast(tif_file)

# Create a table that will summarize the number of excluded records per species
gbif_obs <- as.data.frame(matrix(NA, nrow = length(gbif_files), ncol = 7))
colnames(gbif_obs) <- c("species", "n_orig", "n_excluded",
                        "n_old", "n_oob", "n_thin", "n_outlier")

set.seed(20221109)

for (i in 1:length(gbif_files)) {
  # Start with reading in raw data
  data <- read.csv(file = gbif_files[i])

  # Use the accepted name from the first row of data (should be invariant)
  gbif_obs$species[i] <- data$accepted_name[1]
  
  if (i %% 10 == 0) {
    message(paste0("Filtering species ", i, " of ", length(gbif_files),
                   ": ", gbif_obs$species[i], ", ", nrow(data), " obs"))
  }
  
  # Count the number of unfiltered observations
  gbif_obs$n_orig[i] <- nrow(data)

  ########################################
  # Determine if observation is recent enough
  data <- data %>%
    dplyr::mutate(outside_dates = !(year %in% year_range))

  # Record the number of records that are too old
  gbif_obs$n_old[i] <- sum(data$outside_dates)
  
  # Remove records that are too old
  if (remove_old) {
    data <- data %>%
      dplyr::filter(!outside_dates)
  }

  ########################################
  # Determine if observation has climate data
  # Extract climate data associated with each gbif record location
  # terra::extract will return a two-column data frame in this case, but we 
  # only need the values from column 2 (the value of the bio1 variable)
  data$climate <- terra::extract(x = clim_data, 
                                 y = data[,c('longitude','latitude')])[,2]

  data <- data %>%
    dplyr::mutate(missing_climate = is.na(climate))

  # Record the number of records that don't have climate data
  gbif_obs$n_oob[i] <- sum(data$missing_climate)
  
  # Remove records that don't have climate data
  if (remove_oob) {
    data <- data %>%
      dplyr::filter(!missing_climate)
  }
  
  ########################################
  # Thin observations so there's a maximum of X observations per grid cell
    # Could use duplicated() if we were only retaining one observation
    # per grid cell, but I made this a little more general so that we can keep
    # more than one observation per grid cell if desired.  
  data <- data %>%
    # Extract the cell number associated with each observation
    dplyr::mutate(terra::extract(x = clim_data,
                                 y = data[ ,c("longitude", "latitude")],
                                 cells = TRUE,
                                 ID = FALSE)) %>%
    dplyr::arrange(cell) %>%
    # Assign each observation in a grid cell a unique number, from 1 to the 
    # total number of observations in that grid cell
    dplyr::mutate(obs_no = sequence(from = 1, rle(cell)$lengths)) %>%
    # Identify which observations should be removed to retain X per grid cell
    dplyr::mutate(thin = obs_no > max_obs_per_cell)

  # Record the number of records to be thinned (ie, removed)
  gbif_obs$n_thin[i] <- sum(data$thin)
  
  # Remove excess observations within the same cell
  if (thin) {
    data <- data %>%
      dplyr::filter(!thin)
  }
  
  ########################################
  # Determine if observation is in envelope
  if (nrow(data) >= envelope_min) {

    # Calculate density envelope; using a 0.5 degree resolution
    n_points <- c(abs(max(data$longitude) - min(data$longitude)) * 2,
                  abs(max(data$latitude) - min(data$latitude)) * 2)
    obs_kde <- MASS::kde2d(x = data$longitude,
                           y = data$latitude, 
                           n = n_points)
    
    # The projection string for raster conversion
    wgs_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    # Transform this to a raster; not quite sure why, but need to do 90 degree 
    # counter-clockwise rotation of the z matrix...
    kde_raster <-raster::raster(x = apply(X = t(obs_kde$z),
                                          MARGIN = 2,
                                          FUN = rev),
                                xmn = min(obs_kde$x), 
                                xmx = max(obs_kde$x),
                                ymn = min(obs_kde$y), 
                                ymx = max(obs_kde$y), 
                                crs = wgs_crs)
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
    
    # Pull out values from the kde_envelope raster and add them to data; 
    # 0 = outside envelope, 1 = inside envelope
    data$envelope <- raster::extract(x = kde_envelope, 
                                     y = data[,c('longitude','latitude')])
    
    # Sometimes values for envelope are NA if point is on edge of the defined 
    # envelope; count those as outliers, too
    data <- data %>%
      dplyr::mutate(envelope = if_else(is.na(envelope), 0, envelope))
    
    # Reality check
    # plot(kde_envelope, col = c("white", "grey75"))
    # points(x = data$longitude, y = data$latitude, pch = 16,
    #        cex = 0.75, col = "blue")
    # points(x = data$longitude[data$envelope == 0],
    #        y = data$latitude[data$envelope == 0],
    #        cex = 1, col = "red", pch = 16)

    # Create the column indicating if observation is outside envelope
    data <- data %>% 
      dplyr::mutate(outside_envelope = !as.logical(envelope)) %>%
      dplyr::select(-envelope)
  } else { 
    # for subsequent processing, need to add the outside_envelope column to any 
    # dataset that had too few observations for envelope calculations
    data <- data %>%
      dplyr::mutate(outside_envelope = FALSE)
  }
  
  # Record the number of observations that are outside the envelope
  gbif_obs$n_outlier[i] <- sum(data$outside_envelope)
  
  # Remove records that are outside the envelope
  if (envelope_filter) {
    data <- data %>%
      dplyr::filter(!outside_envelope)
  }

  ########################################
  # Drop those columns needed for filtering
  data <- data %>%
    dplyr::select(-c(outside_dates, missing_climate, thin, outside_envelope,
                     climate, bio1, cell, obs_no))

  # Update the excluded column
  gbif_obs$n_excluded[i] <- gbif_obs$n_orig[i] - nrow(data)
  
  # Finally, write these filtered data to file (ONLY if there's a least one
  # location left after filtering)
  if (nrow(data) > 0) {
      write.csv(x = data,
            file = filtered_files[i],
            row.names = FALSE)
  } else {
    message(paste0("** Zero filtered records of ", gbif_obs$species[i], 
                   ". No csv written to file. **"))
  }
}

# How many records were excluded?
gbif_obs <- gbif_obs %>%
  dplyr::mutate(n_remaining = n_orig - n_excluded,
         perc_excluded = round(n_excluded/n_orig * 100, digits = 2))
# arrange(gbif_obs, desc(perc_excluded))

# Create a zip archive of all the filtered files (first removing previous 
# archive)
zipfile <- "data/gbif-filtered.zip"
if (file.exists(zipfile)) {
    invisible(file.remove(zipfile))
}
zip(zipfile = zipfile,
    files = filtered_files)
