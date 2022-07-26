# GBIF data QA/QC
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2022-06-21

require(MASS)
require(terra)
require(raster)
require(dplyr)
require(dismo)   # thinning for kernel density estimate

# Need to check each gbif file to make sure 
#     observations are restricted to CA, MX, US
#     in locations where climate data are available
#     inside the 95% contour of observations

# TODO: Rename gbif folders to data/gbif/original (where downloads go) and 
#       data/gbif/filtered (where results from this script go) *AND* uncomment
#       the file and zip writing at the end of this file *AND* update 
#       .gitignore as appropriate
# TODO: Order of operations? climate filtering or envelope filtering first?
# TODO: Resolution of envelope is based on 0.5 degrees, but climate data are in 
#       1km resolution. Should we make higher resolution envelope?
# TODO: Thinning, for defining the kernel density estimate, samples one 
#       observation from every cell in climate raster. Should this be 
#       increased? Note that if it increased too high (i.e. to 1000 points), 
#       areas of extremely (artificially?) high density will break algorithm

# Logical indicating whether or not to remove any observations that are out of 
# bounds as defined by those in locations with no (terrestrial) climate data
remove_oob <- TRUE

# Logical indicating whether or not to apply envelope filtering; set to TRUE
# to filter by kernel density estimate envelop.
envelope_filter <- TRUE
# The cutoff for envelope; proportion of observations that defines envelope; 
# i.e. value of 0.95 will remove observations falling outside the 95% density 
# contour
envelope_cutoff <- 0.95
# Problems with kernel density envelope when few observations are present; skip
# the envelope filtering if number of observations is below this
envelope_min <- 10

# Get list of files with gbif data
gbif_files <- list.files(path = "data/gbif",
                         pattern = "*-gbif.csv",
                         full.names = TRUE)

# Load a tif file with climate data
tif_file <- list.files(path = "data/wc2-1", 
                       pattern = ".tif$", 
                       full.names = TRUE)[1]
clim_data <- terra::rast(tif_file)

# Create a table that will summarize the number of excluded records per spp
gbif_obs <- as.data.frame(matrix(NA, nrow = length(gbif_files), ncol = 4))
colnames(gbif_obs) <- c("species", "n_orig", "n_oob", "n_outlier")

for (i in 1:length(gbif_files)) {
  data <- read.csv(file = gbif_files[i])
  gbif_obs$species[i] <- data$accepted_name[1]
  gbif_obs$n_orig[i] <- nrow(data)
  # oob is out of bounds as defined by coordinates with climate data
  gbif_obs$n_oob[i] <- NA
  # outlier as defined by outside the envelope
  gbif_obs$n_outlier[i] <- NA
  
  # Extract climate data associated with each gbif record location
  # terra::extract will return a two-column data frame in this case, but we 
  # only need the values from column 2 (the value of the bio1 variable)
  data$climate <- terra::extract(x = clim_data, 
                                 y = data[,c('longitude','latitude')])[,2]
  
  # Identify any records that do not have corresponding climate data
  any_oob <- data %>%
    filter(is.na(climate))

  if (nrow(any_oob) > 0 & remove_oob == TRUE) {
    # If appropriate, update files, removing records that are out of bounds
    data <- data %>%
      filter(!(gbifID %in% any_oob$gbifID))
    # Update count of observations without climate data
    gbif_obs$n_oob[i] <- nrow(any_oob)
  }
  
  # Drop the climate column (some collisions possible on select function)
  data <- data %>%
    dplyr::select(-climate)
  
  if (envelope_filter & nrow(data) >= envelope_min) {
    # Start by thinning out observations; some very high density areas can 
    # wreck the kernel density estimates, e.g. Fraxinus pennsylvanica
    # We'll use another data frame to thin coordinates and build the envelope
    thinned <- data %>%
      dplyr::select(longitude, latitude) %>%
      dismo::gridSample(r = clim_data, n = 1) # Takes a couple seconds for >10k points
    
    # Calculate density envelope; using a 0.5 degree resolution
    n_points <- c(abs(max(thinned$longitude) - min(thinned$longitude)) * 2,
                  abs(max(thinned$latitude) - min(thinned$latitude)) * 2)
    obs_kde <- MASS::kde2d(x = thinned$longitude,
                           y = thinned$latitude, 
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
      mutate(envelope = if_else(is.na(envelope), 0, envelope))

    # Reality check
    # plot(kde_envelope, col = c("white", "grey50"))
    # points(x = data$longitude, y = data$latitude, pch = 16,
    #        cex = 0.75, col = "red")
    # points(x = data$longitude[data$envelope == 0],
    #        y = data$latitude[data$envelope == 0],
    #        cex = 1, col = "blue", pch = 16)
    
    # Do data reduction as necessary
    n_outside <- 0
    if (any(data$envelope == 0, na.rm = TRUE)) {
      n_outside <- sum(data$envelope == 0, na.rm = TRUE)
      data <- data %>%
        dplyr::filter(envelope == 1) %>%
        dplyr::select(-envelope)
      # Update count of observations outside of envelope (i.e. "outliers")
      gbif_obs$n_outlier[i] <- n_outside
    }
  }

  # Write to file if any of the observations were dropped; some odd logic to 
  # accommodate all NA values without throwing error in if statement
  n_removed <- sum(gbif_obs$n_oob[i], gbif_obs$n_outlier[i], na.rm = TRUE)
  if (n_removed > 0) {
    #   write.csv(x = data,
    #             file = gbif_files[i],
    #             row.names = FALSE)
  }
}

# How many records were excluded?
gbif_obs$n_excluded <- rowSums(gbif_obs[, c("n_oob", "n_outlier")], 
                               na.rm = TRUE)
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
