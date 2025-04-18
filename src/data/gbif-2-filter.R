# GBIF data QA/QC
# Jeff Oliver & Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2023-10-11

require(ks)      # kernel density estimation
require(terra)   # extracting observations with climate data
require(dplyr)   # data wrangling

# Filter observations for each species, so observations:
#     are not on the basis of barcodes only
#     occur between 2000-2024
#     are in locations where climate data are available
#     are thinned to a max of X observations per grid cell (of climate raster)
#     are inside the 98% contour of observations
# Filters are applied in the order above.

# First extract the zip file that has downloaded data
unzip(zipfile = "data/gbif-downloaded.zip")

# Similar to download process, we can set to replace or not
replace <- FALSE

################################################################################
# FILTER SETTINGS

########################################
# Basis of observation setting
# Logical indicating whether or not to exclude GBIF samples that have a 
# basisOfRecord = "MATERIAL_SAMPLE" (i.e. often soil samples with species 
# identity based on DNA barcodes)
remove_material <- TRUE

########################################
# Date of observation settings
# Logical indicating whether or not to remove observations that are outside the 
# range of desired years
remove_old <- TRUE
year_range <- 2000:2024

########################################
# Climate data filter settings
# Logical indicating whether or not to remove any observations that are out of 
# bounds as defined by those in locations with no (terrestrial) climate data
remove_oob <- TRUE

########################################
# Thinning settings
# Logical indicating whether or not to remove excess observations of species 
# in the same grid cell (i.e., so we're left with a maximum of X observations 
# per grid cell)
thin <- TRUE
max_obs_per_cell <- 1

########################################
# Kernel density estimate envelope settings
# Logical indicating whether or not to apply envelope filtering; set to TRUE
# to filter by kernel density estimate envelope.
envelope_filter <- TRUE
# The cutoff for envelope; proportion of observations that defines envelope; 
# i.e. value of 0.98 will remove observations falling outside the 98% density 
# contour
envelope_cutoff <- 0.98
# Problems with kernel density envelope when few observations are present; skip
# the envelope filtering if number of observations is below this
envelope_min <- 100

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
# Character vector to keep track of species that end up with zero records after 
# filtering (and thus no file is written)
no_filtered_file <- character()

# Load a tif file with climate data
tif_file <- list.files(path = "data/wc2-1", 
                       pattern = ".tif$", 
                       full.names = TRUE)[1]
clim_data <- terra::rast(tif_file)

# Create a table that will summarize the number of excluded records per species
summary_cols <- c("species", "n_orig", "n_excluded",
                  "n_old", "n_oob", "n_thin", "n_outlier", "n_material")
gbif_obs <- as.data.frame(matrix(NA, 
                                 nrow = length(gbif_files), 
                                 ncol = length(summary_cols)))
colnames(gbif_obs) <- summary_cols

set.seed(20221109)

for (i in 1:length(gbif_files)) {
  if (i %% 10 == 0) {
    message(paste0("Filtering species ", i, " of ", length(gbif_files), "."))
  }
  # Only proceed if file doesn't exist or we want to replace existing files
  if (!file.exists(filtered_files[i]) | replace) {
    # Start with reading in raw data
    data <- read.csv(file = gbif_files[i])
    
    # Use the accepted name from the first row of data (should be invariant)
    gbif_obs$species[i] <- data$accepted_name[1]
    
    # Count the number of unfiltered observations
    gbif_obs$n_orig[i] <- nrow(data)
    
    ########################################
    # Determine if observation a material sample (i.e. soil sample)
    # In some cases, the column did not come through, in this case, add it
    if (!("basisOfRecord" %in% colnames(data))) {
      data <- data %>%
        mutate(basisOfRecord = NA_character_)
    }
    data <- data %>%
      dplyr::mutate(material_sample = (basisOfRecord == "MATERIAL_SAMPLE"))
    
    # Record the number of material sample observations
    gbif_obs$n_material[i] <- sum(data$material_sample)
    
    # Remove material sample records
    if (remove_material) {
      data <- data %>%
        dplyr::filter(!material_sample)
    }
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
    # Before we do this, we need to deal with observations that fall on borders
    # of cells in climate raster (because they may get assigned to different 
    # cells when using terra::extract() with climate rasters that are 
    # cropped/uncropped) 
    
    # Function to find cells on borders
    on_border <- function(r, x, y, tolerance = sqrt(.Machine$double.eps)) {
      v <- h <- (x >= xmin(r)) & (x <= xmax(r)) & (y >= ymin(r)) & (y <= ymax(r))
      v[v] <- ((x[v] - xmin(r)) %% res(r)[1]) < tolerance
      h[h] <- ((y[h] - ymin(r)) %% res(r)[2]) < tolerance
      h | v
    }
    
    data$on_border <- on_border(r = clim_data, 
                                x = data$longitude, 
                                y = data$latitude)
    
    # Add a tiny amount to the coordinates associated with observations on borders
    data <- data %>%
      mutate(longitude = ifelse(on_border == TRUE, longitude + 0.00001, longitude),
             latitude = ifelse(on_border == TRUE, latitude + 0.00001, latitude))
    
    # Repeat this process in case a point gets moved to another border (which is
    # highly unlikely)
    if (sum(on_border(clim_data, data$longitude, data$latitude)) > 0) {
      data$on_border <- on_border(r = clim_data, 
                                  x = data$longitude, 
                                  y = data$latitude)
      data <- data %>%
        mutate(longitude = ifelse(on_border == TRUE, longitude + 0.00001, longitude),
               latitude = ifelse(on_border == TRUE, latitude + 0.00001, latitude))
    }
    
    # Now, extract climate data associated with each location.
    clim_extract <- terra::extract(x = clim_data,
                                   y = data[, c("longitude", "latitude")], 
                                   cells = TRUE) %>%
      dplyr::select(-ID)
    data <- cbind(data, clim_extract)
    
    data <- data %>%
      dplyr::mutate(missing_climate = is.na(bio1))
    
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
    # per grid cell, but made this a little more general so that we can keep
    # more than one observation per grid cell if desired.  
    data <- data %>%
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
      
      # Create matrix of observations
      obs_mat <- as.matrix(data[, c("longitude", "latitude")])
      
      # Create density envelope using normal scale smoothing parameters (as these 
      # seemed to be reasonable without "overfitting")
      hns <- ks::Hns(obs_mat)
      f_hns <- ks::kde(obs_mat, H = hns)
      data$kde_pred <- predict(object = f_hns, x = obs_mat)
      env_threshold <- (1 - envelope_cutoff) * 100
      
      # Identify those observations that fall within the envelope
      data$envelope <- ifelse(data$kde_pred >= f_hns$cont[paste0(env_threshold, "%")], 
                              1, 0)
      
      # Reality check
      # plot(latitude ~ longitude, data = data, col = "gray30", pch = 19, cex = 0.5,
      #      las = 1, xlab = "", ylab = "")
      # plot(f_hns, cont = 98, drawlabels = FALSE, col = "blue", add = TRUE)
      # points(latitude ~ longitude, data = data[data$envelope == 1, ], 
      #        col = "gray70", pch = 19, cex = 0.5)  
      
      # Create a column indicating if observation is outside envelope
      data <- data %>% 
        dplyr::mutate(outside_envelope = !as.logical(envelope)) %>%
        dplyr::select(-c(envelope, kde_pred))
    } else { 
      # For subsequent processing, need to add the outside_envelope column to any 
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
    # Drop those columns we used for filtering
    data <- data %>%
      dplyr::select(-c(outside_dates, missing_climate, thin, outside_envelope,
                       on_border, bio1, cell, obs_no, material_sample))
    
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
      # Add this species to the list of files we did not create, so we do not try
      # to include it in the archive
      no_filtered_file <- c(no_filtered_file, filtered_files[i])
    }
  } else {
    message("Filtered records already on disk at ", 
            gbif_files[i], " and rerun set to FALSE.")
  }
}

# How many records were excluded? Note only really informative if replace is 
# set to TRUE
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
# First drop any species that didn't have any observations after filtering
filtered_files <- base::setdiff(x = filtered_files, y = no_filtered_file)
# Now write files to archive
zip(zipfile = zipfile,
    files = filtered_files)
