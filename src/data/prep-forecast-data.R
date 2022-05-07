# Download and prepare bioclimatic variables for 2041-2070 and 2071-2100
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-05-06

require(sp)     # raster needs this
require(raster) # you know, raster stuff
require(dismo)  # calculating bioclimate variables

# Calculates average values for the 19 bioclimatic variables for two time 
# spans, 2041-2070 and 2071-2100, based on monthly values for the 30 year span. 
# The monthly data are based on ensemble forecasts under two SSPs: 4.5 and 7.0,
# for North America.
# The monthly data come from:
# https://adaptwest.databasin.org/pages/adaptwest-climatena/

# https://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6/ensembles/ensemble_ssp245_2041_monthly.zip

# For the approach on contemporary-ish climate data, see 
# src/data/prep-climate-data.R

# There are multiple bouts of iteration over the same combinations of 
# variables, while this processing could probably be done with a single bout of 
# iteration over SSPs and time periods, it is kept separate here to afford for 
# separation through time, to debug particular sections as necessary.

# Monthly variables; values are the suffixes used in filenames
monthly_vars <- c("tmin" = "Tmin", 
                  "tmax" = "Tmax", 
                  "prec" = "PPT")
# Used for file downloads, based on URL names at AWS
per_start <- c("2041", "2071")
# Two different ssps of interest (2 = 4.5, 3 = 7.0)
ssps <- c("ssp245", "ssp370")
# For writing raster files to disk
# temp_raster_format <- ".tif"
final_raster_format <- ".tif"
# Names of the variables, to be used in filenames et al
biovar_names <- paste0("bio", 1:19)
# Whether or not to re-calculate averages for the 19 bioclim variables
# overwrite_averages <- TRUE
# Whether or not to remove monthly tmin, tmax, prec after annual bioclim 
# variables have been calculated for that year
# remove_monthly <- TRUE
# Whether or not to remove annual bioclim data after the average has been 
# calculated for the time span of interest
# remove_annual <- FALSE
# Whether or not to remove the historic bioclim data after doing QA
remove_historic <- TRUE

# Check for data files for tmin, tmax, and prec; download from 
# https://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6/ensembles if not here, 
# and extract zip files
timeout_default <- getOption("timeout")
options(timeout = 15 * 60) # Set to 15 minutes, files are large (1GB)
base_url <- "https://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6/ensembles/"
for (ssp in ssps) {
  for (time_per in per_start) {
    # Download zip file if it doesn't exist
    zip_file <- paste0("data/ensemble/monthly/emsemble_", 
                       ssp, "_",
                       time_per, "_monthly.zip")
    if (!file.exists(zip_file)) {
      message(paste0("Downloading zip for ", ssp, ", ", time_per))
      file_url <- paste0(base_url, "ensemble_", 
                         ssp, "_", 
                         time_per, "_monthly.zip")
      message(paste0("from ", file_url))
      download.file(url = file_url,
                    destfile = zip_file)
    } else {
      message(paste0("Zip already present for ", ssp, ", ", time_per))
    }
    
    # if data files haven't been extracted yet, do that now
    # Extraction can take a minute
    one_data_file <- paste0("data/ensemble/monthly/ensemble_", 
                            ssp, "_",
                            time_per, "_Tmin12.tif")
    if (!file.exists(one_data_file)) {
      message(paste0("Extracting ", zip_file))
      # Archives contain top-level folder "monthly"
      unzip(zipfile = zip_file,
            exdir = "data/ensemble")
    } else {
      message(paste0(zip_file, " already extracted"))
    }
  }
}
options(timeout = timeout_default) # Reset to default

# Now have averages for each month

# Need to create a single RasterBrick/Stack for each of the three variables for 
# each SSP/time period combination, then feed them to dismo::biovars()

# Files have two-digit month, so creating a character vector for that
month_vec <- as.character(1:12)
month_vec[nchar(month_vec) == 1] <- paste0("0", month_vec[nchar(month_vec) == 1])

# The data coming in are much higher resolution that we need. Use the 
# calculated bioclim variables to get and update the resolution of the monthly
# temperature and precip data before calling dismo::biovars
template_raster <- raster(x = "data/wc2-1/bio1.tif")

for (ssp in ssps) {
  for (time_per in per_start) {
    raster_list <- vector("list", length(monthly_vars))
    names(raster_list) <- names(monthly_vars)
    # Create a RasterStack for each of the variables
    for (var_i in 1:length(monthly_vars)) {
      # doing this way 'cause iterator loses names
      one_var <- monthly_vars[var_i]
      one_var_name <- names(one_var)
      var_files <- as.list(paste0("data/ensemble/monthly/ensemble_",
                                  ssp, "_",
                                  time_per, "_",
                                  one_var, month_vec, ".tif"))
      raster_list[[one_var_name]] <- raster::stack(x = var_files)
      # Reproject the raster to the resolution & CRS of the contemporary data
      # Can take a a couple of minutes, so let user know progress
      message(paste0(Sys.time(), " | Reprojecting ", one_var_name, ", ", ssp, ", ", time_per))
      raster_list[[one_var_name]] <- raster::projectRaster(from = raster_list[[one_var_name]],
                                                             to = template_raster)
      message(paste0(Sys.time(), " | ", one_var_name, " reprojection complete"))
    }
    # Do biovar calculation for this SSP + time period; can take several (> 10) 
    # minutes
    message(paste0(Sys.time(), " | Calculating biovars for ", ssp, ", ", time_per))
    # dismo::biovars spits out a RasterBrick
    biovars <- dismo::biovars(prec = raster_list[["prec"]],
                              tmin = raster_list[["tmin"]],
                              tmax = raster_list[["tmax"]])
    message(paste0(Sys.time(), " | Finished calculating ", ssp, ", ", time_per))
    for (biovar_name in names(biovars)) {
      biovar_filename <- paste0("data/ensemble/", ssp, "/", time_per, "/",
                                biovar_name, final_raster_format)
      raster::writeRaster(x = biovars[[biovar_name]],
                          filename = biovar_filename,
                          overwrite = TRUE)
    }
  }
}



# QA, comparing averages for this time period to data available via 
# raster::getData(). These are for a (potentially) different time period 
# (1970-2000), but should still be useful to detect massive mistakes
historic_biovars <- raster::getData(name = "worldclim",
                                    var = "bio",
                                    res = 2.5,
                                    path = "data/wc2-1/historic")
# Crop to same extent as data we have
historic_biovars <- raster::crop(x = historic_biovars,
                                 y = geo_extent)
# Load in those averages we calculated above
biovar_filenames <- paste0("data/wc2-1/",
                           biovar_names,
                           final_raster_format)
current_biovars <- raster::stack(x = biovar_filenames)

# For these comparisons, because we used dismo::biovars(), all temperature 
# calculations are in degrees C, but the historic climate data is coming in 
# at 10 x degrees C (plot the bio1 layer for each to see the scales are an 
# order of magnitude different). To make meaningful deltas, multiply the 
# temperature layers for historic biovars by 0.1 before calculating delta
temp_biovars <- c("bio1", "bio2", "bio4", "bio5", "bio6", "bio7", "bio8", 
                  "bio9", "bio10", "bio11")
# Create data frame to hold measures of variance
biovar_qc <- data.frame(name = biovar_names,
                        mean_delta = NA)
# Do calculation for each layer, seems quicker this way
delta_raster_list <- list()
for (biovar_name in biovar_names) {
  cat("Calcluating delta for ", biovar_name, "...\n", sep = "")
  multfac <- 1
  if (biovar_name %in% temp_biovars) {
    multfac <- 0.1
  }
  delta <- current_biovars[[biovar_name]] - (multfac * historic_biovars[[biovar_name]])
  names(delta) <- biovar_name
  delta_raster_list[[biovar_name]] <- delta
  mean_delta <- raster::cellStats(x = delta, stat = "mean")
  biovar_qc$mean_delta[biovar_qc$name == biovar_name] <- mean_delta
}
# biovar_qc

# If we want to see where changes are happening, we can make a raster stack 
# and plot individual layers
# One a with "big" delta (bio4) just shows that it's driven by some Alaska and 
# Great Lakes; precip of wettest quarter (bio16) delta driven by BC and 
# Guatemala
# delta_stack <- raster::stack(x = delta_raster_list)
# plot(delta_stack[["bio4"]])
# plot(delta_stack[["bio16"]])


if (remove_historic) {
  historic_basenames <- paste0("data/wc2-1/historic/wc2-5/bio",
                               1:19)
  historic_filenames <- c(paste0(historic_basenames, ".bil"),
                          paste0(historic_basenames, ".hdr"))
  for (historic_filename in historic_filenames) {
    if (file.exists(historic_filename)) {
      invisible(file.remove(historic_filename))
    }
  }
}
