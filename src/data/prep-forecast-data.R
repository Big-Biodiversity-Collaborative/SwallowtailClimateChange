# Download and prepare bioclimatic variables for 2041-2070 and 2071-2100
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-05-06

require(sp)     # raster needs this
require(raster) # you know, raster stuff
require(dismo)  # calculating bioclimate variables
require(dplyr)  # QA/QC
require(ggplot2)# QA/QC

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

# Do some QA/QC for each of the forecast data sets

# Probably should pick one or two of the forecast GCMs available from worldclim
# and do those comparisons.

suppressWarnings({
  gfdl_data <- raster::getData(name = "CMIP5",
                               var = "bio",
                               res = 2.5,
                               rcp = 45,
                               model = "GD",
                               year = 70,
                               path = "data/")
})
names(gfdl_data) <- paste0("bio", 1:19)

# Get contemporary estimates, for a rough comparison
# current_biovars <- raster::stack(x = paste0("data/wc2-1/",
#                                             biovar_names, 
#                                             final_raster_format))

# Each element should be a list with two objects:
#  + delta_raster_list : rasters of delta values
#  + biovar_qc         : data frame of mean deltas for each biovar
qa_result_list <- list()

# Going to want to crop the gfdl data, but only once
gfdl_cropped <- FALSE

# For these comparisons, because we used dismo::biovars(), all temperature 
# calculations are in degrees C, but the GFDL climate data is coming in 
# at 10 x degrees C (plot the bio1 layer for each to see the scales are an 
# order of magnitude different). To make meaningful deltas, multiply the 
# temperature layers for historic biovars by 0.1 before calculating delta
# bio4 is still questionable...
# TODO: Current implementation does this scaling once for each forecast model 
# (i.e. four times); could do it once on GFDL RasterStack?
temp_biovars <- c("bio1", "bio2", "bio4", "bio5", "bio6", "bio7", "bio8", 
                  "bio9", "bio10", "bio11")

# Iterate over all SSP and time period combinations
for (ssp in ssps) {
  for (time_per in per_start) {
    gcm_name <- paste0(ssp, "_", time_per)
    message(paste0("Delta calculations ", ssp, ", ", time_per))
    # Load in those averages we calculated above
    biovar_filenames <- paste0("data/ensemble/",
                               ssp, "/",
                               time_per, "/",
                               biovar_names,
                               final_raster_format)
    forecast_biovars <- raster::stack(x = biovar_filenames)

    if (!gfdl_cropped) {
      gfdl_data <- raster::crop(x = gfdl_data, y = forecast_biovars)
    }
    
    biovar_qc <- data.frame(name = biovar_names,
                            mean_delta = NA)
    # A list to hold delta rasters, that is one raster for each of the biovars,
    # that has, as raster values, the difference of the current - forecast 
    # values for that biovar.
    delta_raster_list <- list()
    # Do calculation for each layer, seems quicker this way
    for (biovar_name in biovar_names) {
      cat("Calcluating delta for ", biovar_name, "...\n", sep = "")
      multfac <- 1
      if (biovar_name %in% temp_biovars) {
        multfac <- 0.1
      }
      delta <- forecast_biovars[[biovar_name]] - (multfac * gfdl_data[[biovar_name]])
      names(delta) <- biovar_name
      delta_raster_list[[biovar_name]] <- delta
      mean_delta <- raster::cellStats(x = delta, stat = "mean", na.rm = TRUE)
      biovar_qc$mean_delta[biovar_qc$name == biovar_name] <- mean_delta
    }
    # Add the results to the large QA/QC list
    qa_result_list[[gcm_name]] <- list(delta_raster_list = delta_raster_list,
                                       biovar_qc = biovar_qc)
  }
}

# For some quick, eyeball check, will plot the mean deltas for all four 
# forecast scenarios 
deltas_df <- dplyr::bind_rows(lapply(X = qa_result_list,
                                     FUN = "[[",
                                     "biovar_qc"),
                              .id = "GCM")
deltas_plot <- ggplot(data = deltas_df, 
                      mapping = aes(x = GCM, 
                                    y = mean_delta,
                                    color = GCM)) +
  geom_point() +
  facet_wrap(~ name, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_blank())
# print(deltas_plot)

# Plot individual biovars and variation
# plot(qa_result_list[["ssp245_2041"]][["delta_raster_list"]][["bio12"]])
# plot(qa_result_list[["ssp370_2071"]][["delta_raster_list"]][["bio12"]])

forecast_ssp245_2041 <- raster::stack(x = paste0("data/ensemble/",
                                                 "ssp245/",
                                                 "2041/",
                                                 biovar_names,
                                                 final_raster_format))
forecast_ssp370_2071 <- raster::stack(x = paste0("data/ensemble/",
                                                 "ssp370/",
                                                 "2071/",
                                                 biovar_names,
                                                 final_raster_format))
# plot(gfdl_data[["bio12"]])
# plot(forecast_ssp245_2041[["bio12"]])
# plot(forecast_ssp370_2071[["bio12"]])
