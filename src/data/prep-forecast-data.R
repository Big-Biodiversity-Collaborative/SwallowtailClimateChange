# Download and prepare bioclimatic variables for 2041-2070 and 2071-2100
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-05-06

require(terra)  # raster manipulation
require(predicts) # calculating bioclimate variables
require(dplyr)  # QA/QC
require(ggplot2)# QA/QC

# Calculates average values for the 19 bioclimatic variables for two time 
# spans, 2041-2070 and 2071-2100, based on monthly values for the 30 year span. 
# The monthly data are based on ensemble forecasts under three SSPs: 4.5, 7.0, 
# 8.5 for North America.
# The monthly data come from:
# https://adaptwest.databasin.org/pages/adaptwest-climatena/
# Example link:
# https://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6v73/ensembles/ensemble_8GCMs_ssp245_2041_2060_monthly.zip

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
per_starts <- c("2041", "2071")
# Three different ssps of interest (2 = 4.5, 3 = 7.0, 5 = 8.5)
ssps <- c("ssp245", "ssp370", "ssp585")
# For writing raster files to disk
final_raster_format <- ".tif"
# Names of the variables, to be used in filenames et al
biovar_names <- paste0("bio", 1:19)
# Whether or not to remove the historic bioclim data after doing QA
remove_historic <- TRUE
# Whether or not replace calculated values if they already exist on disk
replace_rasters <- FALSE
# Monthly files have two-digit month, so creating a character vector for that
month_vec <- as.character(1:12)
month_vec[nchar(month_vec) == 1] <- paste0("0", month_vec[nchar(month_vec) == 1])

# Check for data files for tmin, tmax, and prec; download from 
# https://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6v73/ensembles if not here, 
# and extract zip files
timeout_default <- getOption("timeout")
options(timeout = 30 * 60) # Set to 30 minutes, files are large (1.3GB)
# e.g. https://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6v73/ensembles/ensemble_8GCMs_ssp245_2041_2070_monthly.zip
base_url <- "https://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6v73/ensembles/"

for (ssp in ssps) {
  for (per_start in per_starts) {
    # Calculate time period (have the start and need the end; necessary for URL)
    per_end <- as.integer(per_start) + 29
    time_per <- paste0(per_start, "_", per_end)

    # Download zip file if it doesn't exist
    zip_file <- paste0("data/ensemble/monthly/emsemble_", 
                       ssp, "_",
                       per_start, "_monthly.zip")
    if (!file.exists(zip_file)) {
      message(paste0("Downloading zip for ", ssp, ", ", per_start))
      file_url <- paste0(base_url, "ensemble_8GCMs_", 
                         ssp, "_", 
                         time_per, "_monthly.zip")
      message(paste0("from ", file_url))
      download.file(url = file_url,
                    destfile = zip_file)
    } else {
      message(paste0("Zip already present for ", ssp, ", ", per_start))
    }
    
    # if data files haven't been extracted yet, do that now
    # Extraction can take a minute
    one_data_file <- paste0("data/ensemble/monthly/ensemble_", 
                            ssp, "_",
                            per_start, "_Tmin12.tif")
    if (!file.exists(one_data_file)) {
      message(paste0("Extracting ", zip_file))
      # Archives contain top-level folder "monthly"
      unzip(zipfile = zip_file,
            exdir = "data/ensemble")
      # File structure & naming is fairly ugly, e.g.
      # ensemble_8GCMs_ssp245_2041_2070/ensemble_8GCMs_ssp245_2041_2070_monthly/ensemble_8GCMs_ssp245_2041_2070_PPT01.tif, or

      # Need to move these _back_ to data/ensemble/monthly and rename
      # ensemble_ssp245_2041_PPT01.tif
      
      # Two step paste because I'm lazy
      month_varnames <- paste0(rep(monthly_vars, each = length(month_vec)), month_vec)
      to_move <- paste0("data/ensemble/",
                        "ensemble_8GCMs_", ssp, "_", time_per, "/",
                        "ensemble_8GCMs_", ssp, "_", time_per, "_monthly/",
                        "ensemble_8GCMs_", ssp, "_", time_per, "_", 
                        month_varnames, ".tif")

      # Figure out the destination names
      move_to <- paste0("data/ensemble/monthly/ensemble_", ssp, "_", per_start, "_", 
                        month_varnames, ".tif")
      if (!all(file.rename(from = to_move, to = move_to))) {
        warning("Warning: there was a problem moving monthly files for ", ssp,
                ", ", per_start)
      }
      # We do not need Tave monthly files, and will cause problems later if we 
      # do not remove them
      taves <- paste0("data/ensemble/",
                      "ensemble_8GCMs_", ssp, "_", time_per, "/",
                      "ensemble_8GCMs_", ssp, "_", time_per, "_monthly/",
                      "ensemble_8GCMs_", ssp, "_", time_per, "_Tave", 
                      month_vec, ".tif")
      if (!all(file.remove(taves))) {
        warning("There was a problem removing Tave files for ", ssp, ", ", per_start)
      }
      
      # Files are moved, ditch those icky folders
      to_remove_outer <- paste0("data/ensemble/",
                                "ensemble_8GCMs_", ssp, "_", time_per)
      to_remove_inner <- paste0(to_remove_outer, "/",
                                "ensemble_8GCMs_", ssp, "_", time_per, "_monthly")
      if (!file.remove(to_remove_inner)) {
        warning("There was a problem removing temporary folder ", to_remove_inner)
      }
      if (!file.remove(to_remove_outer)) {
        warning("There was a problem removing temporary folder ", to_remove_outer)
      }
    } else {
      message(paste0(zip_file, " already extracted"))
    }
  }
}
options(timeout = timeout_default) # Reset to default

# Now have averages for each month

# Need to create a single SpatRaster for each of the three variables for 
# each SSP/time period combination, then feed them to predicts::bcvars()

# The data coming in are much higher resolution that we need. Use the 
# calculated bioclim variables to get and update the resolution of the monthly
# temperature and precip data before calling predicts::bcvars
template_raster <- terra::rast(x = "data/wc2-1/bio1.tif")

for (ssp in ssps) {
  for (per_start in per_starts) {
    bioclim_files <- paste0("data/ensemble/", ssp, "/", per_start, "/", biovar_names, ".tif")
    files_exist <- all(file.exists(bioclim_files))
    if (replace_rasters | !files_exist) {
      raster_list <- vector("list", length(monthly_vars))
      names(raster_list) <- names(monthly_vars)
      # Create a SpatRaster for each of the variables
      for (var_i in 1:length(monthly_vars)) {
        # doing this way because iterator loses names
        one_var <- monthly_vars[var_i]
        one_var_name <- names(one_var)
        var_files <- as.list(paste0("data/ensemble/monthly/ensemble_",
                                    ssp, "_",
                                    per_start, "_",
                                    one_var, month_vec, ".tif"))
        # Each list element is a 12-element list (one for each month)
        raster_list[[one_var_name]] <- c(lapply(X = var_files,
                                                FUN = terra::rast))
        # Call terra::rast (again?) to make each top-level element a single 
        # SpatRaster with 12 layers
        raster_list[[one_var_name]] <- terra::rast(raster_list[[one_var_name]])
        # Reproject the raster to the resolution & CRS of the contemporary data
        # This changes the resolution from 30s to 2.5 minutes
        # Can take a minute or so - let user know progress
        message(paste0(Sys.time(), " | Reprojecting ", one_var_name, ", ", ssp, 
                       ", ", per_start))
        raster_list[[one_var_name]] <- terra::project(x = raster_list[[one_var_name]],
                                                      y = template_raster)
        message(paste0(Sys.time(), " | ", one_var_name, " reprojection complete"))
        # Now mask the raster by the template raster; primary goal is to ensure 
        # we are masking out any cells that are missing data in contemporary 
        # climate raster (i.e. the Great Lakes, which we mask out in preparing 
        # the contemporary bioclim variables; see src/data/prep-climate-data.R)
        raster_list[[one_var_name]] <- terra::mask(x = raster_list[[one_var_name]],
                                                   mask = template_raster)
        message(paste0(Sys.time(), " | ", one_var_name, " masking complete"))
      }
      # Do biovar calculation for this SSP + time period; can take several (> 10) 
      # minutes
      message(paste0(Sys.time(), " | Calculating biovars for ", ssp, ", ", per_start))
      biovars <- predicts::bcvars(prec = raster_list[["prec"]],
                                  tmin = raster_list[["tmin"]],
                                  tmax = raster_list[["tmax"]])
      message(paste0(Sys.time(), " | Finished calculating ", ssp, ", ", per_start))
      # Iterate over the 19 biovars (bio1, bio2, bio3...bio19) and write to file
      # Make sure destination directories exist and create if it does not
      ssp_destination <- paste0("data/ensemble/", ssp)
      if (!dir.exists(ssp_destination)) {
        dir.create(ssp_destination)
      }
      per_start_destination <- paste0("data/ensemble/", ssp, "/", per_start)
      if (!dir.exists(per_start_destination)) {
        dir.create(per_start_destination)
      }
      for (biovar_name in names(biovars)) {
        biovar_filename <- paste0(per_start_destination, "/",
                                  biovar_name, final_raster_format)
        terra::writeRaster(x = biovars[[biovar_name]],
                           filename = biovar_filename,
                           overwrite = TRUE)
        # Remove associated metadata files
        metadata_filename <- paste0(biovar_filename, ".aux.xml")
        if (file.exists(metadata_filename)) {
          invisible(file.remove(metadata_filename))
        }
      }
    } else {# End conditional for file exists or force replacement
      message("Bioclim files for ", ssp, ", ", per_start, " already exist and replace is set to FALSE.")
    }
  } # End iteration over time periods
} # End iteration over ssps

# Do some QA/QC for each of the forecast data sets; still relies on raster
# package

# Check with some of the biovars that are included at 
# https://adaptwest.databasin.org/pages/adaptwest-climatena/
# (there are 33 bioclimatic variables listed there, but the list does *not* 
# include most of the standard 19 bioclimatic variables) Comparable ones are:
# MAT = bio1 = Mean annual temperature
# MAP = bio12 = Mean annual precipitation

# Each element should be a list with two objects:
#  + delta_raster_list : rasters of delta values
#  + biovar_qc         : data frame of mean deltas for each biovar
qa_result_list <- list()

timeout_default <- getOption("timeout")
options(timeout = 15 * 60) # Set to 15 minutes, files are large (1GB)
base_url <- "https://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6/ensembles/"
for (ssp in ssps) {
  for (per_start in per_starts) {
    gcm_name <- paste0(ssp, "_", per_start)
    bioclim_zip <- paste0("data/ensemble/", ssp, "_", per_start, ".zip")
    # If the zip file hasn't been downloaded yet, do so now
    if (!file.exists(bioclim_zip)) {
      bioclim_url <- paste0(base_url, "ensemble_",
                            ssp, "_",
                            per_start, "_bioclim.zip")
      # Download zip file to data/ensemble (it has a bioclim folder inside the
      # archive)
      message(paste0("Downloading zip file for ", gcm_name, "."))
      download.file(url = bioclim_url,
                    destfile = bioclim_zip)
      
    }
    # Extract specific files; namely the MAT and MAP tifs; those are the only 
    # ones we can really compare
    mat_map <- paste0("bioclim/ensemble_", 
                      ssp, "_", 
                      per_start, "_",
                      c("MAT", "MAP"),
                      ".tif")
    unzip(zipfile = bioclim_zip,
          files = mat_map,
          exdir = "data/ensemble")
    biovar_compare <- c("bio1", "bio12")
    # Now do delta calculations for those two variables
    message(paste0("Delta calculations ", gcm_name))
    biovar_filenames <- paste0("data/ensemble/",
                               ssp, "/",
                               per_start, "/",
                               biovar_compare,
                               final_raster_format)
    forecast_biovars <- raster::stack(x = biovar_filenames)
    
    biovar_qc <- data.frame(name = biovar_compare,
                            mean_delta = NA,
                            sd_delta = NA)
    # A list to hold delta rasters, that is one raster for each of the biovars,
    # that has, as raster values, the difference of the current - forecast 
    # values for that biovar.
    delta_raster_list <- list()

    # Read in predictions from archive, re-project (which also crops), then 
    # compare to our calculated predictions
    for (biovar_name in biovar_compare) {
      cat("Calcluating delta for ", biovar_name, "...\n", sep = "")
      comparable_file <- if_else(biovar_name == "bio1",
                                 "MAT",
                                 "MAP")
      archive_forecast <- raster(x = paste0("data/ensemble/bioclim/ensemble_",
                                            ssp, "_",
                                            per_start, "_",
                                            comparable_file, ".tif"))
      archive_forecast <- raster::projectRaster(from = archive_forecast,
                                                to = forecast_biovars[[biovar_name]])
      
      delta <- forecast_biovars[[biovar_name]] - archive_forecast
      names(delta) <- biovar_name
      delta_raster_list[[biovar_name]] <- delta
      mean_delta <- raster::cellStats(x = delta, stat = "mean", na.rm = TRUE)
      sd_delta <- raster::cellStats(x = delta, stat = "mean", na.rm = TRUE)
      biovar_qc$mean_delta[biovar_qc$name == biovar_name] <- mean_delta
      biovar_qc$sd_delta[biovar_qc$name == biovar_name] <- sd_delta
    }
    # Add the results to the large QA/QC list
    qa_result_list[[gcm_name]] <- list(delta_raster_list = delta_raster_list,
                                       biovar_qc = biovar_qc)
  }
}
options(timeout = timeout_default) # Reset to default

# For some quick, eyeball check, will plot the mean deltas for all six 
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
  geom_errorbar(mapping = aes(ymin = mean_delta - sd_delta,
                              ymax = mean_delta + sd_delta)) +
  facet_wrap(~ name, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_blank())
# print(deltas_plot)

# Look at individual deltas
# plot(qa_result_list[["ssp245_2041"]][["delta_raster_list"]][["bio1"]])
# plot(qa_result_list[["ssp245_2041"]][["delta_raster_list"]][["bio12"]])
# plot(qa_result_list[["ssp370_2071"]][["delta_raster_list"]][["bio12"]])
# plot(qa_result_list[["ssp585_2071"]][["delta_raster_list"]][["bio12"]])