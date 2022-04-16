# Download and prepare bioclimatic variables for 2000-2019
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-04-15

library(raster)
library(dismo)

# Calculates average values for the 19 bioclimatic variables for 2000-2018, 
# based on monthly values for the 19 year span (yeah, two 19s, I'm sure this 
# won't cause any confusion). Monthly climate data (tmin, tmax, and prec) 
# sourced from https://worldclim.org/data/monthlywth.html.

# Much of the approach adapted from Keaton Wilson's work on Giant Swallowtails 
# at https://github.com/keatonwilson/swallowtail_ms, especially the code in the 
# appropriately named scripts/terraclim_nonsense.R

# TODO: Add toggle for removal of monthly tif files (they take up considerable
# HD space)
# TODO: Need to add check for existence of bioclim averages before extraction 
# (monthly file check is fine, but these might be deleted, but not necessary)
# TODO: Update filenames for prec and tmax 2018 December data - both have 
# (misleadingly) "2019" in their filenames instead of "2018"

# WorldClim variables
wc_vars <- c("tmin", "tmax", "prec")
# Used for file downloads, based on names at WorldClim site
time_periods <- c("2000-2009", "2010-2018")
# Used for annual biovariable calculations
year_span <- 2000:2018
# Rough extremes of Canada, Mexico, and USA
coord_bounds <- c("xmin" = -169,
                  "xmax" = -48, 
                  "ymin" = 13,
                  "ymax" = 75)
geo_extent <- raster::extent(x = coord_bounds)
# For writing raster files to disk
raster_format <- ".bil"
# Whether or not to re-calculate averages for the 19 bioclim variables
overwrite_averages <- FALSE

# Check for data files for tmin, tmax, and prec; download from 
# https://www.worldclim.org/data/monthlywth.html if not here, and 
# extract zip files
timeout_default <- getOption("timeout")
options(timeout = 10 * 60) # Set to 15 minutes, files are large
for (one_var in wc_vars) {
  for (time_per in time_periods) {
    # Data files doesn't exist, so download
    
    zip_file <- paste0("data/wc2-1/monthly/wc2.1_2.5m_", one_var, 
                       "_", time_per, ".zip")
    if (!file.exists(zip_file)) {
      message(paste0("Downloading zip for ", one_var, ", ", time_per))
      file_url <- paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_",
                         one_var, "_", time_per, ".zip")
      download.file(url = file_url,
                    destfile = zip_file)
    } else {
      message(paste0("Zip already present for ", one_var, ", ", time_per))
    }
    
    # if data files haven't been extracted yet, do that now
    # Extraction can take a minute
    final_year <- substr(x = time_per, start = 6, stop = 9)
    one_data_file <- paste0("data/wc2-1/monthly/wc2.1_2.5m_", one_var, "_", 
                            final_year, "-12.tif")
    if (!file.exists(one_data_file)) {
      message(paste0("Extracting ", zip_file))
      unzip(zipfile = zip_file,
            exdir = "data/wc2-1/monthly")
    }
  }
}
options(timeout = timeout_default) # Reset to default

# Now have monthly data 2000-2019, one for each month / year for each of three 
# variables

# Need to create single RasterBrick/Stack for each of the three variables for 
# a single year (so 12 layers each), then feed them to dismo::biovars()

# We calculate the average for the entire span, 2000-2018, so we start by 
# calculating biovar for each year
# Files have two-digit month, so creating a character vector for that
month_vec <- as.character(1:12)
month_vec[nchar(month_vec) == 1] <- paste0("0", month_vec[nchar(month_vec) == 1])

# Do biovar calculation for each year; biovars_annual will hold values for a 
# single year
biovars_annual <- vector("list", length(year_span))
biovar_names <- paste0("bio", 1:19)
# year_span <- 2000:2001 # For testing only
for (year_i in 1:length(year_span)) {
  one_year <- year_span[year_i]
  # Check to see if biovars have yet been calculated for this year; if not, do 
  # calculations and store in stack (and save to file); if so, read in values 
  # as RasterStack 
  # TODO: Want to save these as bil or grd or tif format?
  biovar_filenames <- paste0("data/wc2-1/annual/biovars-", 
                             one_year, "-",
                             biovar_names, 
                             raster_format)
  if (any(!file.exists(biovar_filenames))) {
    # A list of three elements, one corresponding to each of the variables (tmin, 
    # tmax, and prec). Each element will be a RasterStack of the 12 monthly 
    # layers for that variable
    raster_list <- vector("list", length(wc_vars))
    names(raster_list) <- wc_vars
    # Create a RasterStack for each of the variables for this year
    for (one_var in wc_vars) {
      var_files <- as.list(paste0("data/wc2-1/monthly/wc2.1_2.5m_",
                                  one_var, "_", 
                                  one_year, "-", 
                                  month_vec, ".tif")) 
      raster_list[[one_var]] <- raster::stack(x = var_files)
      # Restrict to geographical area of this study (CA, MX, US); cropping will 
      # take a few seconds
      raster_list[[one_var]] <- raster::crop(x = raster_list[[one_var]],
                                             y = geo_extent)
    }
    # Do biovar calculation for this year; can take several (> 10) minutes
    # See note on parallelization below
    message(paste0(Sys.time(), " | Calculating biovars for ", one_year))
    # Spits out a RasterBrick
    biovars_annual[[year_i]] <- dismo::biovars(prec = raster_list[["prec"]],
                                               tmin = raster_list[["tmin"]],
                                               tmax = raster_list[["tmax"]])
    message(paste0(Sys.time(), " | Finished calculating ", one_year, " biovars"))
    
    # Write each biovar to a raster file
    for (biovar_name in names(biovars_annual[[year_i]])) {
      biovar_filename <- paste0("data/wc2-1/annual/biovars-", 
                                one_year, "-",
                                biovar_name, 
                                raster_format)
      raster::writeRaster(x = biovars_annual[[year_i]][[biovar_name]],
                          filename = biovar_filename,
                          overwrite = TRUE)
    }
  } else { # biovars already calculated for this year, just read them in
    message(paste0("biovars already calculated for ", one_year, "; loading."))
    biovars_annual[[year_i]] <- raster::stack(x = biovar_filenames)
    # Blech
    names(biovars_annual[[year_i]]) <- biovar_names
  }
}

# Could do parallel if RAM is giant. Otherwise, will eat your computer.
# calc_biovar_annual <- function(x, year_span, wc_vars, month_vec, geo_extent) {
#   one_year <- year_span[x]
#   raster_list <- vector("list", length(wc_vars))
#   names(raster_list) <- wc_vars
#   # Create a RasterStack for each of the variables for this year
#   for (one_var in wc_vars) {
#     var_files <- as.list(paste0("data/wc2-1/monthly/wc2.1_2.5m_",
#                                 one_var, "_", 
#                                 one_year, "-", 
#                                 month_vec, ".tif")) 
#     raster_list[[one_var]] <- raster::stack(x = var_files)
#     # Restrict to geographical area of this study (CA, MX, US); cropping will 
#     # take a few seconds
#     raster_list[[one_var]] <- raster::crop(x = raster_list[[one_var]],
#                                            y = geo_extent)
#   }
#   # Do biovar calculation for this year; can take several minutes
#   # biovars_annual <- dismo::biovars(prec = raster_list[["prec"]],
#   #                                       tmin = raster_list[["tmin"]],
#   #                                       tmax = raster_list[["tmax"]])
#   # return(biovars_annual)
#   return(var_files)
# }
# ncores = parallel::detectCores() - 2
# cl = parallel::makeCluster(ncores)
# # Have to explicitly load libraries on each core; using assignment to keep 
# # things quiet
# a <- clusterEvalQ(cl, library(dismo))
# biovars_annual <- parallel::parLapply(cl = cl,
#                                       X = 1:length(year_span),
#                                       fun = calc_biovar_annual,
#                                       year_span = year_span,
#                                       wc_vars = wc_vars,
#                                       month_vec = month_vec,
#                                       geo_extent = geo_extent)
# 
# parallel::stopCluster(cl = cl)

# Now do calculation of means for each variable
for (biovar_name in biovar_names) {
  mean_filename <- paste0("data/wc2-1/", 
                          biovar_name, 
                          raster_format)
  if (!file.exists(mean_filename) | overwrite_averages) {
    message("Averaging ", biovar_name)
    # Pull corresponding RasterLayer out for this variable
    biovar_rasters <- lapply(X = biovars_annual, FUN = "[[", biovar_name)
    # Making a stack, but why?
    biovar_stack <- raster::stack(biovar_rasters)
    # Calculate mean of all layers (each layer is a year in this case)
    biovar_mean <- raster::calc(x = biovar_stack, fun = mean, na.rm = TRUE)
    raster::writeRaster(x = biovar_mean,
                        filename = mean_filename,
                        overwrite = TRUE)
  } else {
    message("Averages already on disk for ", biovar_name)
  }
}
