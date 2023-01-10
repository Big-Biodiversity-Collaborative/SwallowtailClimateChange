# Download and prepare bioclimatic variables for 2000-2018
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-04-15

require(sp)     # raster needs this
require(raster) # you know, raster stuff
require(dismo)  # calculating bioclimate variables
require(terra)  # raster manipulation

# TODO: Migrate away from use of raster package; most functionality provided by 
# the terra package. On hold for now, as the dismo::biovars function still 
# requires RasterBrick/Stack objects as input.

# TODO: Add check for existence of bioclim averages before extraction

# Calculates average values for the 19 bioclimatic variables for 2000-2018, 
# based on monthly values for the 19 year span (yeah, two 19s, I'm sure this 
# won't cause any confusion). Monthly climate data (tmin, tmax, and prec) 
# sourced from https://worldclim.org/data/monthlywth.html.

# Much of the approach adapted from Keaton Wilson's work on Giant Swallowtails 
# at https://github.com/keatonwilson/swallowtail_ms, especially the code in the 
# appropriately named scripts/terraclim_nonsense.R

# WorldClim variables
wc_vars <- c("tmin", "tmax", "prec")
# Used for file downloads, based on names at WorldClim site
time_periods <- c("2000-2009", "2010-2018")
# Used for annual biovariable calculations
year_span <- 2000:2018
# Rough bounding box for Canada + Mexico + USA
coord_bounds <- c("xmin" = -169,
                  "xmax" = -48, 
                  "ymin" = 13,
                  "ymax" = 75)
# geo_extent <- terra::ext(x = coord_bounds)
geo_extent <- raster::extent(x = coord_bounds)
# For writing raster files to disk
temp_raster_format <- ".tif"
annual_raster_format <- ".bil"
final_raster_format <- ".tif"
# Names of the variables, to be used in filenames et al
biovar_names <- paste0("bio", 1:19)
# Whether or not to re-calculate averages for the 19 bioclim variables
overwrite_averages <- TRUE
# Whether or not to remove monthly tmin, tmax, prec after annual bioclim 
# variables have been calculated for that year
remove_monthly <- TRUE
# Whether or not to remove annual bioclim data after the average has been 
# calculated for the time span of interest
remove_annual <- FALSE
# Whether or not to remove climate data (i.e. set to NA) from cells in the 
# Great Lakes and other large bodies of water
remove_lakes <- TRUE
# Whether or not to remove the historic bioclim data after doing QA
remove_historic <- TRUE

# Check for data files for tmin, tmax, and prec; download from 
# https://www.worldclim.org/data/monthlywth.html if not here, and 
# extract zip files
timeout_default <- getOption("timeout")
options(timeout = 15 * 60) # Set to 15 minutes, files are large
for (one_var in wc_vars) {
  for (time_per in time_periods) {
    # Download zip file if it doesn't exist
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
    final_year <- substr(x = time_per, start = 6, stop = 9)
    one_data_file <- paste0("data/wc2-1/monthly/wc2.1_2.5m_", one_var, "_", 
                            final_year, "-11.tif")
    if (!file.exists(one_data_file)) {
      # Extraction can take a minute, so notify user
      message(paste0("Extracting ", zip_file))
      unzip(zipfile = zip_file,
            exdir = "data/wc2-1/monthly")
      # Some December 2018 files are named wrong ("2019" instead of "2018"), 
      # fix this; only necessary for second archive (2010-2018)
      if (final_year == 2018) {
        dec_19_file <- paste0("data/wc2-1/monthly/wc2.1_2.5m_", one_var, 
                              "_2019-12.tif")
        if (file.exists(dec_19_file)) {
          dec_18_file <- paste0("data/wc2-1/monthly/wc2.1_2.5m_", one_var, 
                                "_2018-12.tif")
          invisible(file.rename(from = dec_19_file,
                                to = dec_18_file))
        }
      }
    }
  }
}
options(timeout = timeout_default) # Reset to default

# Now have monthly data 2000-2018, one for each month / year for each of three 
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
# One could do this operation if the RAM on your machine is giant. Otherwise 
# (i.e. on your computer, most likely), if you try to parallelize this step it 
# will eat your computer.
for (year_i in 1:length(year_span)) {
  one_year <- year_span[year_i]
  # Check to see if biovars have yet been calculated for this year; if not, do 
  # calculations and store in stack (and save to file); if so, read in values 
  # as RasterStack 
  annual_filenames <- paste0("data/wc2-1/annual/biovars-", 
                             one_year, "-",
                             biovar_names, 
                             annual_raster_format)
  if (any(!file.exists(annual_filenames))) {
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
      # raster_list[[one_var]] <- terra::rast(x = var_files)
      raster_list[[one_var]] <- raster::stack(x = var_files)
      # Restrict to geographical area of this study (CA, MX, US); cropping will 
      # take a few seconds
      # raster_list[[one_var]] <- terra::crop(x = raster_list[[one_var]],
      #                                       y = geo_extent)
      raster_list[[one_var]] <- raster::crop(x = raster_list[[one_var]],
                                             y = geo_extent)
    }
    # Do biovar calculation for this year; can take several (> 10) minutes
    message(paste0(Sys.time(), " | Calculating biovars for ", one_year))
    # dismo::biovars spits out a RasterBrick
    biovars_annual[[year_i]] <- dismo::biovars(prec = raster_list[["prec"]],
                                               tmin = raster_list[["tmin"]],
                                               tmax = raster_list[["tmax"]])
    message(paste0(Sys.time(), " | Finished calculating ", one_year, " biovars"))
    
    # Write each biovar to a raster file
    for (biovar_name in names(biovars_annual[[year_i]])) {
      biovar_filename <- paste0("data/wc2-1/annual/biovars-", 
                                one_year, "-",
                                biovar_name, 
                                annual_raster_format)
      # terra::writeRaster(x = biovars_annual[[year_i]][[biovar_name]],
      #                    filename = biovar_filename,
      #                    overwrite = TRUE)
      raster::writeRaster(x = biovars_annual[[year_i]][[biovar_name]],
                          filename = biovar_filename,
                          overwrite = TRUE)
      # Remove associated metadata files
      metadata_filename <- paste0(biovar_filename, ".aux.xml")
      if (file.exists(metadata_filename)) {
        invisible(file.remove(metadata_filename))
      }
    }
  } else { # biovars already calculated for this year, just read them in
    message(paste0("biovars already calculated for ", one_year, "; loading."))
    # biovars_annual[[year_i]] <- terra::rast(x = annual_filenames)
    biovars_annual[[year_i]] <- raster::stack(x = annual_filenames)
    # Blech. Have to re-assign layer names.
    names(biovars_annual[[year_i]]) <- biovar_names
  }
  # If necessary, remove monthly files used for biovar calculation
  if (remove_monthly) {
    for (var_i in 1:length(wc_vars)) {
      one_var <- wc_vars[var_i]
      for (month_i in 1:length(month_vec)) {
        one_month <- month_vec[month_i]
        month_file <- paste0("data/wc2-1/monthly/wc2.1_2.5m_",
                             one_var, "_",
                             one_year, "-",
                             one_month, 
                             temp_raster_format)
        if (file.exists(month_file)) {
          invisible(file.remove(month_file))
        }
      }
    }
  }
}

# Now do calculation of means for each variable; before writing to disk, mask 
# out Great Lakes and other large bodies of water
if (remove_lakes) { # Only load shapefile if necessary
  # lakes_shp <- terra::vect(x = "data/lakes/ne_10m_lakes.shp")
  lakes_shp <- raster::shapefile(x = "data/lakes/ne_10m_lakes.shp")
}
for (biovar_name in biovar_names) {
  mean_filename <- paste0("data/wc2-1/", 
                          biovar_name, 
                          final_raster_format)
  if (!file.exists(mean_filename) | overwrite_averages) {
    message("Averaging ", biovar_name)
    # Pull corresponding RasterLayer out for this variable
    biovar_rasters <- lapply(X = biovars_annual, FUN = "[[", biovar_name)
    # Making a stack, but why?
    # biovar_stack <- terra::rast(biovar_rasters)
    biovar_stack <- raster::stack(biovar_rasters)
    # Calculate mean of all layers (each layer is a year in this case)
    # biovar_mean <- terra::app(x = biovar_stack, fun = mean, na.rm = TRUE)
    biovar_mean <- raster::calc(x = biovar_stack, fun = mean, na.rm = TRUE)
    # Set the CRS to WGS84
    terra::crs(biovar_mean) <- "EPSG:4326"
    
    # Here we can mask out the Great Lakes (and other large bodies of water we 
    # want to exclude from predictions) as appropriate
    if (remove_lakes) {
      # biovar_mean <- terra::mask(x = biovar_mean,
      #                            mask = lakes_shp,
      #                            inverse = TRUE)
      biovar_mean <- raster::mask(x = biovar_mean,
                                  mask = lakes_shp,
                                  inverse = TRUE)
    }
    # terra::writeRaster(x = biovar_mean,
    #                    filename = mean_filename,
    #                    overwrite = TRUE)
    raster::writeRaster(x = biovar_mean,
                        filename = mean_filename,
                        overwrite = TRUE)
    # When using raster::writeRaster, a small XML file of metadata is written,
    # we do not need these files
    metadata_filename <- paste0(mean_filename, ".aux.xml")
    if (file.exists(metadata_filename)) {
      invisible(file.remove(metadata_filename))
    }
    
  } else {
    message("Averages already on disk for ", biovar_name)
  }
}

# Remove annual biovariable calculations as necessary
if (remove_annual) {
  for (one_biovar in biovar_names) {
    for (one_year in year_span) {
      annual_basename <- paste0("data/wc2-1/annual/biovars-",
                                one_year, "-",
                                one_biovar)
      for (extension in c("bil", "hdr", "stx", "tif")) {
        annual_filename <- paste0(annual_basename, ".", extension)
        if (file.exists(annual_filename)) {
          invisible(file.remove(annual_filename))
        }
      }
    }
  }
}

################################################################################
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
