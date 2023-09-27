# Generate presence-absence dataset for each species, to be used in any SDM
# Erin Zylstra
# ezylstra@arizona.edu
# 2023-09-27

require(terra)
require(sp)
require(dplyr)
require(ENMeval)

replace <- TRUE
verbose <- TRUE

# Minimum number of gbif records required to save a csv with presence-absence 
# data to file (note that the number of of records in SDM training datasets will 
# be 80% of this number)
min_records <- 40

# Number of background (pseudo-absence) points to generate for each species
n_background <- 10000

# Read in gbif-reconcile
species_list <- read.csv("data/gbif-reconcile.csv")

# Grab one of the climate tif files to use as a mask to generate 
# pseudo-absence points
tif_file <- list.files(path = "data/wc2-1", 
                       pattern = ".tif$", 
                       full.names = TRUE)[1]
predictor <- terra::rast(tif_file)

# Create/amend a table that summarizes data available for each species
gbif_pa_file <- "data/gbif-pa-summary.csv"
if (file.exists(gbif_pa_file)) {
  gbif_pa <- read.csv(gbif_pa_file)
  gbif_pa <- gbif_pa[order(match(gbif_pa$species, species_list$accepted_name)),]
} else {
  gbif_pa <- as.data.frame(matrix(NA, nrow = nrow(species_list), ncol = 6))
  colnames(gbif_pa) <- c("species", "n_filtered", "n_background",
                         "filtered_csv", "pa_csv", "mcp_shapefile")
}

set.seed(20221109)

# Loop through all species in gbif_reconcile
for (i in 1:nrow(species_list)) {
  species <- species_list$accepted_name[i]
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species))
  pa_file <- paste0("data/gbif/presence-absence/", nice_name, "-pa.csv")
  
  # Only proceed if file doesn't exist or we want to replace existing files
  if (file.exists(pa_file) & replace == FALSE) next
  
  if (verbose) {
    message(paste0("\n****  Beginning process for ", species, "  ****"))
  }    
  
  gbif_pa$species[i] <- species_list$accepted_name[i]
  
  # Load observation data
  obs_file <- paste0("data/gbif/filtered/",
                     nice_name,
                     "-gbif.csv")
  # Unzip the first time around to make sure we're using data from the most 
  # recent download
  if (i == 1) {
    unzip(zipfile = "data/gbif-filtered.zip", overwrite = TRUE) 
  }

  # There's a chance that a file might not be present, even after unzipping; 
  # note this and move on.
  if (!file.exists(obs_file)) {
    gbif_pa$n_filtered[i] <- 0
    gbif_pa$n_background[i] <- 0
    gbif_pa$filtered_csv[i] <- "no"
    gbif_pa$pa_csv[i] <- "no"
    gbif_pa$mcp_shapefile[i] <- "no"
    message(paste0("No filtered data for ", species, " on disk"))
  } else {
    obs <- read.csv(file = obs_file)

    # Skip species that have fewer than the minimum number of filtered records 
    # indicated above; if a P/A file exists, remove it
    if (nrow(obs) < min_records) {
      gbif_pa$n_filtered[i] <- nrow(obs)
      gbif_pa$n_background[i] <- 0
      gbif_pa$filtered_csv[i] <- "yes"
      gbif_pa$pa_csv[i] <- "no"
      gbif_pa$mcp_shapefile[i] <- "no"
      # If a presence absence file for this species exists, we need to remove 
      # it so SDMs won't be attempted
      if (file.exists(pa_file)) {
        file.remove(pa_file)
        message(paste0("Less than ", min_records, " filtered records for ", 
                       species, ". No background points created and previous file on disk removed."))
      } else {
        message(paste0("Less than ", min_records, " filtered records for ", 
                       species, ". No background points created."))
      }
    } else {
      # Retain just the geographic coordinates
      presence <- obs %>%
        dplyr::select(longitude, latitude) %>%
        dplyr::rename(x = longitude,
                      y =latitude)

      # Convert to a SpatialPoints object using WGS84 CRS
      presence_sp <- SpatialPoints(coords = presence,
                                   proj4string = CRS("EPSG:4326"))

      # Calculate the GreatCircle distance (in km) between points (can take
      # minutes for larger data sets)
      gc_dist <- sp::spDists(presence_sp, longlat = TRUE) 
      
      # Calculate the maximum of nearest neighbor distances (in km)
      buffer <- gc_dist %>%
        apply(., 1, function(x) min(x[x > 0])) %>%
        max %>%
        round
      rm(gc_dist)

      # Garbage can pile up at this point. Clean it up.
      invisible(gc())

      # Use terra functions to define the minimum convex polygon (MCP)
      ch <- terra::convHull(x = terra::vect(x = presence,
                                            geom = c("x", "y"),
                                            crs = "EPSG:4326"))
      # Adding a buffer; width takes argument in meters
      ch_buffer <- terra::buffer(x = ch, width = buffer * 1000)

      # Save buffered MCP as shapefile
      shapefile_name <- paste0("data/gbif/shapefiles/", 
                               nice_name, "-buffered-mcp.shp") 
      # Below seems unnecessary, but leaving a commented out version in case.
        # GDAL/terra will not let us overwrite existing file (as of 2023-02-13),
        # even when passing overwrite = TRUE
        # So we have to manually remove related files first, then write
        # if (file.exists(shapefile_name)) {
        #   to_delete <- list.files(path = "data/gbif/shapefiles/", 
        #                           pattern = paste0(nice_name, "-buffered-mcp"),
        #                           full.names = TRUE)
        #   invisible(file.remove(to_delete))
        # }
      terra::writeVector(x = ch_buffer, 
                         filename = shapefile_name,
                         overwrite = TRUE)

      # Crop and mask climate data to the buffered MCP polygon
      pred_mask <- predictor %>%
        terra::crop(ch_buffer) %>%
        terra::mask(ch_buffer)

      # Generate pseudo-absence points
      absence <- suppressWarnings(terra::spatSample(x = pred_mask,  
                                                    size = n_background,
                                                    method = "random",
                                                    na.rm = TRUE,
                                                    values = FALSE,
                                                    xy = TRUE))
      # Note: if buffered area is small, function may not be able to generate 
      # the indicated number of points (size) because the default is to select 
      # cells without replacement. If this happens, R will return a warning:
      # [spatSample] fewer cells returned than requested, so wrapped everything
      # in suppressWarnings()
      
      # Reality check:
      # plot(ch_buffer)
      # plot(ch, add = TRUE)
      # points(y ~ x, data = absence, cex = 0.5, col = "gray")
      # points(y ~ x, data = presence, cex = 0.5, col = "blue")
      
      # Make a vector of appropriate length with 0/1 values for 
      # (pseudo)absence/presence
      pa_data <- c(rep(x = 1, times = nrow(presence)), 
                   rep(x = 0, times = nrow(absence)))  
      
      # Create spatial blocks/folds with ENM eval (creates 4 folds via lat/long
      # with relatively even number of occurrences in each partition)
      set.seed(1234)
      fold <- ENMeval::get.block(presence, absence)
      
      # Check number of presences in each fold and view bg points by fold:
      # table(fold$occs.grp)
      # evalplot.grps(pts = absence, pts.grp = fold$bg.grp,
      #               envs = raster::stack(pred_mask))
      
      # Combine presence / pseudo-absence data
      full_data <- data.frame(cbind(pa = pa_data,
                                    fold = c(fold$occs.grp, fold$bg.grp),
                                    rbind(presence, absence)))      
      
      write.csv(x = full_data,
                file = pa_file,
                row.names = FALSE)
      
      gbif_pa$n_filtered[i] <- nrow(presence)
      gbif_pa$n_background[i] <- nrow(absence)
      gbif_pa$filtered_csv[i] <- "yes"
      gbif_pa$pa_csv[i] <- "yes"
      gbif_pa$mcp_shapefile[i] <- "yes"
      
      if (verbose) {
        message(paste0("****  ",nrow(presence), " gbif records and ", 
                       nrow(absence), " pseudo-absence records written to ",
                       pa_file, "  ****"))
      }
    }
  }
}

# Create list of all those files we just created so we can write them to a zip
# archive
# Start with the presence / absence files
pa_files <- list.files(path = "data/gbif/presence-absence", 
                       pattern = "*-pa.csv",
                       full.names = TRUE)
zipfile <- "data/gbif-pa.zip"
if (file.exists(zipfile)) {
  invisible(file.remove(zipfile))
}
zip(zipfile = zipfile,
    files = pa_files)

# Next archive the shapefiles (which include more than just .shp files)
shapefiles <- list.files(path = "data/gbif/shapefiles",
                         full.names = TRUE)
zipfile <- "data/gbif-shapefiles.zip"
if (file.exists(zipfile)) {
  invisible(file.remove(zipfile))
}
zip(zipfile = zipfile,
    files = shapefiles)

# Write gbif_pa to file
write.csv(x = gbif_pa,
          file = gbif_pa_file,
          row.names = FALSE)
