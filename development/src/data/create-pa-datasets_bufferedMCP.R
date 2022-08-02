# Generate presence-absence dataset for each species, to be used in any SDM
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-06-13

require(terra)
require(sp)
require(sf)
require(dplyr)

replace <- TRUE
verbose <- TRUE

# Read in gbif-reconcile
species_list <- read.csv("development/data/gbif-reconcile-13spp.csv")

for (species in species_list$accepted_name) {

  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species))
  filename <- paste0("development/data/presence-absence/", nice_name, "-pa.csv") 

  # Only proceed if file doesn't exist or we want to replace existing files
  if (file.exists(filename) & replace == FALSE) next

    if (verbose) {
      message(paste0("\n****  Beginning process for ", species, "  ****"))
    }

  # Load observation data
  obs_file <- paste0("data/gbif/",
                     nice_name,
                     "-gbif.csv")
  if (!file.exists(obs_file)) {
    unzip(zipfile = "data/gbif.zip")
  }
  obs <- read.csv(file = obs_file)

  # Restrict observations to the most recent 23-year period (2000-2022) and
  # retain just the geographic coordinates
  presence <- obs %>%
    dplyr::filter(year %in% 2000:2022) %>%
    dplyr::select(longitude, latitude) %>%
    dplyr::rename(x = longitude,
                  y =latitude)
  
  # Remove duplicate locations just for calculating nearest neighbor distances
  presence1 <- distinct(presence)

  # Convert to a SpatialPoints object
  presence1_sp <- SpatialPoints(coords = presence1,
                                proj4string = CRS("+init=epsg:4326"))
  
  # Calculate the GreatCircle distance (in km) between points
  gc_dist <- sp::spDists(presence1_sp, longlat = TRUE) 

  # Calculate the maximum of nearest neighbor distances (in km)
  buffer <- gc_dist %>%
    apply(., 1, function(x) min(x[x > 0])) %>%
    max %>%
    round
  rm(gc_dist)
  
  # Create a minimum convex polygon (MCP) for observations
  ch <- chull(presence)
  ch_coords <- presence[c(ch, ch[1]), ]
  ch_polygon <- SpatialPolygons(list(Polygons(list(Polygon(ch_coords)), ID = 1)),
                                proj4string = CRS("+init=epsg:4326"))
  
  # Convert MCP to sf object and project to NA Albers Equal Area Conic 
  ch_poly_proj <- st_as_sf(ch_polygon) %>%
    st_transform(crs = "ESRI:102008")

  # Create a polygon = MCP + buffer
  ch_buffer <- st_buffer(ch_poly_proj,
                         dist = buffer * 1000)

  # Grab one of the climate tif files to use as a mask to generate 
  # pseudo-absence points
  tif_file <- list.files(path = "data/wc2-1", 
                         pattern = ".tif$", 
                         full.names = TRUE)[1]
  predictor <- terra::rast(tif_file)
  
  # Transform the buffered MCP back to lat/long 
  ch_buffer_latlong <- st_transform(ch_buffer, 4326)
  
  # Save buffered MCP as shapefile
  shapefile_name <- paste0("development/output/shapefiles/", 
                           nice_name, "-buffered-mcp.shp") 
  st_write(ch_buffer_latlong, shapefile_name, append = FALSE)

  # Convert the buffered MCP to a SpatVector
  ch_buffer_sv <- terra::vect(ch_buffer_latlong)
  
  # Crop and mask climate data to the buffered MCP polygon
  pred_mask <- predictor %>%
    terra::crop(ch_buffer_sv) %>%
    terra::mask(ch_buffer_sv)
  rm(predictor)
 
  # Generate pseudo-absence points and extract predictor values
  absence <- terra::spatSample(x = pred_mask,  
                               size = 5000,
                               method = "random",
                               na.rm = TRUE,
                               values = FALSE,
                               xy = TRUE)
  # Note: if buffered area is small, function may not be able to generate the 
  # indicated number of points (size) because the default is to select cells
  # without replacement. If this happens, R will return a warning:
  # [spatSample] fewer cells returned than requested

    # Check:
    # plot(ch_buffer_sv)
    # plot(ch_polygon, add = TRUE)
    # points(y ~ x, data = absence, cex = 0.5, col = "gray")
    # points(y ~ x, data = presence, cex = 0.5, col = "blue")

  # Make a vector of appropriate length with 0/1 values for 
  # (pseudo)absence/presence
  pa_data <- c(rep(x = 1, times = nrow(presence)), 
               rep(x = 0, times = nrow(absence)))  

  # Create a vector of folds for easier splitting into testing/training
  num_folds <- 5 # for 20/80 split
  fold <- c(rep(x = 1:num_folds, length.out = nrow(presence)),
            rep(x = 1:num_folds, length.out = nrow(absence)))
  
  # Combine our presence / absence data
  full_data <- data.frame(cbind(pa = pa_data,
                                fold = fold,
                                rbind(presence, absence)))
  
  # Save presence-absence data to a csv file
  write.csv(x = full_data,
            file = filename,
            row.names = FALSE)
  
  if (verbose) {
    message(paste0("\n****  ",nrow(presence), " gbif records and ",
                   nrow(absence), " pseudo-absence records written to ",
                   filename, "  ****"))
  }
}

# Will likely want to put all the csv files into data/pa-datasets.zip
