# Generate presence-absence dataset for each species, to be used in any SDM
# Erin Zylstra
# ezylstra@arizona.edu
# 2022-06-13

# Testing the buffered MCP approach for P. rumiko or some other species 
# (instead of looping through all species in the gbif-reconcile file)

require(terra)
require(sp)
require(sf)
require(dplyr)

replace <- FALSE
verbose <- TRUE

# Read in gbif-reconcile
species_list <- read.csv("development/data/gbif-reconcile-13spp.csv")

#for (species in species_list$accepted_name) {

  species <- "Papilio rumiko"
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species))
  filename <- paste0("development/data/presence-absence/", nice_name, "-pa.csv") 

  # Only proceed if file doesn't exist or we want to replace existing files
  # if (file.exists(filename) & replace == FALSE) next
  #   
  #   if (verbose) {
  #     message(paste0("\n****  Beginning process for ", species, "  ****"))
  #   }    

  # Load observation data
  obs_file <- paste0("data/gbif/",
                     nice_name,
                     "-gbif.csv")
  if (!file.exists(obs_file)) {
    unzip(zipfile = "data/gbif.zip")
  }
  obs <- read.csv(file = obs_file)
  
  # Outside the development folder, we'll be using observations that have
  # already been filtered by date and location (where climate data are avail)
  # and a ~95% density envelope. To avoid including outliers when testing this 
  # MCP approach, I'll manually restrict observations to certain lat/longs
    # For testing P. rumiko, filter out observation with latitude >= 35 deg
    # For testing P. cresphontes, filter lat >= 48 deg, long > -121
    # For testing A. texana, filter >32, long outside -100.5 to -96.5
    # For testing C. greggii, filter lat < 20
    # For testing P. trifoliata, filter lat > 48, long < -115
    # For testing R. graveolens, filter long < -130
    # For testing Z. americanum, filter long < -100
    # For testing Z. clava-herculis, filter long < -104, lat >39
    # For testing Z. fagara, filter lat > 35, long < -115
  
    plot(latitude~longitude,obs,pch=19,cex=0.6)
    abline(h=35,col='blue')
    abline(v=-115, col = "blue")

  obs <- obs %>%
    # dplyr::filter(longitude > (-115)) %>%
    dplyr::filter(latitude < 35)

  # Restrict observations to the most recent 23-year period (2000-2022) and
  # retain just the geographic coordinates
  presence <- obs %>%
    dplyr::filter(year %in% 2000:2022) %>%
    dplyr::select(longitude, latitude)

  # Convert to a SpatialPoints object
  obs_spatial <- SpatialPoints(coords = presence,
                               proj4string = CRS("+init=epsg:4326"))
  
  # Calculate the GreatCircle distance (in km) between points
  gc_dist <- sp::spDists(obs_spatial, longlat = TRUE) 

  # Calculate the maximum of nearest neighbor distances (in km)
  buffer <- gc_dist %>%
    apply(., 1, function(x) min(x[x>0])) %>%
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
    # Check projection:
    # st_crs(ch_poly_proj)
  
  # Create a polygon = MCP + buffer
  ch_buffer <- st_buffer(ch_poly_proj,
                         dist = buffer * 1000)

  # Grab worldclim data to use as predictors
  predictors <- terra::rast(list.files(path = "data/wc2-1",
                                       pattern = ".tif$",
                                       full.names = TRUE))  
  
  # Transform the buffered MCP back to lat/long 
  ch_buffer_latlong <- st_transform(ch_buffer, 4326)
  
  # Convert the buffered MCP to a SpatVector
  ch_buffer_sv <- terra::vect(ch_buffer_latlong)
  
  # Crop and mask predictors to the buffered MCP polygon
  pred_mask <- predictors %>%
    terra::crop(ch_buffer_sv) %>%
    terra::mask(ch_buffer_sv)
  rm(predictors)
  
  # Extract predictor values for observed points
  presence <- terra::extract(x = pred_mask, 
                             y = presence, 
                             xy = TRUE)
  
  # Rearrange columns
  presence <- presence %>%
    dplyr::select(-ID) %>%
    dplyr::relocate(c(x,y), .before = bio1)  

  # Generate pseudo-absence points and extract predictor values
  absence <- terra::spatSample(x = pred_mask,  
                               size = 5000,
                               method = "random",
                               na.rm = TRUE,
                               values = TRUE,
                               xy = TRUE)
  # Note: if buffered area is small, function may not be able to generate the 
  # indicated number of points (size) because the default is to select cells
  # without replacement. If this happens, R will return a warning:
  # [spatSample] fewer cells returned than requested

    # Check:
    plot(ch_buffer_sv)
    plot(ch_polygon, add = TRUE)
    points(y ~ x, data = absence, cex = 0.5, col = "gray")
    points(y ~ x, data = presence, cex = 0.5, col = "blue")

  # Make a vector of appropriate length with 0/1 values for 
  # (pseudo)absence/presence
  pa_data <- c(rep(x = 1, times = nrow(presence)), 
               rep(x = 0, times = nrow(absence)))  

  # Create a vector of folds for easier splitting into testing/training
  num_folds <- 5 # for 20/80 split
  fold <- c(rep(x = 1:num_folds, length.out = nrow(presence)),
            rep(x = 1:num_folds, length.out = nrow(absence)))
  
  # Combine our presence / absence and fold vectors with environmental data
  full_data <- data.frame(cbind(pa = pa_data,
                                fold = fold,
                                rbind(presence, absence)))
  # write.csv(x = full_data,
  #           file = filename,
  #           row.names = FALSE)
  # 
  # if (verbose) {
  #   message(paste0("\n****  ",nrow(presence), " gbif records and ", 
  #                  nrow(absence), " pseudo-absence records written to ",
  #                  filename, "  ****"))
  # }
#}

# Will likely want to put all the csv files into data/pa-datasets.zip
