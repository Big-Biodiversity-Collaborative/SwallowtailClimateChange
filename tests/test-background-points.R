# Checking that when background points are generated 
# (in src/data/gbif-3-presence-absence.R), they will only be located in 
# terrestrial areas (avoiding coastal or inland water bodies)

# Erin Zylstra
# ezylstra@arizona.edu
# 2023-01-18

require(terra)
require(sp)
require(sf)
require(dplyr)

# Grab one of the climate tif files to use as a mask to generate 
# pseudo-absence points
tif_file <- list.files(path = "data/wc2-1", 
                       pattern = ".tif$", 
                       full.names = TRUE)[1]
predictor <- terra::rast(tif_file)

# First, view the climate raster to check that masking around the Great Lakes
# is as expected.  In particular, we want the raster to include all cells that
# overlap any land area and we want the raster to exclude those cells (have NA 
# values in those cells) that fall entirely within a water body.
lakes <- vect("data/lakes/ne_10m_lakes.shp")
plot(predictor, xlim = c(-89, -84), ylim = c(43, 47), 
     maxcell = ncell(predictor))
lines(lakes)

# Number of background points to generate
n_background <- 10000

# Now, test how background points are generated for two species that have 
# occurrence records near inland water bodies
plant <- "Ptelea trifoliata"
insect <- "Papilio cresphontes"
species <- c(plant, insect)

for (spp in species) {

  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = spp))

  # Load observation data
  obs_file <- paste0("data/gbif/filtered/",
                     nice_name,
                     "-gbif.csv")
  if (!file.exists(obs_file)) {
    unzip(zipfile = "data/gbif-filtered.zip")
  }
  obs <- read.csv(file = obs_file)
  
  # Retain just the geographic coordinates
  presence <- obs %>%
    dplyr::select(longitude, latitude) %>%
    dplyr::rename(x = longitude,
                  y =latitude)
  
  # Convert to a SpatialPoints object using WGS84 CRS
  presence_sp <- SpatialPoints(coords = presence,
                               proj4string = CRS("+init=epsg:4326"))
  
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
  gc(verbose = FALSE)
  
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
  
  # Transform the buffered MCP back to lat/long 
  ch_buffer_latlong <- st_transform(ch_buffer, 4326)
  
  # Convert the buffered MCP to a SpatVector
  ch_buffer_sv <- terra::vect(ch_buffer_latlong)
  
  # Crop and mask climate data to the buffered MCP polygon
  pred_mask <- predictor %>%
    terra::crop(ch_buffer_sv) %>%
    terra::mask(ch_buffer_sv)
  
  # Generate pseudo-absence points
  absence <- suppressWarnings(terra::spatSample(x = pred_mask,  
                                                size = n_background,
                                                method = "random",
                                                na.rm = TRUE,
                                                values = TRUE,
                                                xy = TRUE))
  # Note: if buffered area is small, function may not be able to generate 
  # the indicated number of points (size) because the default is to select 
  # cells without replacement. If this happens, R will return a warning:
  # [spatSample] fewer cells returned than requested, so wrapped everything
  # in suppressWarnings()
  
  # Check that each background point has climate data (falls in a non-NA cell)
  if (sum(is.na(absence$mean)) == 0) {
    message(paste0("All background points for ", spp, " have climate data. "))
  } else {
    message(paste0("There is a problem with how background points are generated.",
                   "At least one background point for ", spp, 
                   " does not have climate data."))    
  }
  
  # Plot things at a large scale:
    title <- paste0("All background (gray) and presence (blue) points for ", 
                    spp)
    # Buffered MCP
    plot(ch_buffer_sv, lty = 2, las = 1, main = title) 
    # MCP around occurrence records
    plot(ch_polygon, add = TRUE)
    # Background points
    points(y ~ x, data = absence, cex = 0.5, col = "gray")
    # Occurrence records
    points(y ~ x, data = presence, cex = 0.5, col = "blue")
    lines(lakes)
  
  # Plot things in the Great Lakes area
    title <- paste0("Climate data with background (black) and presence (blue) points for ", 
                    spp)
    plot(predictor, xlim = c(-89, -84), ylim = c(43, 47), las = 1,
         maxcell = ncell(predictor), main = title)
    # Background points
    points(y ~ x, data = absence, cex = 0.8, pch = 19, col = "black")
    # Occurrence records
    points(y ~ x, data = presence, cex = 0.8, pch = 19, col = "blue")
    lines(lakes)

  # Another plot in the Great Lakes area
    plot(predictor, xlim = c(-84, -79), ylim = c(41, 45), las = 1,
         maxcell = ncell(predictor), main = title)
    # Background points
    points(y ~ x, data = absence, cex = 0.8, pch = 19, col = "black")
    # Occurrence records
    points(y ~ x, data = presence, cex = 0.8, pch = 19, col = "blue")
    lines(lakes)
    # Note that points can lie just inside one of the Great Lakes (according to
    # the lake outline from the shapefile). However, these points are in raster 
    # cells with climate data because part of the cell overlaps land. I think 
    # its ok since the points are effectively representing areas on the edge of 
    # water.  
  
}
