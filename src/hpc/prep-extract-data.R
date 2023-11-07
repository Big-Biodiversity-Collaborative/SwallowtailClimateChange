# Extract presence / absence and shapefile archives
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-10-20

# Clean out old pa data files
pa_files <- list.files(path = "data/gbif/presence-absence", 
                       full.names = TRUE, 
                       pattern = "*.csv")
if (!all(file.remove(pa_files))) {
  message("One or more presence absence files not removed")
}

# Extract data files
unzip(zipfile = "data/gbif-pa.zip", overwrite = TRUE)

# Clean out old shapefiles
shape_files <-list.files(path = "data/gbif/shapefiles", 
                         full.names = TRUE, 
                         pattern = "*-buffered-mcp*")
if (!all(file.remove(shape_files))) {
  message("One or more shapefiles not removed")
}

# Extract shape files
unzip(zipfile = "data/gbif-shapefiles.zip", overwrite = TRUE)
