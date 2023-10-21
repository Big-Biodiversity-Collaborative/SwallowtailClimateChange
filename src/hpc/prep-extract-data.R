# Extract presence / absence and shapefile archives
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-10-20

# Extract data files
unzip(zipfile = "data/gbif-pa.zip", overwrite = TRUE)

# Extract shape files
unzip(zipfile = "data/gbif-shapefiles.zip", overwrite = TRUE)