# GBIF data QA/QC
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-03

require(dplyr)
# Need to check each gbif file to make sure 
#     observation are restricted to CA, MX, US
#     are geographically bounded to north america (roughly)

# Logical indicating whether or not to update data files, removing any 
# observations that are out of bounds (i.e. roughly outside North America 
# longitude and latitude)
remove_oob <- TRUE

gbif_files <- list.files(path = "./data",
                         pattern = "*-gbif.csv",
                         full.names = TRUE)
lat_lim <- c(14, 80)
lon_lim <- c(-170, -52)

oob <- NULL
for (one_file in gbif_files) {
  data <- read.csv(file = one_file)
  # Identify any that are out of the rough lat/lon bounds of North America
  any_oob <- data %>%
    filter(longitude < lon_lim[1] | longitude > lon_lim[2] |
             latitude < lat_lim[1] | latitude > lat_lim[2])
  
  if (nrow(any_oob) > 0) {
    if (is.null(oob)) {
      oob <- any_oob
    } else {
      oob <- oob %>%
        bind_rows(any_oob)
    }
    
    # If appropriate update data files, removing those beast that are out of 
    # bounds
    if (remove_oob) {
      data <- data %>%
        filter(!(gbifID %in% any_oob$gbifID))
      write.csv(x = data,
                file = one_file)
    }
  }
}
