# Extract summary statistics for aridity measures for each insect species
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-10-17

require(raster)  # handling environmental raster data
require(dplyr)   # summarizing values
require(ggplot2) # data visualization for quality assurance

# TODO: envirem data are 1980-1990. Might need to think about doing the manual 
# calculations of aridity for our time periods...

outfile <- "output/aridity/aridity-statistics.csv"

# If necessary, download environmental data and extract appropriate raster

# Aridity measures available from the envirem resource: 
# [https://envirem.github.io](https://envirem.github.io). Files are available 
# in the format:
# [region]_[time period]_[circulation model]_[resolution]_[file format].zip
#   + region = NAmerica
#   + time period = current
#   + circulation model = (none, since current data)
#   + resolution = 2.5arcmin
#   + file format = generic (i.e. a bil file)
# Files are archived at https://deepblue.lib.umich.edu/data/concern/generic_works/gt54kn05f
# So our file is NAmerica_current_2.5arcmin_generic.zip
# Which is at https://deepblue.lib.umich.edu/data/downloads/4b29b610g

# annualPET: annual potential evapotranspiration: a measure of the ability of 
# the atmosphere to remove water through evapotranspiration processes, given 
# unlimited moisture; measured in mm / year
aridity_measure <- "annualPET"

# The data should live in data/envirem, but check to see if they do not
aridity_file <- paste0("data/envirem/current_2-5arcmin_",
                       aridity_measure,
                       ".bil")
# If the file we want isn't on disk, download it
if (!file.exists(aridity_file)) {
  # Download the whole archive if necessary; it is possible that the archive is
  # on disc, but the measurement of aridity has not yet been extracted
  archive_file = "data/envirem/NAmerica_current_2.5arcmin_generic.zip"
  if (!file.exists(archive_file)) {
    download.file(url = "https://deepblue.lib.umich.edu/data/downloads/4b29b610g",
                  destfile = archive_file)
  }
  
  # Unzip said archive, which will extract to working directory; only pulling 
  # out aridity index of interest 
  file_start <- paste0("current_2-5arcmin_", aridity_measure)
  to_extract <- paste0(file_start, c(".bil", ".bil.aux.xml", ".hdr", ".prj"))
  unzip(zipfile = "data/envirem/NAmerica_current_2.5arcmin_generic.zip",
        files = to_extract)
  
  # Move files of interest to appropriate data folder
  new_files <- paste0("data/envirem/", to_extract)
  if(!all(file.rename(from = to_extract, to = new_files))) {
    warning("Problem encountered when trying to move environmental data file(s)")
  }
  # TODO: Do we want to remove the zip archive? (leaving it for now, though)
  # file.remove("data/envirem/NAmerica_current_2.5arcmin_generic.zip")
}

# Load aridity data into memory
aridity_data <- raster::raster(x = aridity_file)

# For each insect species, calculate mean, median, and standard deviation of 
# aridity, based on thinned & filtered observational data. 

# Data for identifying insect species
insects_hosts <- read.csv(file = "data/insect-host.csv")

# identify unique species of insects
insect_species <- unique(insects_hosts$insect)

# Store each species' extracted values as a list and do summarizing after the 
# iteration
arid_list <- list()

# Iterate over all insect species
for (species_name in insect_species) {
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species_name))
  
  # Load thinned & filtered observational data with lat/lon coordinates
  obs_file <- paste0("data/gbif/presence-absence/",
                     nice_name,
                     "-pa.csv")

  if (!file.exists(obs_file)) {
    warning(paste0("No observation data found for ", species_name))
  } else {
    message(paste0("Extracting ", aridity_measure, " for ", species_name))
    obs <- read.csv(file = obs_file)
    obs <- obs %>%
      dplyr::filter(pa == 1) %>%
      rename(longitude = x, latitude = y) %>%
      dplyr::select(longitude, latitude)
    # Extract aridity from cells with an observation; returns vector of values
    obs$aridity_values <- raster::extract(x = aridity_data, 
                                          y = obs)

    obs <- obs %>%
      mutate(species_name = species_name,
             aridity_measure = aridity_measure)
    
    # Update list with aridity values
    arid_list[[nice_name]] <- obs
    
    # Calculate a mean aridity score for each species
    # mean_aridity <- mean(sp_arid_raster, na.rm = TRUE)
    # median_aridity <- median(sp_arid_raster, na.rm = TRUE)
    # cat(species_name, ": ", mean_aridity, ", ", median_aridity, "\n", sep = "")
    # aridity_means$aridity_mean[i] <- mean_aridity
    # aridity_means$aridity_median[i] <- median_aridity
  }  
} # end iterating over all insect species

# Bind lists together in data frame
arid_df <- dplyr::bind_rows(arid_list)

# Calculate mean, median, standard deviation
arid_summary <- arid_df %>%
  dplyr::select(species_name, aridity_values, aridity_measure) %>%
  dplyr::group_by(species_name) %>%
  dplyr::summarize(mean_arid = mean(aridity_values, na.rm = TRUE),
            median_arid = median(aridity_values, na.rm = TRUE),
            sd_arid = sd(aridity_values, na.rm = TRUE),
            n_arid = n(),
            missing = sum(is.na(aridity_values))) %>%
  ungroup()

# Send that summary to the output file
write.csv(x = arid_summary,
          file = outfile,
          row.names = FALSE)

# Some graphical QA

# Mean longitude as measure of "west-ness"
# westness <- arid_df %>%
#   dplyr::group_by(species_name) %>%
#   dplyr::summarize(mean_lon = mean(longitude)) %>%
#   dplyr::arrange(mean_lon)

# Re-level species names based on "westness"
# arid_df$species_name <- factor(x = arid_df$species_name,
#                                levels = westness$species_name)

arid_boxplot <- ggplot(data = arid_df %>% filter(!is.na(aridity_values)), 
                       mapping = aes(x = species_name,
                                                     y = aridity_values)) +
  geom_boxplot() +
  geom_point(data = arid_summary, 
             mapping = aes(x = species_name, y = mean_arid),
             color = "#55DD99",
             size = 3) +
  ylab(label = paste0("Aridity (", aridity_measure, ")")) +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank())
arid_boxplot
ggsave(filename = paste0("output/plots/aridity-summary-", aridity_measure, ".pdf"),
       plot = arid_boxplot)
