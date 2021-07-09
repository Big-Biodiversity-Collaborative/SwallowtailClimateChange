# Create species richness maps of current & forecast insect species richness GLM
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-06

require(raster)
require(ggplot2)

source(file = "load_functions.R")

model <- "glm"
output_format <- "png" # "pdf"

# Indicates whether or not to write output ggplot maps to file
save_maps <- TRUE

insects_hosts <- read.csv(file = "data/insect-host.csv")

# Identify unique species of insects
insect_species <- unique(insects_hosts$insect)

# Grab the rasters for the current climate
current_rasters <- list()
forecast_rasters <- list()
for (species_name in insect_species) {
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species_name))
  current_file <- paste0("output/distributions/",
                         nice_name, 
                         "-distribution-",
                         model,
                         "-current.rds")
  forecast_file <- paste0("output/distributions/",
                         nice_name, 
                         "-distribution-",
                         model, 
                         "-GFDL-ESM4_RCP45.rds")
  current_rasters[[nice_name]] <- readRDS(current_file)
  forecast_rasters[[nice_name]] <- readRDS(forecast_file)
}

# Maps can be quite large, so limit to 
# long: -170 to -50
# lat: 10 to 70

# Make plot for current bioclim conditions
current_predictor <- "current"
current_map <- richness_map(r = current_rasters,
                            predictor = current_predictor)
# current_map$bio_plot
ggsave(filename = paste0("output/maps/richness-",
                         model, 
                         "-",
                         current_predictor,
                         ".",
                         output_format),
       plot = current_map$bio_plot)

# Make plot for forecast conditions
forecast_predictor <- "GFDL-ESM4_RCP45"
forecast_map <- richness_map(r = forecast_rasters,
                             predictor = forecast_predictor)
# forecast_map$bio_plot
ggsave(filename = paste0("output/maps/richness-",
                         model, 
                         "-",
                         forecast_predictor,
                         ".",
                         output_format),
       plot = forecast_map$bio_plot)

# Finally, subtract the current raster from the forecast raster, to generate a
# map of differences. We can use the rasters that come back from richness_map
delta_raster <- forecast_map$rasters - current_map$rasters

# Conversion for plotting with ggplot
delta_points <- raster::rasterToPoints(x = delta_raster, 
                                     spatial = TRUE)
# Then to a 'conventional' dataframe
delta_df  <- data.frame(delta_points)
rm(delta_points)

# Rename columns so they plot without extra ggplot commands
delta_df <- delta_df %>%
  dplyr::rename(Longitude = x,
                Latitude = y)

plot_title <- paste0("Change in species richness")
delta_plot <- ggplot(data = delta_df,
                   mapping = aes(x = Longitude,
                                 y = Latitude,
                                 fill = layer)) +
  geom_raster() +
  ggtitle(label = plot_title) +
  scale_fill_distiller(palette = "PRGn", # PiYG is an alternative
                       direction = 1) +  # So green is increase, purple decrease
  coord_equal() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        legend.title = element_blank()) +
  xlim(c(-170, -50)) +
  ylim(c(10, 70))

# delta_plot
ggsave(filename = paste0("output/maps/delta-richness-",
                         model, 
                         ".",
                         output_format),
       plot = delta_plot)


