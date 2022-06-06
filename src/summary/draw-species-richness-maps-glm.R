# Create species richness maps of current & forecast insect species richness GLM
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-06

require(raster)
require(ggplot2)
require(dplyr)

source(file = "load_functions.R")

method <- "glm"
output_format <- "png" # "pdf"
# The name of the current (contemporary) global climate model for comparisons 
# with forecast models
current_climate_name <- "current"

insects_hosts <- read.csv(file = "data/insect-host.csv")

# Identify unique species of insects
insect_species <- unique(insects_hosts$insect)
nice_names <- tolower(x = gsub(pattern = " ",
                               replacement = "_",
                               x = insect_species))

climate_models <- read.csv(file = "data/climate-models.csv")

# We do not need individual species' rasters for each climate model, just a 
# single stack rasters for each climate model, so climate models are the 
# "outer" iteration and species are the "inner" iteration
raster_stacks <- list()
for (climate_model in climate_models$name) {
  distribution_rasters <- list()
  for (nice_name in nice_names) {
    raster_file <- paste0("output/distributions/",
                          nice_name,
                          "-distribution-",
                          method, "-",
                          climate_model, ".rds")
    # Read the species' raster into the list for this climate model
    distribution_rasters[[nice_name]] <- readRDS(file = raster_file)
  }
  # Stack all individual species' rasters for this climate model into one
  # RasterStack
  raster_stacks[[climate_model]] <- stack_rasters(r = distribution_rasters,
                                                  out = "total")
}

# For now, we'll assume 0 as minimum, but need to find maximum from rasters
maximums <- sapply(X = raster_stacks, 
                   FUN = raster::cellStats,
                   stat = "max")
palette_limits <- c(0, max(maximums))
rm(maximums)

# Now that we have the scale for the palette, iterate over the climate models
# (yes, again) and make a plot for each of the climate models
for (climate_model in climate_models$name) {
  prediction_map <- richness_map(r = raster_stacks[[climate_model]],
                                 predictor = climate_model,
                                 pal_limits = palette_limits)
  ggsave(filename = paste0("output/maps/richness-",
                           method, 
                           "-",
                           climate_model,
                           ".",
                           output_format),
         plot = prediction_map)
  
  # iff this is not the raster stack for the current (contemporary) climate 
  # model, we can also calculate a delta map
  if (climate_model != current_climate_name) {
    # Finally, subtract the current raster from the forecast raster, to generate a
    # map of differences. We can use the rasters that come back from richness_map
    delta_raster <- raster_stacks[[climate_model]] - raster_stacks[[current_climate_name]]
    
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
      labs(title = "Change in species richness",
           subtitle = paste0(method, ", current vs. ", climate_model)) +
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
                             method, 
                             ".",
                             output_format),
           plot = delta_plot)
  }
}
