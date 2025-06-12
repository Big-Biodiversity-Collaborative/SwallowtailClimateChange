# Look (visually) at observations through time
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-06-11

library(ggplot2)
library(gganimate)
library(terra)
library(rnaturalearth)
library(gifski) # to render animations without the dumb file permissions issue
# https://stackoverflow.com/questions/60259080/fail-to-render-an-animation

# Want to see observations through time. Testing with filtered data, but would 
# want to use all data (or rather, the data that does not impose a 1 obs/cell)

observations <- read.csv(file = "data/gbif/filtered/papilio_cresphontes-gbif.csv")
# observations <- read.csv(file = "data/gbif/downloaded/papilio_cresphontes-gbif.csv")

# Get limits for plot (add 4% to each direction)
min_lon <- min(observations$longitude)
max_lon <- max(observations$longitude)
min_lat <- min(observations$latitude)
max_lat <- max(observations$latitude)

plot_ext <- terra::ext(c(min_lon, max_lon, min_lat, max_lat)) * 1.04
xlim = c(plot_ext[1], plot_ext[2])
ylim = c(plot_ext[3], plot_ext[4])

# Get North American political boundaries map  
boundaries <- rnaturalearth::ne_countries(continent = "north america",
                                          scale = "medium",
                                          returnclass = "sf") %>% 
  dplyr::select(1) 


# Create ggplot object, adding political boundaries first, then adding points
observations_plot <- ggplot() +
  geom_spatvector(data = boundaries, color = "black", fill = NA) +
  geom_point(data = observations, 
             mapping = aes(x = longitude,
                           y = latitude,
                           group = seq_along(year)), # This makes points appear/disappear as desired
             size = 0.75,
             alpha = 0.5) +
  coord_sf(xlim = xlim, ylim = ylim) +
  transition_states(year,
                    transition_length = 1,
                    state_length = 1) +
  labs(title = "Year: {closest_state}") +
  enter_fade() + # these don't do much
  exit_fade() +
  theme_bw() +
  theme(axis.title = element_blank())

animate(observations_plot, renderer = gifski_renderer())

