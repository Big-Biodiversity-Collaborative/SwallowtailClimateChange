# Create species richness and hotspot figures
# Jeff Oliver, Erin Zylstra
# jcoliver@arizona.edu, ezylstra@arizona.edu
# 2025-01-07

require(dplyr)
require(terra)
require(ggplot2)
require(tidyterra)
require(sf) # should be imported by tidyterra, but just in case...
require(cowplot)
source(file = "functions/get_colors.R")

# Richness: Six panel figure showing
#   (a) contemporary species richness, 
#   (b) contemporary richness hotspots (>= 4 species), 
#   (c) forecast richness for SSP3-7.0, 2050s, 
#   (d) forecast richness hotspots for SSP3-7.0, 2050s, and 
#   (e) the difference in richness between (a) and (c)
#   (f) the difference in hotspots between (b) and (d)
# Supplemental Richness: Six panel figure of species richness estimates of all 
#   combinations of forecast SSP / time points
# Supplemental Hotspots: Six panel figure of richness hotspots for all 
#   combinations of forecast SSP / time points

################################################################################
# Data preparation
################################################################################

# Get climate model information, removing "ensemble_" from climate name
climate_models <- read.csv(file = "data/climate-models.csv") %>%
  mutate(name = gsub(pattern = "ensemble_", replacement = "", x = name))

# Climate model to use for forecast comparison
climate_model <- "ssp370_2041"

# Type of map 
# ov = insect distributed only where one or more host also occurs
# io = insect distribution depends only on climate)
type <- "ov"

# Base of output filenames
output_basename <- "output/manuscript/"

# Load richness rasters
rich_current_file <- paste0("output/richness/current-richness-", type, ".rds")
rich_current <- readRDS(rich_current_file)
rich_forecast_file <- paste0("output/richness/ensemble_", climate_model, 
                             "-richness-", type, ".rds")
rich_forecast <- readRDS(rich_forecast_file)

# Load delta raster
rich_delta_file <- paste0("output/richness/ensemble_", climate_model, 
                          "-delta-richness-", type, ".rds")
rich_delta <- readRDS(rich_delta_file)

# Create the hotspot rasters. We need to do this *before* re-projecting, 
# because terra::mosaic seems to have challenges if we try it after re-
# projection

# Create binary rasters (species richness "hotspots"), where 0 is suitable for 
# < 4spp. and 1 is suitable >= 4 species
hotspot_current <- terra::classify(x = rich_current,
                                   rcl = matrix(data = c(0, 3, 0,
                                                         3.1, Inf, 1),
                                                nrow = 2, byrow = TRUE))
hotspot_forecast <- terra::classify(x = rich_forecast,
                                    rcl = matrix(data = c(0, 3, 0,
                                                          3.1, Inf, 1),
                                                 nrow = 2, byrow = TRUE))
# Create a delta hotspot raster; need to 
# create a SpatRasterCollection from a list of rasters (two in this case, the 
# current hotspot and forecast hotspot rasters), changing the current raster to 
# negative values so we can use the sum function in terra::mosaic, i.e. 
# Forecast hotspot - Current hotspot.
# We also need to be sure we can tell the gains from losses, so we first 
# multiply the forecast hotspots raster so values are 0, 2, when we add the two
# rasters together, we get a raster with four values:
#   0 = not a hotspot in current or forecast
#   1 = hotspot only in current climate (= loss)
#   2 = hotspot only in forecast climate (= gain)
#   3 = hotspot in both current and forecast (= stable)
hotspot_delta_coll <- terra::sprc(list(hotspot_current, 2 * hotspot_forecast))
# Calculate delta in new single raster
hotspot_delta <- terra::mosaic(x = hotspot_delta_coll, fun = "sum")

# Grab spatial files with political boundaries
countries <- vect("data/political-boundaries/countries.shp")
states <- vect("data/political-boundaries/states.shp")  

# Reproject states and countries (can take a couple moments with first call to 
# project)
states <- project(states, "ESRI:102009")
countries <- project(countries, crs(states))

# For plotting purposes, we only need Canada, Mexico, and US because the LCC 
# projection is centered in the US, and country boundaries become very 
# distorted for Greenland and Central America
countries <- countries %>%
  filter(adm0_a3 %in% c("USA", "CAN", "MEX"))

# Reproject the richness rasters, too (takes a moment)
# Using nearest-neighbor method for cell values because we want to keep the 
# cardinal nature of raster cell values (i.e. integers)
rich_current <- project(rich_current, crs(states), method = "near")
rich_forecast <- project(rich_forecast, crs(states), method = "near")
rich_delta <- project(rich_delta, crs(states), method = "near")
hotspot_current <- project(hotspot_current, crs(states), method = "near")
hotspot_forecast <- project(hotspot_forecast, crs(states), method = "near")
hotspot_delta <- project(hotspot_delta, crs(states), method = "near")

# Get rid of NA values in richness raster and calculate extent for plot areas
rich_current <- tidyterra::drop_na(rich_current)
rich_ext <- ext(rich_current) * 1.01

# Set western boundary of extent in order to make sure Aleutians are included 
# in map
rich_ext <- ext(c(-5e6, rich_ext[2:4]))
xlim <- c(ext(rich_ext)[1], ext(rich_ext)[2])
ylim <- c(ext(rich_ext)[3], ext(rich_ext)[4])

# Crop the states & countries shapes to this expanded extent based on richness
states <- crop(states, rich_ext)
countries <- crop(countries, rich_ext)

################################################################################
# Creating individual plot objects
################################################################################

# Set a few plotting parameters
margins <- c(2, 0, 6, 0)
linewidth <- 0.1

# Get some colors 
rich_cols <- get_colors(palette = "richness")
rich_delta_cols <- get_colors(palette = "richdelta")
hotspot_cols <- get_colors(palette = "hotspot")
hotspot_delta_cols <- get_colors(palette = "distdelta")

# Setting colors for state & country lines
state_fill <- "white"
state_color <- "gray50" # or should it be "gray65"?
countries_color <- "black"

# Contemporary species richness plot
rich_current_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = state_fill) +
  geom_spatraster(data = rich_current) +
  scale_fill_gradientn(colors = rich_cols, na.value = NA, 
                       name = "Richness") +
  geom_spatvector(data = states, color = state_color, linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = countries,
                  color = countries_color, linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim,
           expand = FALSE) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        legend.spacing.y = unit(10, 'pt'))

# Forecast species richness plot
rich_forecast_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = state_fill) +
  geom_spatraster(data = rich_forecast) +
  scale_fill_gradientn(colors = rich_cols, na.value = NA, 
                       name = "Richness") +
  geom_spatvector(data = states, color = state_color, linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = countries,
                  color = state_color, linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim,
           expand = FALSE) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        legend.spacing.y = unit(10, 'pt'))

# Delta richness plot (difference between contemporary and forecast richness)
rich_delta_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = state_fill) +
  geom_spatraster(data = rich_delta) +
  scale_fill_gradient2(low = rich_delta_cols[1], mid = rich_delta_cols[2], 
                       high = rich_delta_cols[3],
                       na.value = NA, name = "Change", 
                       breaks = c(-5, 0, 5)) +
  geom_spatvector(data = states, color = state_color, linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = countries,
                  color = countries_color, linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim,
           expand = FALSE) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        legend.spacing.y = unit(10, 'pt'))

# Plot of contemporary richness hotspots
hotspot_current_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = state_fill) +
  geom_spatraster(data = hotspot_current) +
  scale_fill_gradientn(colors = hotspot_cols, na.value = NA) +
  geom_spatvector(data = states, color = state_color, linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = countries,
                  color = countries_color, linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim, 
           expand = FALSE) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        legend.position = "none")

# Plot of forecast richness hotspots
hotspot_forecast_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = state_fill) +
  geom_spatraster(data = hotspot_forecast) +
  scale_fill_gradientn(colors = hotspot_cols, na.value = NA) +
  geom_spatvector(data = states, color = state_color, linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = countries,
                  color = countries_color, linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim, 
           expand = FALSE) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        legend.position = "none")

# Delta hotspot plot (difference between contemporary and forecast hotspots)
hotspot_delta <- as.factor(hotspot_delta)
levels(hotspot_delta) <- data.frame(value = 0:3, 
                                    label = names(hotspot_delta_cols))
hotspot_delta <- drop_na(hotspot_delta)
#  Currently failing with:
# Warning messages:
#   1: Computation failed in `stat_terra_spat_raster()`.
# Caused by error:
#   ! [spatSample] at least one of 'values', 'cells', or 'xy' must be TRUE; or 'as.points' must be TRUE 
# 2: No shared levels found between `names(values)` of the manual scale and the data's fill values. 
# Look at figs-4-distributions.R to see how that one works

hotspot_delta_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = state_fill) +
  geom_spatraster(data = hotspot_delta, maxcell = Inf) +
  scale_fill_manual(name = "label", values = hotspot_delta_cols, na.translate = FALSE) +
  geom_spatvector(data = states, color = state_color, linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = countries,
                  color = countries_color, linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim,
           expand = FALSE) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        legend.position = "none")


################################################################################
# Assemble into single, six-panel figure
################################################################################
# Start by moving legends inside plot area for richness and delta maps
# Had to test lots of things to get these things to fit
# Note the legends will appear too small in an IDE (i.e. RStudio) pane, but 
# will be appropriately sized in output graphics file
legend_title_size <- 8
legend_text_size <- 6
legend_key_height <- 10
legend_key_width <- 12
legend_position <- c(0.12, 0.22)
legend_margin <- margin(c(0, 0, 0, 0))
rich_current_plot <- rich_current_plot +
  theme(legend.title = element_text(size = legend_title_size),
        legend.text = element_text(size = legend_text_size),
        legend.key.height = unit(legend_key_height, "pt"),
        legend.key.width = unit(legend_key_width, "pt"),
        legend.position = "inside",
        legend.position.inside = legend_position,
        legend.margin = legend_margin)
rich_forecast_plot <- rich_forecast_plot +
  theme(legend.title = element_text(size = legend_title_size),
        legend.text = element_text(size = legend_text_size),
        legend.key.height = unit(legend_key_height, "pt"),
        legend.key.width = unit(legend_key_width, "pt"),
        legend.position = "inside",
        legend.position.inside = legend_position,
        legend.margin = legend_margin)
rich_delta_plot <- rich_delta_plot +
  theme(legend.title = element_text(size = legend_title_size),
        legend.text = element_text(size = legend_text_size),
        legend.key.height = unit(legend_key_height, "pt"),
        legend.key.width = unit(legend_key_width, "pt"),
        legend.position = "inside",
        legend.position.inside = legend_position,
        legend.margin = legend_margin)

# Combine everything into a single, five-panel figure (this can take a moment)
six_panel <- cowplot::plot_grid(rich_current_plot, hotspot_current_plot,
                                 rich_forecast_plot, hotspot_forecast_plot,
                                 rich_delta_plot, hotspot_delta_plot,
                                 align = "h",
                                 ncol = 2,
                                 labels = "auto",
                                 vjust = 1,
                                 hjust = 0)
six_panel_file <- paste0(output_basename, "Figure-Richness.png")
ggsave(filename = six_panel_file,
       plot = six_panel,
       width = 6,
       height = 8,
       units = "in")

################################################################################
# Six-panel figure of species richness for each time (2) and climate (3) 
# scenario and six-panel figure of hotspots for each time (2) and climate (3) 
# scenario. Doing it once to keep the number of loops down (two instead of 
# four).
################################################################################

# Forecast scenarios and time points
scenarios <- c("ssp245", "ssp370", "ssp585")
times <- c("2041", "2071")

# Lists to hold plots
rich_plots <- vector(mode = "list", 
                     length = length(scenarios) * length(times))
hotspot_plots <- vector(mode = "list", 
                        length = length(scenarios) * length(times))

for (scenario_i in 1:length(scenarios)) {
  scenario <- scenarios[scenario_i]
  for (time_i in 1:length(times)) {
    # Enumerator to keep track of list elements
    list_i <- (scenario_i - 1) * length(times) + time_i
    time <- times[time_i]
    # Using "model" for the pasted scenario (e.g. ssp245) and timepoint (e.g. 
    # 2041)
    model <- paste0(scenario, "_", time)
    message("Plotting ", model, " richness & hotspots [", list_i, "]")
    
    richness_file <- paste0("output/richness/ensemble_", model, 
                            "-richness-", type, ".rds")
    richness_ras <- readRDS(file = richness_file)
    # Crop to extent of interest (here the same as contemporary richness plot)
    richness_ras <- crop(richness_ras, rich_ext)
    
    # Reproject in Lambert
    richness_ras <- project(richness_ras, crs(states), method = "near")
    
    # Get rid of NA values and calculate extent for plot area
    richness_ras <- drop_na(richness_ras)
    # Use that extent for limits
    xlim_rich <- c(ext(rich_ext)[1], ext(rich_ext)[2])
    ylim_rich <- c(ext(rich_ext)[3], ext(rich_ext)[4])
    
    # Now turn into binary map of hotspot (>= 4 species) or not 
    hotspot_ras <- terra::classify(x = richness_ras,
                                       rcl = matrix(data = c(-Inf, 3.1, 0,
                                                             3.1, Inf, 1),
                                                    nrow = 2, byrow = TRUE))
    
    # Get the scenario & time text for annotation
    model_info <- climate_models %>%
      filter(name == model)
    anno_text <- paste0(model_info$ssp_text, "\n", model_info$yr_text)
    # Turn this into a data frame for ggplot compatibility. May need to futz 
    # with x & y to get it to show up in right place...
    anno_df <- data.frame(x = -4110000,
                          y = -1700000,
                          label = anno_text)

    # Many elements are the same in both plots (but they differ in early layers 
    # of their respective ggplot elements) so we can make those objects once 
    # and re-use them to ensure consistency 
    
    # Same base plot of states for both plots
    state_base <- ggplot() +
      geom_spatvector(data = states, color = NA, fill = state_fill)
      
    # States & country overlays also same across maps
    state_overlay <- geom_spatvector(data = states, color = state_color, 
                                     linewidth = linewidth, fill = NA)
    country_overlay <- geom_spatvector(data = countries, color = countries_color, 
                                       linewidth = linewidth, fill = NA)

    # Same label and coordinate system in both plots
    label <- geom_label(data = anno_df, mapping = aes(x = x, y = y,
                                                      label = label),
                        size = 3, hjust = 0)
    coord <- coord_sf(datum = sf::st_crs("EPSG:4326"), expand = FALSE,
                      xlim = xlim, ylim = ylim)
    
    # Create the richness plot for this scenario / time combination
    rich_plot <- state_base + 
      geom_spatraster(data = richness_ras) +
      scale_fill_gradientn(colors = rich_cols, na.value = NA, 
                           name = "Richness") +
      state_overlay +
      country_overlay +
      label +
      coord +
      theme_bw() +
      theme(plot.margin = unit(margins, "pt"),
            axis.title = element_blank()) # the data for geom_label adds this
  
    # Since the panels all have the same color scale for richness, we only need 
    # a legend in the first panel AND this legend should be inside the plot 
    # area (northeast Pacific Ocean)
    if (list_i == 1) {
      # Panel #1, so we need a legend
      rich_plot <- rich_plot +
        theme(legend.title = element_text(size = 8),
              legend.text = element_text(size = 6),
              legend.key.height = unit(x = 10, units = "pt"),
              legend.key.width = unit(x = 12, units = "pt"),
              legend.position = "inside",
              legend.position.inside = c(0.2, 0.5), # second was 0.42
              legend.margin = margin(c(0, 0, 0, 0)),
              legend.spacing.y = unit(10, 'pt'))
    } else {
      # Not the first panel, so turn off legend
      rich_plot <- rich_plot + 
        theme(legend.position = "none")
    }
    
    # Create the hotspot plot for this scenario / time combination
    hotspot_plot <- state_base +
      geom_spatraster(data = hotspot_ras) +
      scale_fill_gradientn(colors = hotspot_cols, na.value = NA) +
      state_overlay +
      country_overlay +
      label +
      coord +
      theme_bw() +
      theme(plot.margin = unit(margins, "pt"),
            legend.position = "none",
            axis.title = element_blank()) # only necessary because of geom_label

    # Update our big plot lists
    rich_plots[[list_i]] <- rich_plot
    names(rich_plots)[list_i] <- model
    hotspot_plots[[list_i]] <- hotspot_plot
    names(hotspot_plots)[list_i] <- model
  }
}

# Make 3 x 2 panel plot of species richness
rich_6 <- plot_grid(plotlist = rich_plots,
                    align = "h",
                    ncol = 2,
                    labels = "auto",
                    vjust = 1, # justification so a, b, c... labels do 
                    hjust = 0) # not overlap with figure stuff
rich_6_file <- paste0(output_basename, "Supplemental-Figure-Richness.png")
ggsave(filename = rich_6_file,
       plot = rich_6,
       width = 6,
       height = 8,
       units = "in")


# Make 3 x 2 panel plot of hotspots
hotspot_6 <- plot_grid(plotlist = hotspot_plots,
                       align = "h",
                       ncol = 2,
                       labels = "auto",
                       vjust = 1,
                       hjust = 0)
hotspots_6_file <- paste0(output_basename, "Supplemental-Figure-Hotspots.png")
ggsave(filename = hotspots_6_file,
       plot = hotspot_6,
       width = 6,
       height = 8,
       units = "in")
