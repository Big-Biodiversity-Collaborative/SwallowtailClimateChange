# Create species richness and hotspot figures
# Jeff Oliver, Erin Zylstra
# jcoliver@arizona.edu, ezylstra@arizona.edu
# 2025-01-07

library(dplyr)
library(ggplot2)
library(terra)
library(tidyterra)
source(file = "functions/get_colors.R")

# Richness: Five panel figure showing
#   (a) contemporary species richness, 
#   (b) contemporary richness hotspots (>= 4 species), 
#   (c) forecast richness for SSP3-7.0, 2050s, 
#   (d) forecast richness hotspots for SSP3-7.0, 2050s, and 
#   (e) the difference in richness between (a) and (c)
# Supplemental Richness: Six panel figure of species richness estimates of all 
#   combinations of forecast SSP / time points
# Supplemental Hotspots: Six panel figure of richness hotspots for all 
#   combinations of forecast SSP / time points




# Need to:
# Use get_colors() where appropriate
# Use climate & time names for figures from climate_models.csv
# Focus only on Lambert Conformal Conic projection
# Rename things as would be appropriate for this standalone script

# Get climate model information
climate_models <- read.csv(file = "data/climate-models.csv") %>%
  mutate(name = gsub(pattern = "ensemble_", replacement = "", x = name))

# Climate model to use for forecast comparison
climate_model <- "ssp370_2041"

# Type of map 
# ov = insect distributed only where one or more host also occurs
# io = insect distribution depends only on climate)
type <- "ov"

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

# Set a few plotting parameters
margins <- c(2, 0, 6, 0)
linewidth <- 0.1

# Get some colors 
rich_cols <- get_colors(palette = "richness")
delta_cols <- get_colors(palette = "richdelta")

# Grab spatial files with political boundaries
countries <- vect("data/political-boundaries/countries.shp")
states <- vect("data/political-boundaries/states.shp")  

# Need to crop the eastern edge of states and countries layers for Lambert 
# Conformal Conic North America projection ("LCC" here)
state_ext <- ext(states)
state_ext[2] <- -45

# Crop to desired extent  
states <- crop(states, state_ext)
countries <- crop(countries, state_ext)

# Reproject states and countires
states <- project(states, "ESRI:102009")
countries <- project(countries, crs(states))

# Reproject the richness rasters, too (takes a moment)
rich_current <- project(rich_current, crs(states))
rich_forecast <- project(rich_forecast, crs(states))
rich_delta <- project(rich_delta, crs(states))

# Get rid of NA values in richness raster and calculate extent for plot area
rich_current <- tidyterra::drop_na(rich_current)
rich_ext <- ext(rich_current) * 1.01
xlim <- c(ext(rich_ext)[1], ext(rich_ext)[2])
ylim <- c(ext(rich_ext)[3], ext(rich_ext)[4])

# Note that when plotting, we're removing boundaries for all countries 
# except the US, Canada, and Mexico because with this projection that is 
# centered in the US, country boundaries become very distorted for Greenland 
# and Central America
rich_current_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = "white") +
  geom_spatraster(data = rich_current) +
  scale_fill_gradientn(colors = rich_cols, na.value = NA, 
                       name = "Richness") +
  geom_spatvector(data = states, color = "gray50", linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = filter(countries, 
                                countries$adm0_a3 %in% c("USA", "CAN", "MEX")),
                  color = "black", linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim,
           expand = FALSE) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        legend.spacing.y = unit(10, 'pt'))




################################################################################
# Copy/paste of relevant sections of create-manuscript-objects.R (lines 706 
# onward)
# Figures: richness maps ------------------------------------------------------#
################################################################################

# Future scenario
scen <- "ssp370_2041"

# Type of map 
# ov = insect distributed only where one or more host also occurs
# io = insect distribution depends only on climate)
type <- "ov"

# Load richness rasters
rich_c_name <- paste0("output/richness/current-richness-", type, ".rds")
rich_c <- readRDS(rich_c_name)
rich_f_name <- paste0("output/richness/ensemble_", scen, "-richness-",
                      type, ".rds")
rich_f <- readRDS(rich_f_name)

# Load delta raster
delta_name <- paste0("output/richness/ensemble_", scen, "-delta-richness-",
                     type, ".rds")
delta <- readRDS(delta_name)

# Plot extent
xlimr <- c(ext(delta)[1], ext(delta)[2])
ylimr <- c(ext(delta)[3], ext(delta)[4])

# Few plotting parameters
margins <- c(2, 0, 6, 0)
linewidth <- 0.1

# Colors
# Light gray
graycol <- "#f2f2f2"
# For richness, eBird abundance colors but add a light gray for 0 (multiple
# steps to replicate ebirdst::ebirdst_palettes behavior)
rich_cols <- rev(hcl.colors(n = 7, palette = "plasma"))
# Drop the first value in the vector, which is ugly yellow
rich_cols <- rich_cols[-1]
# Create color function between our zero (light gray) and first yellow to 
# span gray to first color; otherwise contrast between 0 and 1 is too high
gry_ramp <- colorRampPalette(c(graycol, rich_cols[1]))
# Create final vector with gray (0), bridge to our palette (1), and remainder
# of palette (2-7)
rich_cols <- c(graycol, gry_ramp(6)[3], rich_cols)
# "#f2f2f2" "#EFDE91" "#ECC000" "#E8853A" "#D24E71" "#AB1488" "#72008D" "#001889"
# Compare with vector we would see with ebirdst::ebirdst_palettes
# rich_cols <- c(graycol, ebirdst::ebirdst_palettes(7, "weekly"))
# "#f2f2f2" "#EDDEA5" "#FCCE25" "#F58A47" "#D5536F" "#A51F99" "#6300A7" "#0D0887"

# For deltas, red-blue spectrum
delta_cols <- c("#D10000",
                graycol,
                "#104e8b")

# Create map objects
richness_c <- ggplot() +
  geom_spatvector(data = countries, color = NA, fill = "white") +
  geom_spatraster(data = rich_c) +
  scale_fill_gradientn(colors = rich_cols, na.value = NA,
                       name = "Richness") +
  geom_spatvector(data = states, color = "gray65", linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = countries, color = "black", linewidth = linewidth, 
                  fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr, ylim = ylimr) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        legend.spacing.y = unit(10, 'pt'))
richness_f <- ggplot() +
  geom_spatvector(data = countries, color = NA, fill = "white") +
  geom_spatraster(data = rich_f, maxcell = Inf) +
  scale_fill_gradientn(colors = rich_cols, na.value = NA, 
                       name = "Richness") +
  geom_spatvector(data = states, color = "gray65", linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = countries, color = "black", linewidth = linewidth, 
                  fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr, ylim = ylimr) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        legend.spacing.y = unit(10, 'pt'))
richness_d <- ggplot() +
  geom_spatvector(data = countries, color = NA, fill = "white") +
  geom_spatraster(data = delta) +
  scale_fill_gradient2(low = delta_cols[1], mid = delta_cols[2], 
                       high = delta_cols[3], na.value = NA, name = "Change", 
                       breaks = c(-5, 0, 5)) +
  geom_spatvector(data = states, color = "gray65", linewidth = linewidth, 
                  fill = NA) +
  geom_spatvector(data = countries, color = "black", linewidth = linewidth, 
                  fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr, ylim = ylimr) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        legend.spacing.y = unit(10, 'pt'))

# Combine everything
r <- plot_grid(richness_c, richness_f, richness_d,
               align = "h",
               ncol = 1,
               labels = "auto",
               vjust = 1,
               hjust = 0)
richness_3panel <- paste0(output_basename, "richness_3panel.png")
if (!file.exists(richness_3panel) | (file.exists(richness_3panel) & replace)) {
  ggsave(filename = richness_3panel,
         plot = r,
         width = 4.5,
         height = 8,
         units = "in")
}

# Same figures, except with a different projection
# Crop the eastern edge of states and countries layers
state_ext <- ext(states)
state_ext[2] <- -45
states <- crop(states, state_ext)
countries <- crop(countries, state_ext)

# Project layers to Lambert Conformal Conic North America
states_lcc <- project(states, "ESRI:102009")
countries_lcc <- project(countries, crs(states_lcc))
rich_c_lcc <- project(rich_c, crs(states_lcc))
rich_f_lcc <- project(rich_f, crs(states_lcc))
rich_d_lcc <- project(delta, crs(states_lcc))

# Get rid of NA values and calculate extent for plot area
rich_c_lcc <- drop_na(rich_c_lcc)
limsr_lcc <- ext(rich_c_lcc) * 1.01
xlimr_lcc <- c(ext(limsr_lcc)[1], ext(limsr_lcc)[2])
ylimr_lcc <- c(ext(limsr_lcc)[3], ext(limsr_lcc)[4])

# Note that when plotting, we're removing boundaries for all countries 
# except the US, Canada, and Mexico because with this projection that is 
# centered in the US, country boundaries become very distorted for Greenland 
# and Central America
richness_c_lcc <- ggplot() +
  geom_spatvector(data = states_lcc, color = NA, fill = "white") +
  geom_spatraster(data = rich_c_lcc) +
  scale_fill_gradientn(colors = rich_cols, na.value = NA, 
                       name = "Richness") +
  geom_spatvector(data = states_lcc, color = "gray50", linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = filter(countries_lcc, 
                                countries_lcc$adm0_a3 %in% c("USA", "CAN", "MEX")),
                  color = "black", linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr_lcc, ylim = ylimr_lcc,
           expand = FALSE) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        legend.spacing.y = unit(10, 'pt'))
richness_f_lcc <- ggplot() +
  geom_spatvector(data = states_lcc, color = NA, fill = "white") +
  geom_spatraster(data = rich_f_lcc) +
  scale_fill_gradientn(colors = rich_cols, na.value = NA, 
                       name = "Richness") +
  geom_spatvector(data = states_lcc, color = "gray50", linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = filter(countries_lcc, 
                                countries_lcc$adm0_a3 %in% c("USA", "CAN", "MEX")),
                  color = "black", linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr_lcc, ylim = ylimr_lcc,
           expand = FALSE) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        legend.spacing.y = unit(10, 'pt'))
richness_d_lcc <- ggplot() +
  geom_spatvector(data = countries_lcc, color = NA, fill = "white") +
  geom_spatraster(data = rich_d_lcc) +
  scale_fill_gradient2(low = delta_cols[1], mid = delta_cols[2], 
                       high = delta_cols[3],
                       na.value = NA, name = "Change", 
                       breaks = c(-5, 0, 5)) +
  geom_spatvector(data = states_lcc, color = "gray65", linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = filter(countries_lcc, 
                                countries_lcc$adm0_a3 %in% c("USA", "CAN", "MEX")),
                  color = "black", linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr_lcc, ylim = ylimr_lcc,
           expand = FALSE) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        legend.spacing.y = unit(10, 'pt'))

# Combine everything
r_lcc <- plot_grid(richness_c_lcc, richness_f_lcc, richness_d_lcc,
                   align = "vh",
                   ncol = 1,
                   labels = "auto",
                   vjust = 1,
                   hjust = 0)
richness_lcc_3panel <- paste0(output_basename, "richness_lcc_3panel.png")
if (!file.exists(richness_lcc_3panel) | (file.exists(richness_lcc_3panel) & replace)) {
  ggsave(filename = richness_lcc_3panel,
         plot = r_lcc,
         width = 4,
         height = 8,
         units = "in")
}

# Create binary richness maps, where 0 is suitable for < 4spp. and 1 is 
# suitable >= 4 species (richness "hotspot")
# We have richness rasters (rich_c and rich_c_lcc), need to convert to binary
rich_binary_c <- terra::classify(x = rich_c,
                                 rcl = matrix(data = c(0, 3, 0,
                                                       3.1, Inf, 1),
                                              nrow = 2, byrow = TRUE))
rich_binary_f <- terra::classify(x = rich_f,
                                 rcl = matrix(data = c(0, 3, 0,
                                                       3.1, Inf, 1),
                                              nrow = 2, byrow = TRUE))

# Now make the two binary plot objects
richness_binary_c <- ggplot() +
  geom_spatvector(data = countries, color = NA, fill = "white") +
  geom_spatraster(data = rich_binary_c) +
  scale_fill_gradientn(colors = rich_cols[c(1, 5)], na.value = NA,
                       name = "Richness") +
  geom_spatvector(data = states, color = "gray65", linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = countries, color = "black", linewidth = linewidth, 
                  fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr, ylim = ylimr) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        legend.position = "none")

richness_binary_f <- ggplot() +
  geom_spatvector(data = countries, color = NA, fill = "white") +
  geom_spatraster(data = rich_binary_f) +
  scale_fill_gradientn(colors = rich_cols[c(1, 5)], na.value = NA,
                       name = "Richness") +
  geom_spatvector(data = states, color = "gray65", linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = countries, color = "black", linewidth = linewidth, 
                  fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr, ylim = ylimr) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        legend.position = "none")

# Add those binary maps to our multi-panel image, but first need to move the 
# legend inside the plot for those richness and delta map
# So much fiddly bits!
legend_title_size <- 10
legend_text_size <- 8
legend_key_height <- 10
legend_key_width <- 12
legend_position <- c(0.15, 0.3)
richness_c_5 <- richness_c +
  theme(legend.title = element_text(size = legend_title_size),
        legend.text = element_text(size = legend_text_size),
        legend.key.height = unit(legend_key_height, "pt"),
        legend.key.width = unit(legend_key_width, "pt"),
        legend.position = "inside",
        legend.position.inside = legend_position)
richness_f_5 <- richness_f +
  theme(legend.title = element_text(size = legend_title_size),
        legend.text = element_text(size = legend_text_size),
        legend.key.height = unit(legend_key_height, "pt"),
        legend.key.width = unit(legend_key_width, "pt"),
        legend.position = "inside",
        legend.position.inside = legend_position)
richness_d_5 <- richness_d +
  theme(legend.title = element_text(size = legend_title_size),
        legend.text = element_text(size = legend_text_size),
        legend.key.height = unit(legend_key_height, "pt"),
        legend.key.width = unit(legend_key_width, "pt"),
        legend.position = "inside",
        legend.position.inside = legend_position)

# Combine everything
r_5 <- plot_grid(richness_c_5, richness_binary_c,
                 richness_f_5, richness_binary_f,
                 richness_d_5,
                 align = "h",
                 ncol = 2,
                 labels = "auto",
                 vjust = 1,
                 hjust = 0)
richness_5panel <- paste0(output_basename, "richness_5panel.png")
if (!file.exists(richness_5panel) | (file.exists(richness_5panel) & replace)) {
  ggsave(filename = richness_5panel,
         plot = r_5,
         width = 9,
         height = 8,
         units = "in")
}

# And do the same 5-panel map for using the Lambert projection
rich_binary_c_lcc <- project(rich_binary_c, crs(states_lcc))
rich_binary_c_lcc <- drop_na(rich_binary_c_lcc)
rich_binary_f_lcc <- project(rich_binary_f, crs(states_lcc))
rich_binary_f_lcc <- drop_na(rich_binary_f_lcc)
limsr_binary_lcc <- ext(rich_binary_c_lcc) * 1.01
xlimr_binary_lcc <- c(ext(limsr_binary_lcc)[1], ext(limsr_binary_lcc)[2])
ylimr_binary_lcc <- c(ext(limsr_binary_lcc)[3], ext(limsr_binary_lcc)[4])

# Now create two binary plots
richness_binary_c_lcc <- ggplot() +
  geom_spatvector(data = states_lcc, color = NA, fill = "white") +
  geom_spatraster(data = rich_binary_c_lcc) +
  scale_fill_gradientn(colors = rich_cols[c(1, 5)], na.value = NA) +
  geom_spatvector(data = states_lcc, color = "gray50", linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = filter(countries_lcc, 
                                countries_lcc$adm0_a3 %in% c("USA", "CAN", "MEX")),
                  color = "black", linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr_binary_lcc, 
           ylim = ylimr_binary_lcc, expand = FALSE) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        legend.position = "none")

richness_binary_f_lcc <- ggplot() +
  geom_spatvector(data = states_lcc, color = NA, fill = "white") +
  geom_spatraster(data = rich_binary_f_lcc) +
  scale_fill_gradientn(colors = rich_cols[c(1, 5)], na.value = NA) +
  geom_spatvector(data = states_lcc, color = "gray50", linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = filter(countries_lcc, 
                                countries_lcc$adm0_a3 %in% c("USA", "CAN", "MEX")),
                  color = "black", linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr_binary_lcc, 
           ylim = ylimr_binary_lcc, expand = FALSE) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        legend.position = "none")

# Add those binary maps to our multi-panel image, but first need to move the 
# legend inside the plot for those richness and delta map
# So much fiddly bits!
legend_title_size <- 8
legend_text_size <- 6
legend_key_height <- 10
legend_key_width <- 12
legend_position <- c(0.12, 0.22)
legend_margin <- margin(c(0, 0, 0, 0))
richness_c_lcc_5 <- richness_c_lcc +
  theme(legend.title = element_text(size = legend_title_size),
        legend.text = element_text(size = legend_text_size),
        legend.key.height = unit(legend_key_height, "pt"),
        legend.key.width = unit(legend_key_width, "pt"),
        legend.position = "inside",
        legend.position.inside = legend_position,
        legend.margin = legend_margin)
richness_f_lcc_5 <- richness_f_lcc +
  theme(legend.title = element_text(size = legend_title_size),
        legend.text = element_text(size = legend_text_size),
        legend.key.height = unit(legend_key_height, "pt"),
        legend.key.width = unit(legend_key_width, "pt"),
        legend.position = "inside",
        legend.position.inside = legend_position,
        legend.margin = legend_margin)
richness_d_lcc_5 <- richness_d_lcc +
  theme(legend.title = element_text(size = legend_title_size),
        legend.text = element_text(size = legend_text_size),
        legend.key.height = unit(legend_key_height, "pt"),
        legend.key.width = unit(legend_key_width, "pt"),
        legend.position = "inside",
        legend.position.inside = legend_position,
        legend.margin = legend_margin)

# Combine everything
r_lcc_5 <- plot_grid(richness_c_lcc_5, richness_binary_c_lcc,
                     richness_f_lcc_5, richness_binary_f_lcc,
                     richness_d_lcc_5,
                     align = "h",
                     ncol = 2,
                     labels = "auto",
                     vjust = 1,
                     hjust = 0)
richness_lcc_5panel <- paste0(output_basename, "richness_lcc_5panel.png")
if (!file.exists(richness_lcc_5panel) | (file.exists(richness_lcc_5panel) & replace)) {
  ggsave(filename = richness_lcc_5panel,
         plot = r_lcc_5,
         width = 6,
         height = 8,
         units = "in")
}

#####
# Now we want a 6-panel figure of hotspots for each time (2) and climate (3) 
# scenario. Only doing lambert projection for now.

# Future scenarios
scenarios <- c("ssp245", "ssp370", "ssp585")
times <- c("2041", "2071")
type <- "ov"

# Colors
graycol <- "#f2f2f2"
hotspot_col <- "#D24E71" # Fifth color in richness colors from above

# Crop the eastern edge of states and countries layers
state_ext <- ext(states)
state_ext[2] <- -45
states <- crop(states, state_ext)
countries <- crop(countries, state_ext)

# Project layers to Lambert Conformal Conic North America
states_lcc <- project(states, "ESRI:102009")
countries_lcc <- project(countries, crs(states_lcc))

# We will use same extent values as in delta plots
lims_ext <- ext(c(-168, -48, 15, 75))

# List to hold plots
hotspot_plots <- vector(mode = "list", 
                        length = length(scenarios) * length(times))

element_i <- 1
for (scenario_i in scenarios) {
  for (time_i in times) {
    scenario <- paste0(scenario_i, "_", time_i)
    message("Plotting ", scenario, " hotspots")
    richness_file <- paste0("output/richness/ensemble_", scenario, 
                            "-richness-", type, ".rds")
    richness_ras <- readRDS(file = richness_file)
    # Crop to extent of interest (here the same a delta plots)
    richness_ras <- crop(richness_ras, lims_ext)
    
    # Reproject in Lambert
    richness_ras <- project(richness_ras, crs(states_lcc))
    
    # Get rid of NA values and calculate extent for plot area
    richness_ras <- drop_na(richness_ras)
    # Add a little bit to all sides of extent and use that extent for limits
    rich_ext <- ext(richness_ras) * 1.01
    xlim_rich <- c(ext(rich_ext)[1], ext(rich_ext)[2])
    ylim_rich <- c(ext(rich_ext)[3], ext(rich_ext)[4])
    
    # Now turn into binary map of hotspot (>= 4 species) or not 
    richness_binary <- terra::classify(x = richness_ras,
                                       rcl = matrix(data = c(-Inf, 3.1, 0,
                                                             3.1, Inf, 1),
                                                    nrow = 2, byrow = TRUE))
    
    # Get the scenario & time text for annotation
    # Start by setting time value
    time_text <- dplyr::if_else(time_i == "2041", 
                                true = "2050s", 
                                false = "2080s")
    
    # Now add to end of re-formatted SSP scenario
    anno_text <- paste0("SSP", 
                        substr(scenario_i, start = 4, stop = 4), # 2, 3, or 5
                        "-",
                        substr(scenario_i, start = 5, stop = 5), # 4, 7, or 8,
                        ".",
                        substr(scenario_i, start = 6, stop = 6), # 5, 0, or 5
                        "\n",
                        time_text)
    
    anno_df <- data.frame(x = -3610000,
                          y = -1700000,
                          label = anno_text)
    
    # Plot the binary (hotspot or not) plot
    richness_binary_plot <- ggplot() +
      geom_spatvector(data = states_lcc, color = NA, fill = "white") +
      geom_spatraster(data = richness_binary) +
      scale_fill_gradientn(colors = c(graycol, hotspot_col), na.value = NA) +
      geom_spatvector(data = states_lcc, color = "gray50", linewidth = linewidth,
                      fill = NA) +
      geom_spatvector(data = filter(countries_lcc, 
                                    countries_lcc$adm0_a3 %in% c("USA", "CAN", "MEX")),
                      color = "black", linewidth = linewidth, fill = NA) +
      # annotate(geom = "text", x = -3810000, y = 2900000, label = anno_text) +
      geom_label(data = anno_df, mapping = aes(x = x, y = y, label = label),
                 size = 3, hjust = 0) +
      coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim_rich, 
               ylim = ylim_rich, expand = FALSE) +
      theme_bw() +
      theme(plot.margin = unit(margins, "pt"),
            legend.position = "none",
            axis.title = element_blank()) # only necessary because of geom_label
    richness_binary_plot
    
    # Update our big plot list
    hotspot_plots[[element_i]] <- richness_binary_plot
    names(hotspot_plots)[element_i] <- scenario
    element_i <- element_i + 1
  }
}

# Make 3 x 2 panel plot
h_6 <- plot_grid(plotlist = hotspot_plots,
                 align = "h",
                 ncol = 2,
                 labels = "auto",
                 hjust = -2)
hotspots_6panel <- paste0(output_basename, "hotspots_6panel.png")
if (!file.exists(hotspots_6panel) | (file.exists(hotspots_6panel) & replace)) {
  ggsave(filename = hotspots_6panel,
         plot = h_6,
         width = 6,
         height = 8,
         units = "in")
}


