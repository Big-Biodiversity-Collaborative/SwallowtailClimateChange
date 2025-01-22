# Predicted distributions and deltas
# Erin Zylstra, Jeff Oliver
# ezylstra@arizona.edu, jcoliver@arizona.edu
# 2025-01-16

require(dplyr)
require(terra)
require(ggplot2)
require(tidyterra)
require(cowplot)
source(file = "functions/get_colors.R")

# Two figures
# Main manuscript: six panel figure, one column each for P. cresphontes & P. 
#   rumiko
#   + Predicted suitable areas for contemporary climate
#   + Predicted suitable areas for 2050s, under SSP3-7.0
#   + Difference in area between two predictions
# Supplemental: woo boy. Two things, really.
#   Multi-panel figure with predicted area for current climate for each species
#     (a total of 15 species, including P. appalachiensis and P. palamedes), 
#     this probably works best as two pages of 6 and one page of 3 panels, AND 
#   Two 12-panel figures for each species with forecast predictions (I think 
#     it is just 13 species, excluding P. appalachiensis and P. palamedes):
#   + Page 1, column 1: suitable areas for 2041 for three SSPs
#   + Page 1, column 2: differences in area from current predicted areas
#   + Page 2, column 1: suitable areas for 2071 for three SSPs
#   + Page 2, column 2: differences in area from current predicted areas

# To ensure consistency across figures, we will go ahead and make plot objects
# for each species (that we have the appropriate data for), and store these in 
# a list (of lists?). We'll do subsequent iteration across the list to create 
# the figure for the main manuscript and for the supplemental. Note 
# supplemental will be slightly duplicative, as it will also include the two 
# species in the main manuscript figure.

ih <- read.csv("data/insect-host.csv")
gbif <- read.csv("data/gbif-pa-summary.csv")

# Figure out which species have current predictions (currently based on 
# whether or not the presence/absence data were created)
gbif <- gbif %>%
  dplyr::filter(species %in% unique(ih$insect)) %>%
  dplyr::filter(pa_csv == "yes")

# Pull species with current predictions out of the gbif data.frame
species <- gbif$species

# Grab spatial files with political boundaries
states <- vect("data/political-boundaries/states.shp")  
countries <- vect("data/political-boundaries/countries.shp")

# Reproject states and countries (can take a couple moments with first call to 
# project)
states <- project(states, "ESRI:102009")
countries <- project(countries, crs(states))

# For plotting purposes, we only need Canada, Mexico, and US because the LCC 
# projection is centered in the US, and country boundaries become very 
# distorted for Greenland and Central America
countries <- countries %>%
  filter(adm0_a3 %in% c("USA", "CAN", "MEX"))

# List to hold current species predictions
current_plots <- vector(mode = "list", length = length(species))
names(current_plots) <- species

# List to hold all the forecast and delta plots
species_plots <- vector(mode = "list", length = length(species))
names(species_plots) <- species

# Forecast scenarios and time points
scenarios <- c("ssp245", "ssp370", "ssp585")
times <- c("2041", "2071")

# TODO: testing iteration stuff here. Skipping for now. Requires combinatorics 
# expertise I don't have the time for.
# species <- c("indra", "rumiko", "rutulus", "zelicaon")
# for (species_i in 1:length(species)) {
#   one_species <- species[species_i]
#   for (time_i in 1:length(times)) {
#     time <- times[time_i]
#     for (scenario_i in 1:length(scenarios)) {
#       scenario <- scenarios[scenario_i]
#       # For the list of plots for this species (up to 12 plots), we need to 
#       # keep track of the forecast plot and the delta plot for this 
#       # scenario/time combination.
#       #  1  2   SSP2 (1)  2041 (1)
#       #  3  4   SSP3 (2)  2041 (1)
#       #  5  6   SSP5 (3)  2041 (1)
#       #  7  8   SSP2 (1)  2071 (2)
#       #  9 10   SSP3 (2)  2071 (2)
#       # 11 12   SSP5 (3)  2071 (2)
#       
#       # Starting point is how many full sets have already been done
#       base <- (time_i - 1) * length(scenarios) * 2 + 1 
#       scenario_add <- (scenario_i - 1) * 2
#       forecast_i <- (time_i - 1) * length(scenarios) * 2 + 1 + (scenario_i - 1) * 2
#       delta_i <- forecast_i + 1
#       
#       # "model" is the text combination of the scenario (ssp) & time (year)
#       model <- paste0(scenario, "_", time)
#       
#       message(model, ": ", forecast_i, ", ", delta_i)
#     }
#   }
# }      

# Get colors for plots
dist_cols <- get_colors(palette = "overlap")
names(dist_cols) <- c("Absent", "Hosts only", "Swallowtail only", 
                      "Swallowtail and hosts")  
delta_cols <- get_colors(palette = "distdelta")

# Setting colors for state & country lines
state_fill <- "white"
state_color <- "gray50"
countries_color <- "black"

# TODO: Not used
# margins <- c(4, 0, 6, 0)

# Iterate over all species
nice_names <- tolower(gsub(pattern = " ",
                           replacement = "_",
                           x = species))
for (species_i in 1:length(nice_names)) {
  one_species <- nice_names[species_i]
  # This list will hold all forecast & delta plots for one species (and become 
  # one element of the species_plots list)
  one_species_plots <- vector(mode = "list", 
                              length = length(scenarios) * length(times) * 2)
    
  # Get raster of contemporary predictions
  current <- readRDS(paste0("output/overlaps/", one_species,
                                 "-overlap-current.rds"))
  
  # Update the rasters to have values we want (1, 2, 3, 4). Rasters coming in 
  # should have the following values:
  # 0 = (Insect and hosts absent) Insect and all host plants predicted absent
  # 1 = (1 host only) Insect predicted absent, only 1 host predicted present
  # 2 = (2 or more hosts) Insect predicted absent, >= 2 hosts predicted present
  # 3 = (Insect, no hosts) Insect predicted present, all hosts predicted absent
  # 4 = (Insect, only 1 host) Insect and only 1 host predicted present
  # 5 = (Insect, 2 or more hosts) Insect and >= 2 hosts predicted present
  # Update these to 0-3 scale
  
  current[current %in% 1:2] <- 1
  current[current == 3] <- 2
  current[current %in% 4:5] <- 3
  current <- as.factor(current)
  levels(current) <- data.frame(value = 0:3, label = names(dist_cols))
  
  # Do re-projection to Lambert & drop missing
  current <- project(current, crs(states), method = "near")
  current <- drop_na(current)

  # Figure out extent for plot (otherwise extent is based on CAN, MEX, USA)
  xlim_current <- c(ext(current)[1], ext(current)[2])
  ylim_current <- c(ext(current)[3], ext(current)[4])
  
  # Make plot for contemporary predictions
  current_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = state_fill) +
    geom_spatraster(data = current, maxcell = Inf) +
    scale_fill_manual(name = "label", values = dist_cols, na.translate = FALSE) +
    geom_spatvector(data = states, color = state_color, fill = NA) +
    geom_spatvector(data = countries, color = countries_color, fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim) +
    # scale_x_continuous(breaks = c(-120, -110, -100)) +
    theme_bw() +
    theme(legend.position = "none")
    # theme(plot.margin = unit(margins, "pt"), 
    #       axis.text = element_text(size = 10))
  
  current_plots[[one_species]] <- current_plot
  # Now make forecast and delta plot for each ssp and future time period, as 
  # supplemental output is going to have one time period per page, we use the 
  # time (year) as the outer loop and scenario (SSP) as the inner loop to make 
  # subsequent cowplot::plot_grid wrangling a little easier.
  # Keeps track of which spot in the list to insert the plot
  element_i <- 1
  for (time_i in 1:length(times)) {
    time <- times[time_i]
    for (scenario_i in 1:length(scenarios)) {
      scenario <- scenarios[scenario_i]
      # "model" is the text combination of the scenario (ssp) & time (year)
      model <- paste0(scenario, "_", time)

      # Get forecast raster
      forecast <- readRDS(paste0("output/overlaps/", one_species,
                                 "-overlap-ensemble_", model, ".rds"))

      # Update to 0-3 scale
      forecast[forecast %in% 1:2] <- 1
      forecast[forecast == 3] <- 2
      forecast[forecast %in% 4:5] <- 3
      forecast <- as.factor(forecast)
      levels(forecast) <- data.frame(value = 0:3, label = names(dist_cols))
      
      # Do re-projection to Lambert & drop missing
      forecast <- project(forecast, crs(states), method = "near")
      forecast <- drop_na(forecast)
      
      # Figure out extent for plot (otherwise extent is based on CAN, MEX, USA)
      xlim_forecast <- c(ext(forecast)[1], ext(forecast)[2])
      ylim_forecast <- c(ext(forecast)[3], ext(forecast)[4])
      
      # Make plot for forecast predictions
      forecast_plot <- ggplot() +
        geom_spatvector(data = states, color = NA, fill = state_fill) +
        geom_spatraster(data = forecast, maxcell = Inf) +
        scale_fill_manual(name = "label", values = dist_cols, na.translate = FALSE) +
        geom_spatvector(data = states, color = state_color, fill = NA) +
        geom_spatvector(data = countries, color = countries_color, fill = NA) +
        coord_sf(datum = sf::st_crs("EPSG:4326"), 
                 xlim = xlim_forecast, 
                 ylim = ylim_forecast) +
        theme_bw() +
        theme(legend.position = "none")

      one_species_plots[[element_i]] <- forecast_plot
      # Increment list element counter
      # TODO: Yuck yuck yuck
      element_i <- element_i + 1
      
      # Get delta raster
      delta <- readRDS(paste0("output/deltas/", one_species,
                                 "-delta-insecthost_", model, ".rds"))
      
      # Update to 0-2 scale
      # TODO: To here

      
      levels(forecast) <- data.frame(value = 0:3, label = names(dist_cols))
      
      # Do re-projection to Lambert & drop missing
      forecast <- project(forecast, crs(states), method = "near")
      forecast <- drop_na(forecast)
      
      # Figure out extent for plot (otherwise extent is based on CAN, MEX, USA)
      xlim_forecast <- c(ext(forecast)[1], ext(forecast)[2])
      ylim_forecast <- c(ext(forecast)[3], ext(forecast)[4])
      
      # Make plot for forecast predictions
      forecast_plot <- ggplot() +
        geom_spatvector(data = states, color = NA, fill = state_fill) +
        geom_spatraster(data = forecast, maxcell = Inf) +
        scale_fill_manual(name = "label", values = dist_cols, na.translate = FALSE) +
        geom_spatvector(data = states, color = state_color, fill = NA) +
        geom_spatvector(data = countries, color = countries_color, fill = NA) +
        coord_sf(datum = sf::st_crs("EPSG:4326"), 
                 xlim = xlim_forecast, 
                 ylim = ylim_forecast) +
        theme_bw() +
        theme(legend.position = "none")

      # Store forecast plot
      
            
      # Re-project delta to Lambert
      
      # Any necessary cropping?
      

      # Make delta plot
      delta_plot <- ggplot()
      
      # Store forecast plot
    }
  }
}

# We now have contemporary and forecast plots stored in lists. Need to pull out 
# specific plots to create main and supplemental figures

# Main manuscript: six panel figure, one column each for P. cresphontes & P. 
#   rumiko
#   + Predicted suitable areas for contemporary climate
#   + Predicted suitable areas for 2050s, under SSP3-7.0
#   + Difference in area between two predictions

# Pull out the two contemporary plots of interest

# Pull out the two list elements of forecast plot list corresponding to the two 
# species of interest

# Pull out the two forecast plots of interest (2050s + SSP3-7.0, and the delta) 
# for each of the two species

# Make six-panel plot with those six images

# Supplemental: woo boy. Two things, really.
#   Multi-panel figure with predicted area for current climate for each species
#     (a total of 15 species, including P. appalachiensis and P. palamedes), 
#     this probably works best as two pages of 6 and one page of 3 panels, AND 
#   Two 12-panel figures for each species with forecast predictions (I think 
#     it is just 13 species, excluding P. appalachiensis and P. palamedes):
#   + Page 1, column 1: suitable areas for 2041 for three SSPs
#   + Page 1, column 2: differences in area from current predicted areas
#   + Page 2, column 1: suitable areas for 2071 for three SSPs
#   + Page 2, column 2: differences in area from current predicted areas





################################################################################
# Below here is pasted from create-manuscripts-objects.R
################################################################################
# Figures: 2-swallowtail comparison (overlaps, deltas) ------------------------#

# For now, picking P. indra (western species; some gain [incl GB], some loss; 
# not limited by host plants) and P. cresphontes (eastern species; some gain
# [incl midwest], some loss; limited by host plants in FL)

# Species
spp <- c("indra", "cresphontes")
nice_names <- paste0("papilio_", spp)
abbr_names <- paste0("P. ", spp)
full_names <- paste0("Papilio ", spp)

# Future scenario
scen <- "ssp370_2041"

# Load overlap maps
overlapc1 <- readRDS(paste0("output/overlaps/", nice_names[1], 
                            "-overlap-current.rds"))
overlapc2 <- readRDS(paste0("output/overlaps/", nice_names[2], 
                            "-overlap-current.rds"))
overlapf1 <- readRDS(paste0("output/overlaps/", nice_names[1], 
                            "-overlap-ensemble_", scen, ".rds"))
overlapf2 <- readRDS(paste0("output/overlaps/", nice_names[2], 
                            "-overlap-ensemble_", scen, ".rds"))

labels_o <- c("Absent", "Hosts only", "Swallowtail only", "Swallowtail and hosts")  

# Create new overlap rasters with specified levels
overlapc1[overlapc1 %in% 1:2] <- 1
overlapc1[overlapc1 == 3] <- 2
overlapc1[overlapc1 %in% 4:5] <- 3
overlapc1 <- as.factor(overlapc1)
levels(overlapc1) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)
overlapc2[overlapc2 %in% 1:2] <- 1
overlapc2[overlapc2 == 3] <- 2
overlapc2[overlapc2 %in% 4:5] <- 3
overlapc2 <- as.factor(overlapc2)
levels(overlapc2) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)
overlapf1[overlapf1 %in% 1:2] <- 1
overlapf1[overlapf1 == 3] <- 2
overlapf1[overlapf1 %in% 4:5] <- 3
overlapf1 <- as.factor(overlapf1)
levels(overlapf1) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)
overlapf2[overlapf2 %in% 1:2] <- 1
overlapf2[overlapf2 == 3] <- 2
overlapf2[overlapf2 %in% 4:5] <- 3
overlapf2 <- as.factor(overlapf2)
levels(overlapf2) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)

# Using Paired color palette
color_vec_o <- c("#ededed",   # Absent
                 "#b2df8a",   # Hosts only
                 "#a6cee3",   # Insect only
                 "#1f78b4")   # Hosts and insect
names(color_vec_o) <- levels(overlapc1)[[1]][,"desc"]

# Get rid of NA values and calculate extent for plot areas
overlapc1 <- drop_na(overlapc1)
overlapc2 <- drop_na(overlapc2)
overlapf1 <- drop_na(overlapf1)
overlapf2 <- drop_na(overlapf2)
xlim1 <- c(ext(overlapf1)[1], ext(overlapf1)[2])
ylim1 <- c(ext(overlapf1)[3], ext(overlapf1)[4])
xlim2 <- c(ext(overlapf2)[1], ext(overlapf2)[2])
ylim2 <- c(ext(overlapf2)[3], ext(overlapf2)[4])

margins <- c(4, 0, 6, 0)

# Create overlap map objects
overlapc1_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = "white") +
  geom_spatraster(data = overlapc1, maxcell = Inf) +
  scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
  geom_spatvector(data = states, color = "gray50", fill = NA) +
  geom_spatvector(data = countries, color = "black", fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
  scale_x_continuous(breaks = c(-120, -110, -100)) +
  theme_bw() +
  ylab("Current") +
  theme(plot.margin = unit(margins, "pt"), 
        axis.text = element_text(size = 10))
overlapc2_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = "white") +
  geom_spatraster(data = overlapc2, maxcell = Inf) +
  scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
  geom_spatvector(data = states, color = "gray50", fill = NA) +
  geom_spatvector(data = countries, color = "black", fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim2, ylim = ylim2) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        axis.text = element_text(size = 10))
overlapf1_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = "white") +
  geom_spatraster(data = overlapf1, maxcell = Inf) +
  scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
  geom_spatvector(data = states, color = "gray50", fill = NA) +
  geom_spatvector(data = countries, color = "black", fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
  scale_x_continuous(breaks = c(-120, -110, -100)) +
  theme_bw() +
  ylab("Future") +
  theme(plot.margin = unit(margins, "pt"), 
        axis.text = element_text(size = 10))
overlapf2_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = "white") +
  geom_spatraster(data = overlapf2, maxcell = Inf) +
  scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
  geom_spatvector(data = states, color = "gray50", fill = NA) +
  geom_spatvector(data = countries, color = "black", fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim2, ylim = ylim2) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"))

# Load delta maps
delta1 <- readRDS(paste0("output/deltas/", nice_names[1], "-delta-insecthost-",
                         scen, ".rds"))
delta2 <- readRDS(paste0("output/deltas/", nice_names[2], "-delta-insecthost-",
                         scen, ".rds"))

labels_d <- c("Absent", "Loss", "Gain", "Stable")  

delta1 <- as.factor(delta1)
levels(delta1) <- data.frame(value = c(0, 1, 2, 3), desc = labels_d)
delta2 <- as.factor(delta2)
levels(delta2) <- data.frame(value = c(0, 1, 2, 3), desc = labels_d)

# Tweaked version of RDYlBu palette (so colors pop on light gray background)
color_vec_d <- c("#ededed",   # Absent
                 "#fc8d59",   # Loss
                 "#2c7bb6",   # Gain
                 "#ffffbf")   # Stable
names(color_vec_d) <- levels(delta1)[[1]][,"desc"]

# Get rid of NA values and calculate extent for plot areas
delta1 <- drop_na(delta1)
delta2 <- drop_na(delta2)

# Create delta map objects
delta1_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = "white") +
  geom_spatraster(data = delta1, maxcell = Inf) +
  scale_fill_manual(name = "desc", values = color_vec_d, na.translate = FALSE) +
  geom_spatvector(data = states, color = "gray50", fill = NA) +
  geom_spatvector(data = countries, color = "black", fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
  scale_x_continuous(breaks = c(-120, -110, -100)) +
  theme_bw() +
  ylab("Future - Current") +
  theme(plot.margin = unit(margins, "pt"), 
        axis.text = element_text(size = 10))
delta2_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = "white") +
  geom_spatraster(data = delta2, maxcell = Inf) +
  scale_fill_manual(name = "desc", values = color_vec_d, na.translate = FALSE) +
  geom_spatvector(data = states, color = "gray50", fill = NA) +
  geom_spatvector(data = countries, color = "black", fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim2, ylim = ylim2) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"))

# Create legends (Had to change things up, since cowplot::get_legend didn't 
# seem to work anymore)
# Function to grab legend from ggplot object
get_leg <- function(myggplot) {
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
overlap_for_legend <- ggplot() +
  geom_spatraster(data = overlapc1, maxcell = Inf) +
  scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
  theme(legend.position = "bottom",
        legend.title = element_blank())
legend_o <- get_leg(overlap_for_legend)
delta_for_legend <- ggplot() +
  geom_spatraster(data = delta1, maxcell = Inf) +
  scale_fill_manual(name = "desc", values = color_vec_d, na.translate = FALSE) +
  theme(legend.position = "bottom",
        legend.title = element_blank())
legend_d <- get_leg(delta_for_legend)

# Combine things
overlaps <- plot_grid(overlapc1_plot + theme(legend.position = "none"),
                      overlapc2_plot + theme(legend.position = "none"),
                      overlapf1_plot + theme(legend.position = "none"),
                      overlapf2_plot + theme(legend.position = "none"),
                      align = "vh",
                      nrow = 2,
                      byrow = TRUE)
deltas <- plot_grid(delta1_plot + theme(legend.position = "none"),
                    delta2_plot + theme(legend.position = "none"),
                    align = 'vh',
                    nrow = 1)
col_titles <- ggdraw() + 
  draw_label(full_names[1], fontface = "italic", size = 12, x = 0.29) +
  draw_label(full_names[2], fontface = "italic", size = 12, x = 0.79)

p <- plot_grid(col_titles, overlaps, legend_o, NULL, deltas, legend_d,
               ncol = 1, rel_heights = c(0.2, 5.6, 0.2, 0.2, 2.8, 0.2))

twospecies_name <- paste0(output_basename, "twospecies_6panel.png")
if (!file.exists(twospecies_name) | (file.exists(twospecies_name) & replace)) {
  ggsave(filename = twospecies_name,
         plot = p,
         width = 6.5,
         height = 9,
         units = "in")
}