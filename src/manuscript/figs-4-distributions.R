# Predicted distributions and deltas
# Erin Zylstra, Jeff Oliver
# ezylstra@arizona.edu, jcoliver@arizona.edu
# 2025-01-16

require(dplyr)
require(terra)
require(ggplot2)
require(tidyterra)
require(cowplot)

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

# Figure out how many (and which) species have contemporary predictions
# One way, maybe not the best
# spp_sdms <- gbif %>%
#   filter(pa_csv == "yes")
# sum(spp_sdms$n_background < 10000) / nrow(spp_sdms) * 100

# Pull species with contemporary predictions out of the ih data.frame
species <- NULL # TODO: make a vector of species names

# List to hold contemporary species predictions
contemporary_plots <- vector(mode = "list", length = length(species))
names(contemporary_plots) <- species

# List to hold all the forecast and delta plots
# TODO: Need to figure out how many have forecast predictions..?
species_plots <- vector(mode = "list", length = length(species))
names(species_plots) <- species

# Forecast scenarios and time points
scenarios <- c("ssp245", "ssp370", "ssp585")
times <- c("2041", "2071")

# TODO: testing iteration stuff here
species <- c("indra", "rumiko", "rutulus", "zelicaon")
for (species_i in 1:length(species)) {
  one_species <- species[species_i]
  for (time_i in 1:length(times)) {
    time <- times[time_i]
    for (scenario_i in 1:length(scenarios)) {
      scenario <- scenarios[scenario_i]
      # For the list of plots for this species (up to 12 plots), we need to 
      # keep track of the forecast plot and the delta plot for this 
      # scenario/time combination.
      #  1  2   SSP2 (1)  2041 (1)
      #  3  4   SSP3 (2)  2041 (1)
      #  5  6   SSP5 (3)  2041 (1)
      #  7  8   SSP2 (1)  2071 (2)
      #  9 10   SSP3 (2)  2071 (2)
      # 11 12   SSP5 (3)  2071 (2)
      
      # Starting point is how many full sets have already been done
      base <- (time_i - 1) * length(scenarios) * 2 + 1 
      scenario_add <- (scenario_i - 1) * 2
      forecast_i <- (time_i - 1) * length(scenarios) * 2 + 1 + (scenario_i - 1) * 2
      delta_i <- forecast_i + 1
      
      # "model" is the text combination of the scenario (ssp) & time (year)
      model <- paste0(scenario, "_", time)
      
      message(model, ": ", forecast_i, ", ", delta_i)
    }
  }
}      


# Iterate over all species
for (species_i in 1:length(species)) {
  one_species <- species[species_i]
  one_species_plots <- vector(mode = "list", 
                              length = length(scenarios) * length(times) * 2)
    
  # Get raster of contemporary predictions

  # Do re-projection to Lambert
  
  # Make plot for contemporary predictions
  current_plot <- ggplot()
  contemporary_plots[[one_species]] <- current_plot
  # Now make forecast and delta plot for each ssp and future time period, as 
  # supplemental output is going to have one time period per page, we use the 
  # time (year) as the outer loop and scenario (SSP) as the inner loop to make 
  # subsequent cowplot::plot_grid wrangling a little easier.
  for (time_i in 1:length(times)) {
    time <- times[time_i]
    for (scenario_i in 1:length(scenarios)) {
      scenario <- scenarios[scenario_i]
      # Enumerator to keep track of list elements
      # TODO: Needs to be updated because we now have two plots for each 
      # ssp/time combo: prediction & delta
      list_i <- (time_i - 1) * length(scenario) + scenario_i

      # "model" is the text combination of the scenario (ssp) & time (year)
      model <- paste0(scenario, "_", time)
      # message("Plotting ", model, " richness & hotspots [", list_i, "]")
      
      # Get forecast raster
      
      # Do re-projection to Lambert
      
      # Get delta raster
      
      # Re-project delta to Lambert
      
      # Any necessary cropping?
      
      # Make forecast plot
      forecast_plot <- ggplot()
      
      # Store forecast plot
      
      # Make delta plot
      delta_plot <- ggplot()
      
      # Store forecast plot
    }
  }
}


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