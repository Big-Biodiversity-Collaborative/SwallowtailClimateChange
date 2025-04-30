# Script to create 6-panel figures for supplement
# Erin Zylstra
# ezylstra@arizona.edu
# 2024-08-22

require(dplyr)
require(stringr)
require(terra)
require(ggplot2)
require(tidyterra)
require(gridExtra)
require(cowplot)

# DEPRECATED
# For the most part. Most of this is farmed out to various scripts in 
# src/manuscript now.

# Creating 6-panel figures (3x2)for each future climate scenario
  # Left column = 2041-2070; right column = 2071-2100
  # Top row = SSP245; middle = SSP370; bottom = SSP585

# Logical indicating whether to recreate maps/tables if they already exist
replace <- TRUE

# Base of output filenames
output_basename <- "output/manuscript/"

# Grab spatial files with political boundaries
countries <- vect("data/political-boundaries/countries.shp")
countries3 <- filter(countries, countries$adm0_a3 %in% c("USA", "CAN", "MEX"))
states <- vect("data/political-boundaries/states.shp")  

# Function to grab a legend from ggplot object
get_leg <- function(myggplot) {
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Creating overlap map figures ------------------------------------------------#

species <- c("indra", "cresphontes")

for (spp in species) {

  nice_name <- paste0("papilio_", spp)
  abbr_name <- paste0("P. ", spp)
  full_name <- paste0("Papilio ", spp)
  
  # Load overlap maps
  overlap11 <- readRDS(paste0("output/overlaps/", nice_name, 
                              "-overlap-ensemble_ssp245_2041.rds"))
  overlap21 <- readRDS(paste0("output/overlaps/", nice_name, 
                              "-overlap-ensemble_ssp370_2041.rds"))
  overlap31 <- readRDS(paste0("output/overlaps/", nice_name, 
                              "-overlap-ensemble_ssp585_2041.rds"))
  overlap12 <- readRDS(paste0("output/overlaps/", nice_name, 
                              "-overlap-ensemble_ssp245_2071.rds"))
  overlap22 <- readRDS(paste0("output/overlaps/", nice_name, 
                              "-overlap-ensemble_ssp370_2071.rds"))
  overlap32 <- readRDS(paste0("output/overlaps/", nice_name, 
                              "-overlap-ensemble_ssp585_2071.rds"))
  
  labels_o <- c("Absent", "Hosts only", "Swallowtail only", "Swallowtail and hosts")  
  
  # Create new overlap rasters with specified levels
  overlap11[overlap11 %in% 1:2] <- 1
  overlap11[overlap11 == 3] <- 2
  overlap11[overlap11 %in% 4:5] <- 3
  overlap11 <- as.factor(overlap11)
  levels(overlap11) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)
  overlap21[overlap21 %in% 1:2] <- 1
  overlap21[overlap21 == 3] <- 2
  overlap21[overlap21 %in% 4:5] <- 3
  overlap21 <- as.factor(overlap21)
  levels(overlap21) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)
  overlap31[overlap31 %in% 1:2] <- 1
  overlap31[overlap31 == 3] <- 2
  overlap31[overlap31 %in% 4:5] <- 3
  overlap31 <- as.factor(overlap31)
  levels(overlap31) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)
  overlap12[overlap12 %in% 1:2] <- 1
  overlap12[overlap12 == 3] <- 2
  overlap12[overlap12 %in% 4:5] <- 3
  overlap12 <- as.factor(overlap12)
  levels(overlap12) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)
  overlap22[overlap22 %in% 1:2] <- 1
  overlap22[overlap22 == 3] <- 2
  overlap22[overlap22 %in% 4:5] <- 3
  overlap22 <- as.factor(overlap22)
  levels(overlap22) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)
  overlap32[overlap32 %in% 1:2] <- 1
  overlap32[overlap32 == 3] <- 2
  overlap32[overlap32 %in% 4:5] <- 3
  overlap32 <- as.factor(overlap32)
  levels(overlap32) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)
  
  # Using Paired color palette
  color_vec_o <- c("#ededed",   # Absent
                   "#b2df8a",   # Hosts only
                   "#a6cee3",   # Insect only
                   "#1f78b4")   # Hosts and insect
  names(color_vec_o) <- levels(overlap11)[[1]][,"desc"]
  
  # Get rid of NA values and calculate extent for plot areas
  overlap11 <- drop_na(overlap11)
  overlap21 <- drop_na(overlap21)
  overlap31 <- drop_na(overlap31)
  overlap12 <- drop_na(overlap12)
  overlap22 <- drop_na(overlap22)
  overlap32 <- drop_na(overlap32)
  
  xlim1 <- c(ext(overlap21)[1], ext(overlap21)[2])
  ylim1 <- c(ext(overlap21)[3], ext(overlap21)[4])
  
  margins <- c(4, 0, 6, 0)
  
  # Create overlap map objects
  overlap11_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = "white") +
    geom_spatraster(data = overlap11, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray50", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
    theme_bw() +
    ylab("SSP245") +
    theme(plot.margin = unit(margins, "pt"), 
          axis.text = element_text(size = 10))
  overlap21_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = "white") +
    geom_spatraster(data = overlap21, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray50", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
    theme_bw() +
    ylab("SSP370") +
    theme(plot.margin = unit(margins, "pt"), 
          axis.text = element_text(size = 10))
  overlap31_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = "white") +
    geom_spatraster(data = overlap31, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray50", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
    theme_bw() +
    ylab("SSP585") +
    theme(plot.margin = unit(margins, "pt"), 
          axis.text = element_text(size = 10))
  overlap12_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = "white") +
    geom_spatraster(data = overlap12, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray50", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
    theme_bw() +
    theme(plot.margin = unit(margins, "pt"), 
          axis.text = element_text(size = 10))
  overlap22_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = "white") +
    geom_spatraster(data = overlap22, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray50", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
    theme_bw() +
    theme(plot.margin = unit(margins, "pt"), 
          axis.text = element_text(size = 10))
  overlap32_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = "white") +
    geom_spatraster(data = overlap32, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray50", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
    theme_bw() +
    theme(plot.margin = unit(margins, "pt"), 
          axis.text = element_text(size = 10))
  if (spp == "indra") {
    overlap11_plot <- overlap11_plot +
      scale_x_continuous(breaks = c(-120, -110, -100)) 
    overlap21_plot <- overlap21_plot +
      scale_x_continuous(breaks = c(-120, -110, -100)) 
    overlap31_plot <- overlap31_plot +
      scale_x_continuous(breaks = c(-120, -110, -100)) 
    overlap12_plot <- overlap12_plot +
      scale_x_continuous(breaks = c(-120, -110, -100)) 
    overlap22_plot <- overlap22_plot +
      scale_x_continuous(breaks = c(-120, -110, -100)) 
    overlap32_plot <- overlap32_plot +
      scale_x_continuous(breaks = c(-120, -110, -100))  
  }
  
  overlap_for_legend <- ggplot() +
    geom_spatraster(data = overlap11, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
    theme(legend.position = "bottom",
          legend.title = element_blank())
  legend_o <- get_leg(overlap_for_legend)
  
  # Combine things
  overlaps <- plot_grid(overlap11_plot + theme(legend.position = "none"),
                        overlap21_plot + theme(legend.position = "none"),
                        overlap31_plot + theme(legend.position = "none"),
                        overlap12_plot + theme(legend.position = "none"),
                        overlap22_plot + theme(legend.position = "none"),
                        overlap32_plot + theme(legend.position = "none"),
                        align = "vh",
                        nrow = 3,
                        byrow = FALSE)
  col_titles <- ggdraw() + 
    draw_label("2041-2070", size = 12, x = 0.29) +
    draw_label("2071-2100", size = 12, x = 0.79)
  
  p <- plot_grid(col_titles, overlaps, legend_o,
                 ncol = 1, rel_heights = c(0.2, 8.6, 0.2))
  
  plot_name <- paste0(output_basename, nice_name, "-overlap-6panel.png")
  if (!file.exists(plot_name) | (file.exists(plot_name) & replace)) {
    ggsave(filename = plot_name,
           plot = p,
           width = 6.5,
           height = 9,
           units = "in")
  }
}

# Creating delta map figures --------------------------------------------------#

species <- c("indra", "cresphontes")

for (spp in species) {
  
  nice_name <- paste0("papilio_", spp)
  abbr_name <- paste0("P. ", spp)
  full_name <- paste0("Papilio ", spp)
  
  # Load delta maps
  delta11 <- readRDS(paste0("output/deltas/", nice_name, 
                            "-delta-insecthost-ssp245_2041.rds"))
  delta21 <- readRDS(paste0("output/deltas/", nice_name, 
                            "-delta-insecthost-ssp370_2041.rds"))
  delta31 <- readRDS(paste0("output/deltas/", nice_name, 
                            "-delta-insecthost-ssp585_2041.rds"))
  delta12 <- readRDS(paste0("output/deltas/", nice_name, 
                            "-delta-insecthost-ssp245_2071.rds"))
  delta22 <- readRDS(paste0("output/deltas/", nice_name, 
                            "-delta-insecthost-ssp370_2071.rds"))
  delta32 <- readRDS(paste0("output/deltas/", nice_name, 
                            "-delta-insecthost-ssp585_2071.rds"))
  
  labels_d <- c("Absent", "Loss", "Gain", "Stable") 
  
  delta11 <- as.factor(delta11)
  levels(delta11) <- data.frame(value = c(0, 1, 2, 3), desc = labels_d)
  delta21 <- as.factor(delta21)
  levels(delta21) <- data.frame(value = c(0, 1, 2, 3), desc = labels_d)
  delta31 <- as.factor(delta31)
  levels(delta31) <- data.frame(value = c(0, 1, 2, 3), desc = labels_d)
  delta12 <- as.factor(delta12)
  levels(delta12) <- data.frame(value = c(0, 1, 2, 3), desc = labels_d)
  delta22 <- as.factor(delta22)
  levels(delta22) <- data.frame(value = c(0, 1, 2, 3), desc = labels_d)
  delta32 <- as.factor(delta32)
  levels(delta32) <- data.frame(value = c(0, 1, 2, 3), desc = labels_d)
  
  # Tweaked version of RDYlBu palette (so colors pop on light gray background)
  color_vec_d <- c("#ededed",   # Absent
                   "#fc8d59",   # Loss
                   "#2c7bb6",   # Gain
                   "#ffffbf")   # Stable
  names(color_vec_d) <- levels(delta11)[[1]][,"desc"]
  
  # Get rid of NA values and calculate extent for plot areas
  delta11 <- drop_na(delta11)
  delta21 <- drop_na(delta21)
  delta31 <- drop_na(delta31)
  delta12 <- drop_na(delta12)
  delta22 <- drop_na(delta22)
  delta32 <- drop_na(delta32)
  
  xlim1 <- c(ext(delta21)[1], ext(delta21)[2])
  ylim1 <- c(ext(delta21)[3], ext(delta21)[4])
  
  margins <- c(4, 0, 6, 0)
  
  # Create delta map objects
  delta11_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = "white") +
    geom_spatraster(data = delta11, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_d, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray50", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
    theme_bw() +
    ylab("SSP245") +
    theme(plot.margin = unit(margins, "pt"), 
          axis.text = element_text(size = 10))
  delta21_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = "white") +
    geom_spatraster(data = delta21, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_d, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray50", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
    theme_bw() +
    ylab("SSP370") +
    theme(plot.margin = unit(margins, "pt"), 
          axis.text = element_text(size = 10))
  delta31_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = "white") +
    geom_spatraster(data = delta31, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_d, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray50", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
    theme_bw() +
    ylab("SSP585") +
    theme(plot.margin = unit(margins, "pt"), 
          axis.text = element_text(size = 10))
  delta12_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = "white") +
    geom_spatraster(data = delta12, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_d, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray50", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
    theme_bw() +
    theme(plot.margin = unit(margins, "pt"), 
          axis.text = element_text(size = 10))
  delta22_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = "white") +
    geom_spatraster(data = delta22, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_d, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray50", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
    theme_bw() +
    theme(plot.margin = unit(margins, "pt"), 
          axis.text = element_text(size = 10))
  delta32_plot <- ggplot() +
    geom_spatvector(data = states, color = NA, fill = "white") +
    geom_spatraster(data = delta32, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_d, na.translate = FALSE) +
    geom_spatvector(data = states, color = "gray50", fill = NA) +
    geom_spatvector(data = countries, color = "black", fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
    theme_bw() +
    theme(plot.margin = unit(margins, "pt"), 
          axis.text = element_text(size = 10))
  if (spp == "indra") {
    delta11_plot <- delta11_plot +
      scale_x_continuous(breaks = c(-120, -110, -100)) 
    delta21_plot <- delta21_plot +
      scale_x_continuous(breaks = c(-120, -110, -100)) 
    delta31_plot <- delta31_plot +
      scale_x_continuous(breaks = c(-120, -110, -100)) 
    delta12_plot <- delta12_plot +
      scale_x_continuous(breaks = c(-120, -110, -100)) 
    delta22_plot <- delta22_plot +
      scale_x_continuous(breaks = c(-120, -110, -100)) 
    delta32_plot <- delta32_plot +
      scale_x_continuous(breaks = c(-120, -110, -100))  
  }
  
  delta_for_legend <- ggplot() +
    geom_spatraster(data = delta11, maxcell = Inf) +
    scale_fill_manual(name = "desc", values = color_vec_d, na.translate = FALSE) +
    theme(legend.position = "bottom",
          legend.title = element_blank())
  legend_d <- get_leg(delta_for_legend)
  
  # Combine things
  deltas <- plot_grid(delta11_plot + theme(legend.position = "none"),
                      delta21_plot + theme(legend.position = "none"),
                      delta31_plot + theme(legend.position = "none"),
                      delta12_plot + theme(legend.position = "none"),
                      delta22_plot + theme(legend.position = "none"),
                      delta32_plot + theme(legend.position = "none"),
                      align = "vh",
                      nrow = 3,
                      byrow = FALSE)
  col_titles <- ggdraw() + 
    draw_label("2041-2070", size = 12, x = 0.29) +
    draw_label("2071-2100", size = 12, x = 0.79)
  
  p <- plot_grid(col_titles, deltas, legend_d,
                 ncol = 1, rel_heights = c(0.2, 8.6, 0.2))
  
  plot_name <- paste0(output_basename, nice_name, "-delta-6panel.png")
  if (!file.exists(plot_name) | (file.exists(plot_name) & replace)) {
    ggsave(filename = plot_name,
           plot = p,
           width = 6.5,
           height = 9,
           units = "in")
  }
}

# Creating richness figures ---------------------------------------------------#

# Type of map 
# ov = insect distributed only where one or more host also occurs
# io = insect distribution depends only on climate)
type <- "ov"

# Load richness rasters
rich11 <- readRDS(paste0("output/richness/ensemble_ssp245_2041-richness-",
                         type, ".rds"))
rich21 <- readRDS(paste0("output/richness/ensemble_ssp370_2041-richness-",
                         type, ".rds"))
rich31 <- readRDS(paste0("output/richness/ensemble_ssp585_2041-richness-",
                         type, ".rds"))
rich12 <- readRDS(paste0("output/richness/ensemble_ssp245_2071-richness-",
                         type, ".rds"))
rich22 <- readRDS(paste0("output/richness/ensemble_ssp370_2071-richness-",
                         type, ".rds"))
rich32 <- readRDS(paste0("output/richness/ensemble_ssp585_2071-richness-",
                         type, ".rds"))

# Define color palette, with light gray for 0 values
rich_cols <- c("#f2f2f2", ebirdst::ebirdst_palettes(7, "weekly"))

# Few plotting parameters
margins <- c(2, 0, 6, 0)
linewidth <- 0.1

# Project maps using Lambert Conformal Conic North America. Croping the eastern
# edge of states and countries layers. Also note that when plotting, we're 
# removing boundaries for all countries except the US, Canada, and Mexico 
# because with this projection that is centered in the US, country boundaries 
# become very distorted for Greenland and Central America.
state_ext <- ext(states)
state_ext[2] <- -45
states <- crop(states, state_ext)
countries <- crop(countries, state_ext)
states_lcc <- project(states, "ESRI:102009")
countries_lcc <- project(countries, crs(states_lcc))
rich11_lcc <- project(rich11, crs(states_lcc))
rich21_lcc <- project(rich21, crs(states_lcc))
rich31_lcc <- project(rich31, crs(states_lcc))
rich12_lcc <- project(rich12, crs(states_lcc))
rich22_lcc <- project(rich22, crs(states_lcc))
rich32_lcc <- project(rich32, crs(states_lcc))

# Get rid of NA values and calculate extent for plot area
rich11_lcc <- drop_na(rich11_lcc)
limsr_lcc <- ext(rich11_lcc) * 1.01
xlimr_lcc <- c(ext(limsr_lcc)[1], ext(limsr_lcc)[2])
ylimr_lcc <- c(ext(limsr_lcc)[3], ext(limsr_lcc)[4])

# Create map objects
richness11_lcc <- ggplot() +
  geom_spatvector(data = states_lcc, color = NA, fill = "white") +
  geom_spatraster(data = rich11_lcc, maxcell = Inf) +
  scale_fill_gradientn(colors = rich_cols, na.value = NA,
                       breaks = c(0, 2, 4, 6), limits = c(0, 7)) +
  geom_spatvector(data = states_lcc, color = "gray50", linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = filter(countries_lcc, 
                                countries_lcc$adm0_a3 %in% c("USA", "CAN", "MEX")),
                  color = "black", linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr_lcc, ylim = ylimr_lcc,
           expand = FALSE) +
  theme_bw() +
  ylab("SSP245") +
  theme(plot.margin = unit(margins, "pt"),
        legend.spacing.y = unit(10, 'pt'))
richness21_lcc <- ggplot() +
  geom_spatvector(data = states_lcc, color = NA, fill = "white") +
  geom_spatraster(data = rich21_lcc, maxcell = Inf) +
  scale_fill_gradientn(colors = rich_cols, na.value = NA,
                       breaks = c(0, 2, 4, 6), limits = c(0, 7)) +
  geom_spatvector(data = states_lcc, color = "gray50", linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = filter(countries_lcc, 
                                countries_lcc$adm0_a3 %in% c("USA", "CAN", "MEX")),
                  color = "black", linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr_lcc, ylim = ylimr_lcc,
           expand = FALSE) +
  theme_bw() +
  ylab("SSP370") +
  theme(plot.margin = unit(margins, "pt"),
        legend.spacing.y = unit(10, 'pt'))
richness31_lcc <- ggplot() +
  geom_spatvector(data = states_lcc, color = NA, fill = "white") +
  geom_spatraster(data = rich31_lcc, maxcell = Inf) +
  scale_fill_gradientn(colors = rich_cols, na.value = NA,
                       breaks = c(0, 2, 4, 6), limits = c(0, 7)) +
  geom_spatvector(data = states_lcc, color = "gray50", linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = filter(countries_lcc, 
                                countries_lcc$adm0_a3 %in% c("USA", "CAN", "MEX")),
                  color = "black", linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr_lcc, ylim = ylimr_lcc,
           expand = FALSE) +
  theme_bw() +
  ylab("SSP585") +
  theme(plot.margin = unit(margins, "pt"),
        legend.spacing.y = unit(10, 'pt'))
richness12_lcc <- ggplot() +
  geom_spatvector(data = states_lcc, color = NA, fill = "white") +
  geom_spatraster(data = rich12_lcc, maxcell = Inf) +
  scale_fill_gradientn(colors = rich_cols, na.value = NA,
                       breaks = c(0, 2, 4, 6), limits = c(0, 7)) +
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
richness22_lcc <- ggplot() +
  geom_spatvector(data = states_lcc, color = NA, fill = "white") +
  geom_spatraster(data = rich22_lcc, maxcell = Inf) +
  scale_fill_gradientn(colors = rich_cols, na.value = NA,
                       breaks = c(0, 2, 4, 6), limits = c(0, 7)) +
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
richness32_lcc <- ggplot() +
  geom_spatvector(data = states_lcc, color = NA, fill = "white") +
  geom_spatraster(data = rich32_lcc, maxcell = Inf) +
  scale_fill_gradientn(colors = rich_cols, na.value = NA,
                       breaks = c(0, 2, 4, 6), limits = c(0, 7)) +
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

richness_for_legend <- ggplot() +
  geom_spatraster(data = rich11_lcc, maxcell = Inf) +
  scale_fill_gradientn(colors = rich_cols, na.value = NA, name = "Richness",
                       breaks = c(0, 2, 4, 6), limits = c(0, 7)) +
  theme(legend.position = "bottom")
legend_r <- get_leg(richness_for_legend)

# Combine things
rich6 <- plot_grid(richness11_lcc + theme(legend.position = "none"),
                   richness21_lcc + theme(legend.position = "none"),
                   richness31_lcc + theme(legend.position = "none"),
                   richness12_lcc + theme(legend.position = "none"),
                   richness22_lcc + theme(legend.position = "none"),
                   richness32_lcc + theme(legend.position = "none"),
                   align = "vh",
                   nrow = 3,
                   byrow = FALSE)
col_titles <- ggdraw() + 
  draw_label("2041-2070", size = 12, x = 0.29) +
  draw_label("2071-2100", size = 12, x = 0.79)

p <- plot_grid(col_titles, rich6, legend_r,
               ncol = 1, rel_heights = c(0.2, 8.6, 0.4))

plot_name <- paste0(output_basename, "richness-lcc-6panel.png")
if (!file.exists(plot_name) | (file.exists(plot_name) & replace)) {
  ggsave(filename = plot_name,
         plot = p,
         width = 6.5,
         height = 9,
         units = "in")
}

# Creating change in richness (delta) figures ---------------------------------#

# Type of map 
# ov = insect distributed only where one or more host also occurs
# io = insect distribution depends only on climate)
type <- "ov"

# Load delta rasters
rdelta11 <- readRDS(paste0("output/richness/ensemble_ssp245_2041-delta-richness-",
                           type, ".rds"))
rdelta21 <- readRDS(paste0("output/richness/ensemble_ssp370_2041-delta-richness-",
                           type, ".rds"))
rdelta31 <- readRDS(paste0("output/richness/ensemble_ssp585_2041-delta-richness-",
                           type, ".rds"))
rdelta12 <- readRDS(paste0("output/richness/ensemble_ssp245_2071-delta-richness-",
                           type, ".rds"))
rdelta22 <- readRDS(paste0("output/richness/ensemble_ssp370_2071-delta-richness-",
                           type, ".rds"))
rdelta32 <- readRDS(paste0("output/richness/ensemble_ssp585_2071-delta-richness-",
                           type, ".rds"))

# Define color palette
delta_cols <- c("#D10000","#104e8b")
graycol <- "#f2f2f2"

# Few plotting parameters
margins <- c(2, 0, 6, 0)
linewidth <- 0.1

# Project maps using Lambert Conformal Conic North America. Croping the eastern
# edge of states and countries layers. Also note that when plotting, we're 
# removing boundaries for all countries except the US, Canada, and Mexico 
# because with this projection that is centered in the US, country boundaries 
# become very distorted for Greenland and Central America.
state_ext <- ext(states)
state_ext[2] <- -45
states <- crop(states, state_ext)
countries <- crop(countries, state_ext)
states_lcc <- project(states, "ESRI:102009")
countries_lcc <- project(countries, crs(states_lcc))
rdelta11_lcc <- project(rdelta11, crs(states_lcc))
rdelta21_lcc <- project(rdelta21, crs(states_lcc))
rdelta31_lcc <- project(rdelta31, crs(states_lcc))
rdelta12_lcc <- project(rdelta12, crs(states_lcc))
rdelta22_lcc <- project(rdelta22, crs(states_lcc))
rdelta32_lcc <- project(rdelta32, crs(states_lcc))

# Get rid of NA values and calculate extent for plot area
rdelta11_lcc <- drop_na(rdelta11_lcc)
limsr_lcc <- ext(rdelta11_lcc) * 1.01
xlimr_lcc <- c(ext(limsr_lcc)[1], ext(limsr_lcc)[2])
ylimr_lcc <- c(ext(limsr_lcc)[3], ext(limsr_lcc)[4])

# Create map objects
rdelta11_plot <- ggplot() +
  geom_spatvector(data = states_lcc, color = NA, fill = "white") +
  geom_spatraster(data = rdelta11_lcc, maxcell = Inf) +
  scale_fill_gradient2(low = delta_cols[1], high = delta_cols[2], 
                       mid = graycol, na.value = NA, 
                       breaks = c(-5, 0, 5), limits = c(-6, 6)) +
  geom_spatvector(data = states_lcc, color = "gray50", linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = filter(countries_lcc, 
                                countries_lcc$adm0_a3 %in% c("USA", "CAN", "MEX")),
                  color = "black", linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr_lcc, ylim = ylimr_lcc,
           expand = FALSE) +
  theme_bw() +
  ylab("SSP245") +
  theme(plot.margin = unit(margins, "pt"),
        legend.spacing.y = unit(10, 'pt'))
rdelta21_plot <- ggplot() +
  geom_spatvector(data = states_lcc, color = NA, fill = "white") +
  geom_spatraster(data = rdelta21_lcc, maxcell = Inf) +
  scale_fill_gradient2(low = delta_cols[1], high = delta_cols[2], 
                       mid = graycol, na.value = NA, 
                       breaks = c(-5, 0, 5), limits = c(-6, 6)) +
  geom_spatvector(data = states_lcc, color = "gray50", linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = filter(countries_lcc, 
                                countries_lcc$adm0_a3 %in% c("USA", "CAN", "MEX")),
                  color = "black", linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr_lcc, ylim = ylimr_lcc,
           expand = FALSE) +
  theme_bw() +
  ylab("SSP370") +
  theme(plot.margin = unit(margins, "pt"),
        legend.spacing.y = unit(10, 'pt'))
rdelta31_plot <- ggplot() +
  geom_spatvector(data = states_lcc, color = NA, fill = "white") +
  geom_spatraster(data = rdelta31_lcc, maxcell = Inf) +
  scale_fill_gradient2(low = delta_cols[1], high = delta_cols[2], 
                       mid = graycol, na.value = NA, 
                       breaks = c(-5, 0, 5), limits = c(-6, 6)) +
  geom_spatvector(data = states_lcc, color = "gray50", linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = filter(countries_lcc, 
                                countries_lcc$adm0_a3 %in% c("USA", "CAN", "MEX")),
                  color = "black", linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlimr_lcc, ylim = ylimr_lcc,
           expand = FALSE) +
  theme_bw() +
  ylab("SSP585") +
  theme(plot.margin = unit(margins, "pt"),
        legend.spacing.y = unit(10, 'pt'))
rdelta12_plot <- ggplot() +
  geom_spatvector(data = states_lcc, color = NA, fill = "white") +
  geom_spatraster(data = rdelta12_lcc, maxcell = Inf) +
  scale_fill_gradient2(low = delta_cols[1], high = delta_cols[2], 
                       mid = graycol, na.value = NA, 
                       breaks = c(-5, 0, 5), limits = c(-6, 6)) +
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
rdelta22_plot <- ggplot() +
  geom_spatvector(data = states_lcc, color = NA, fill = "white") +
  geom_spatraster(data = rdelta22_lcc, maxcell = Inf) +
  scale_fill_gradient2(low = delta_cols[1], high = delta_cols[2], 
                       mid = graycol, na.value = NA, 
                       breaks = c(-5, 0, 5), limits = c(-6, 6)) +
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
rdelta32_plot <- ggplot() +
  geom_spatvector(data = states_lcc, color = NA, fill = "white") +
  geom_spatraster(data = rdelta32_lcc, maxcell = Inf) +
  scale_fill_gradient2(low = delta_cols[1], high = delta_cols[2], 
                       mid = graycol, na.value = NA, 
                       breaks = c(-5, 0, 5), limits = c(-6, 6)) +
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

rdelta_for_legend <- ggplot() +
  geom_spatraster(data = rdelta11_lcc, maxcell = Inf) +
  scale_fill_gradient2(low = delta_cols[1], high = delta_cols[2], 
                       mid = graycol, na.value = NA, name = "Change",
                       breaks = c(-5, 0, 5), limits = c(-6, 6)) +
  theme(legend.position = "bottom")
legend_rdelta <- get_leg(rdelta_for_legend)

# Combine things
rdelta6 <- plot_grid(rdelta11_plot + theme(legend.position = "none"),
                     rdelta21_plot + theme(legend.position = "none"),
                     rdelta31_plot + theme(legend.position = "none"),
                     rdelta12_plot + theme(legend.position = "none"),
                     rdelta22_plot + theme(legend.position = "none"),
                     rdelta32_plot + theme(legend.position = "none"),
                     align = "vh",
                     nrow = 3,
                     byrow = FALSE)
col_titles <- ggdraw() + 
  draw_label("2041-2070", size = 12, x = 0.29) +
  draw_label("2071-2100", size = 12, x = 0.79)

p <- plot_grid(col_titles, rdelta6, legend_rdelta,
               ncol = 1, rel_heights = c(0.2, 8.6, 0.4))

plot_name <- paste0(output_basename, "delta-richness-lcc-6panel.png")
if (!file.exists(plot_name) | (file.exists(plot_name) & replace)) {
  ggsave(filename = plot_name,
         plot = p,
         width = 6.5,
         height = 9,
         units = "in")
}
