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
  
  color_vec_o <- c("#e8e8e8",   # Absent
                   "#89cf72",   # Hosts only
                   "#90c3de",   # Insect only
                   "#8f5cd6")   # Hosts and insect
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
  
  color_vec_d <- c("#e8e8e8",   # Absent
                   "#f0b041",   # Loss
                   "#2f6cc2",   # Gain
                   "#f6e8c3")   # Stable
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
