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

# Creating overlap maps -------------------------------------------------------#

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
    scale_x_continuous(breaks = c(-120, -110, -100)) +
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
    scale_x_continuous(breaks = c(-120, -110, -100)) +
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
    scale_x_continuous(breaks = c(-120, -110, -100)) +
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
    scale_x_continuous(breaks = c(-120, -110, -100)) +
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
    scale_x_continuous(breaks = c(-120, -110, -100)) +
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
    scale_x_continuous(breaks = c(-120, -110, -100)) +
    theme_bw() +
    theme(plot.margin = unit(margins, "pt"), 
          axis.text = element_text(size = 10))
  
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