# Create lollipop plots of individual species' range changes, noting east/west 
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-10-07

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot) # For joining lat & long lollipops into one figure
source(file = "functions/get_colors.R")

# Plots based on the output of src/summary/summary-2-compare-ranges.R, which is 
# written to output/summary-stats/overlap-summary-allspp.csv

# lat_max_shift: median value of shifts (km) along northern edge in each 
#  longitudinal band (positive values = northward shift; negative values = 
#   southward shift)
# lat_min_shift: median value of shifts (km) along southern edge in each 
#   longitudinal band (pos values = northward; neg values = southward)
# lon_max_shift: median value of shifts (km) along eastern edge in each 
#   latitudinal band (positive values = eastward shift; negative values = 
#   westward shift)
# lon_min_shift: median value of shifts (km) along western edge in each 
#   latitudinal band (pos values = eastward; neg values = westward)
# lat_max_05_shift: shift (deg) in median values of northernmost 5% of cells
#   (positive values = northward shift; negative values = southward shift)
# lat_min_05_shift: shift (deg) in median values of southernmost 5% of cells 
#   (pos values = northward; neg values = southward)
# lon_max_05_shift: shift (deg) in median values of easternmost 5% of cells
#   (positive values = eastward shift; negative values = westward shift)
# lon_min_05_shift: shift (deg) in median values of westernmost 5% of cells
#   (pos values = eastward; neg values = westward)

# We chose one or the other of above for our plots
use_05_percent <- TRUE # if false, we use the shifts based on lat/lon bands
var_suffix <- ifelse(use_05_percent, "_05_shift", "_shift")
# More elegant way exists for sure
lat_min_var <- paste0("lat_min", var_suffix)
lat_max_var <- paste0("lat_max", var_suffix)
lon_min_var <- paste0("lon_min", var_suffix)
lon_max_var <- paste0("lon_max", var_suffix)

# Grab the info for all the species and models
range_info <- read.csv(file = "output/summary-stats/overlap-summary-allspp.csv")
# Read in east/west information
ew <- read.csv(file = "data/insect-eastwest.csv")
# Figure-friendly metadata on climate models for plotting
climate_models <- read.csv(file = "data/climate-models.csv") %>%
  mutate(name = gsub(pattern = "ensemble_", replacement = "", x = name)) %>%
  filter(name != "current") %>%
  select(name, description, yr, ssp, yr_midpoint, yr_text, ssp_text)

# Restrict to data where insect suitability includes overlap with area suitable 
# for at least one host plant
range_info <- range_info %>%
  filter(distribution == "insect + host") %>%
  select(-distribution)

# Need to drop "current" climate rows and add climate/year data more ameanable to 
# figures
range_info <- range_info %>%
  filter(climate != "current") %>%
  left_join(climate_models, by = c("climate" = "name"))

# Now add in east/west information for coloring
range_info <- range_info %>%
  left_join(ew, by = join_by(insect))

# From here on out, the species names should have the genus abbreviated
range_info <- range_info %>%
  mutate(insect = gsub(pattern = "Papilio", 
                       replacement = "P.", 
                       x = insect))


ew_colors <- get_colors(palette = "eastwest")
# Need to make title case to align with data in range_info
names(ew_colors) <- tools::toTitleCase(names(ew_colors))

# In subsequent dplyr & ggplot2 functions, because we are using a character 
# string to indicate which column to use for our edge variable (e.g. 
# "lat_min_shift" or "lat_min_05_shift" is stored in the variable lat_min_var), 
# we use the funky .data[[var_name]] syntax. See 
# https://rlang.r-lib.org/reference/args_data_masking.html for more info (and 
# note that the {{ var_name }} syntax does not work - that seems restricted to 
# use when var_name is a parameter passed to a custom function).

# TODO: Could remove appalachiensis and palamedes from shift data...(filtering 
# in ggplot call removes the former, but not the latter, which only has non-NA 
# values for SSP245-2055)
# Southern (lat_min)
southern_lollipop <- ggplot(data = range_info %>%
                              filter(!is.na(.data[[lat_min_var]])), 
                            mapping = aes(y = insect,
                                          color = ew)) +
  geom_point(mapping = aes(x = .data[[lat_min_var]])) +
  geom_segment(mapping = aes(x = 0, xend = .data[[lat_min_var]])) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5) +
  scale_color_manual(name = "East/West",
                    values = ew_colors) +
  xlab(label = "Southern edge latitudinal shift (km)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                   hjust = 1, face = "italic"),
        axis.title.x = element_blank(),
        strip.background = element_rect(fill = "#F0F0F0")) +
  coord_flip() +
  facet_grid(ssp_text ~ yr_text)
southern_lollipop

# Northern (lat_max)
northern_lollipop <- ggplot(data = range_info %>%
                              filter(!is.na(.data[[lat_max_var]])), 
                            mapping = aes(y = insect,
                                          color = ew)) +
  geom_point(mapping = aes(x = .data[[lat_max_var]])) +
  geom_segment(mapping = aes(x = 0, xend = .data[[lat_max_var]])) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5) +
  scale_color_manual(name = "East/West",
                     values = ew_colors) +
  xlab(label = "Northern edge latitudinal shift (km)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                   hjust = 1, face = "italic"),
        axis.title.x = element_blank(),
        strip.background = element_rect(fill = "#F0F0F0")) +
  coord_flip() +
  facet_grid(ssp_text ~ yr_text)
northern_lollipop

# For eastern & western plots, need to relevel the names, so they occur in 
# ascending alphabetical order on the y axis (i.e. P. brevicauda on top and 
# P. zelicaon on bottom)

# Western (lon_min)
western_lollipop <- ggplot(data = range_info %>%
                             filter(!is.na(.data[[lon_min_var]])) %>%
                             mutate(insect = factor(insect,
                                                    levels = rev(unique(insect)))), 
                           mapping = aes(y = insect,
                                         color = ew)) +
  geom_point(mapping = aes(x = .data[[lon_min_var]])) +
  geom_segment(mapping = aes(x = 0, xend = .data[[lon_min_var]])) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5) +
  scale_color_manual(name = "East/West",
                     values = ew_colors) +
  xlab(label = "Western edge longitudinal shift (km)") +
  theme_bw() +
  theme(axis.text.y = element_text(face = "italic"),
        strip.background = element_rect(fill = "#F0F0F0"),
        axis.title.y = element_blank()) +
  facet_grid(ssp_text ~ yr_text)
western_lollipop

# Eastern (lon_max)
eastern_lollipop <- ggplot(data = range_info %>%
                             filter(!is.na(.data[[lon_max_var]])) %>%
                             mutate(insect = factor(insect,
                                                    levels = rev(unique(insect)))), 
                           mapping = aes(y = insect,
                                         color = ew)) +
  geom_point(mapping = aes(x = .data[[lon_max_var]])) +
  geom_segment(mapping = aes(x = 0, xend = .data[[lon_max_var]])) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5) +
  scale_color_manual(name = "East/West",
                     values = ew_colors) +
  xlab(label = "Eastern edge longitudinal shift (km)") +
  theme_bw() +
  theme(axis.text.y = element_text(face = "italic"),
        strip.background = element_rect(fill = "#F0F0F0"),
        axis.title.y = element_blank()) +
  facet_grid(ssp_text ~ yr_text)
eastern_lollipop

# Make 
# For now, just saving each lollipop as separate file
ggsave(plot = southern_lollipop, filename = "output/manuscript/shifts/shift-south.png")
ggsave(plot = northern_lollipop, filename = "output/manuscript/shifts/shift-north.png")
ggsave(plot = western_lollipop, filename = "output/manuscript/shifts/shift-west.png")
ggsave(plot = eastern_lollipop, filename = "output/manuscript/shifts/shift-east.png")

# Some paired lollipop plots for a single ssp & year, where longitude is one 
# plot (two facets) and latitude is another plot (two facets).

# Start with data wrangling to get long-formatted data for SSP 3-7.0, 2055
# Remember shift column names are actually stored in variables lat_min_var etc.
range_info_long <- range_info %>%
  select(insect, all_of(c(lat_min_var, lat_max_var, lon_min_var, lon_max_var)),
         ssp, yr, ew) %>%
  filter(ssp == "370", yr == 2041) %>%
  pivot_longer(cols = c(lat_min_var, lat_max_var, lon_min_var, lon_max_var),
               names_to = "shift",
               values_to = "value")

# We can drop P. appalachiensis & P. palamedes 
range_info_long <- range_info_long  %>% 
  filter(!(insect %in% c("P. appalachiensis", "P. palamedes")))

# Also want to re-level so east groups with east etc.
range_info_long <- range_info_long %>%
  arrange(desc(ew), insect) %>%
  mutate(insect = factor(x = insect,
                         levels = unique(insect)))

# We can go ahead and update values in shift column to make things easier later 
# on. This really just involves removing a "_05" from the string. We do this 
# because subsequent code is looking for strings "lat_min_shift", 
# "lat_max_shift", "lon_min_shift", and "lon_max_shift"
range_info_long <- range_info_long %>%
  mutate(shift = gsub(pattern = "_05",
                      replacement = "", 
                      x = shift))

# Make named vector for labels
facet_labels <- c("lon_min_shift" = "Western edge",
                  "lon_max_shift" = "Eastern edge",
                  "lat_min_shift" = "Southern edge",
                  "lat_max_shift" = "Northern edge")

# Longitude plots (west shifts on left, east shifts on right); need to re-level
# species for ascending alphabetical order on y-axis
longitude_lollipop <- ggplot(data = range_info_long %>%
                             filter(shift %in% c("lon_min_shift", "lon_max_shift")) %>%
                               mutate(shift = factor(x = shift,
                                                     levels = c("lon_min_shift",
                                                                "lon_max_shift"))) %>%
                               mutate(insect = factor(insect,
                                                      levels = rev(unique(insect)))),
                             mapping = aes(y = insect, color = ew)) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5) +
  geom_point(mapping = aes(x = value), size = 2.5) +
  geom_segment(mapping = aes(x = 0, xend = value)) +
  scale_color_manual(name = "East/West",
                     values = ew_colors) +
  xlab("Longitudinal shift (km)") +
  ylab("Species") +
  facet_wrap(~ shift, 
             nrow = 1,
             ncol = 2,
             # scales = "free_x", 
             labeller = as_labeller(facet_labels)) +
  theme_bw() +
  theme(axis.text.y = element_text(face = "italic", size = 10),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.position = "inside",
        legend.position.inside = c(0.1, 0.85))
longitude_lollipop
ggsave(file = "output/manuscript/Figure-Shifts-Longitude.png",
       plot = longitude_lollipop)

# Latitude plots (north shifts on top, south shifts on bottom)
latitude_lollipop <- ggplot(data = range_info_long %>% 
                               filter(shift %in% c("lat_min_shift", "lat_max_shift")) %>%
                               mutate(shift = factor(x = shift,
                                                     levels = c("lat_max_shift",
                                                                "lat_min_shift"))),
                             mapping = aes(y = insect,
                                           color = ew)) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5) +
  geom_point(mapping = aes(x = value), size = 2.5) +
  geom_segment(mapping = aes(x = 0, xend = value)) +
  scale_color_manual(name = "East/West",
                     values = ew_colors) +
  xlab("Latitudinal shift (km)") +
  # ylab("Species") +
  facet_wrap(~ shift, 
             nrow = 2,
             ncol = 1,
             # scales = "free_y", 
             labeller = as_labeller(facet_labels)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, 
                                   hjust = 1, face = "italic", size = 10),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.position = "inside",
        legend.position.inside = c(0.1, 0.85)) +
  coord_flip()
latitude_lollipop
ggsave(file = "output/manuscript/Figure-Shifts-Latitude.png",
       plot = latitude_lollipop)

#####
# Single png with both longitude and latitude lollipop shifts
# Remove legend from latitude because it doesn't fit
latitude_lollipop <- latitude_lollipop +
  theme(legend.position = "none")

lollipop_shifts <- cowplot::plot_grid(plotlist = list(longitude_lollipop,
                                                      latitude_lollipop),
                                      labels = "auto",
                                      ncol = 1,
                                      vjust = 1, 
                                      hjust = 0)
ggsave(file = "output/manuscript/Figure-Shifts.png",
       plot = lollipop_shifts,
       width = 6,
       height = 8,
       units = "in")
