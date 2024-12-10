# Create lollipop plots of individual species' range changes, noting east/west 
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-10-07

library(dplyr)
library(tidyr)
library(ggplot2)
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



  # mutate(ssp = substr(climate, start = 4, stop = 6),
  #        year = substr(climate, start = 8, stop = 11)) %>%
  # Create year information for facet labels
  #     2041 -> 2050s
  #     2071 -> 2080s
  # mutate(yr_text = if_else(year == 2041, 
  #                            true = "2050s", 
  #                            false = "2080s")) %>%
  # Create cleaner ssp labels
  # mutate(ssp_text = paste0("SSP",
  #                          substr(ssp, start = 1, stop = 1),
  #                          "-",
  #                          substr(ssp, start = 2, stop = 2),
  #                          ".",
  #                          substr(ssp, start = 3, stop = 3)))

# Now add in east/west information for coloring
range_info <- range_info %>%
  left_join(ew)

# From here on out, the species names should have the genus abbreviated
range_info <- range_info %>%
  mutate(insect = gsub(pattern = "Papilio", 
                       replacement = "P.", 
                       x = insect))


ew_colors <- get_colors(palette = "eastwest")
# Need to make title case to align with data in range_info
names(ew_colors) <- tools::toTitleCase(names(ew_colors))

# TODO: Could remove appalachiensis and palamedes from shift data...(filtering 
# in ggplot call removes the former, but not the latter, which only has non-NA 
# values for SSP245-2055)
# Southern (lat_min)
southern_lollipop <- ggplot(data = range_info %>%
                              filter(!is.na(lat_min_shift)), 
                            mapping = aes(y = insect,
                                          color = ew)) +
  geom_point(mapping = aes(x = lat_min_shift)) +
  geom_segment(mapping = aes(x = 0, xend = lat_min_shift)) +
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
                              filter(!is.na(lat_max_shift)), 
                            mapping = aes(y = insect,
                                          color = ew)) +
  geom_point(mapping = aes(x = lat_max_shift)) +
  geom_segment(mapping = aes(x = 0, xend = lat_max_shift)) +
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
                             filter(!is.na(lon_min_shift)) %>%
                             mutate(insect = factor(insect,
                                                    levels = rev(unique(insect)))), 
                           mapping = aes(y = insect,
                                         color = ew)) +
  geom_point(mapping = aes(x = lon_min_shift)) +
  geom_segment(mapping = aes(x = 0, xend = lon_min_shift)) +
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
                             filter(!is.na(lon_max_shift)) %>%
                             mutate(insect = factor(insect,
                                                    levels = rev(unique(insect)))), 
                           mapping = aes(y = insect,
                                         color = ew)) +
  geom_point(mapping = aes(x = lon_max_shift)) +
  geom_segment(mapping = aes(x = 0, xend = lon_max_shift)) +
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
# For now, just saving each lollipop as separate file; can assemble a 
# multi-page pdf with ghostscript, e.g. from within output/plots, run in bash 
# and note command to suppress page rotation:
# gs -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -dAutoRotatePages=/None \
#    -sOUTPUTFILE=Figure-Supplemental-Shifts.pdf \
#    shift-south.pdf shift-north.pdf shift-west.pdf shift-east.pdf
ggsave(plot = southern_lollipop, filename = "output/plots/shift-south.pdf")
ggsave(plot = northern_lollipop, filename = "output/plots/shift-north.pdf")
ggsave(plot = western_lollipop, filename = "output/plots/shift-west.pdf")
ggsave(plot = eastern_lollipop, filename = "output/plots/shift-east.pdf")

# Some paired lollipop plots for a single ssp & year, where longitude is one 
# plot (two facets) and latitude is another plot (two facets).

# Start with data wrangling to get long-formatted data for SSP 3-7.0, 2055
range_info_long <- range_info %>%
  select(insect, lat_min_shift, lat_max_shift, lon_min_shift, lon_max_shift,
         ssp, yr, ew) %>%
  filter(ssp == "370", yr == 2041) %>%
  pivot_longer(cols = c(lat_min_shift, lat_max_shift, lon_min_shift, lon_max_shift),
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
  geom_point(mapping = aes(x = value), size = 4) +
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
        legend.position = "inside",
        legend.position.inside = c(0.1, 0.1))
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
  geom_point(mapping = aes(x = value), size = 4) +
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
        legend.position = "inside",
        legend.position.inside = c(0.1, 0.9)) +
  coord_flip()
latitude_lollipop
ggsave(file = "output/manuscript/Figure-Shifts-Latitude.png",
       plot = latitude_lollipop)
