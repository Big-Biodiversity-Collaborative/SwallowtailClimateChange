# Create plots of individual species' range changes, noting east/west 
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-10-07

library(dplyr)
library(tidyr)
library(ggplot2)

# Plot histograms of (all from the output of 
# src/summary/summary-2-compare-ranges.R, which is written to 
# output/summary-stats/overlap-summary-allspp.csv)

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
# area_gained: area (sqkm) predicted suitable in future that wasn't in current range
# area_lost: area (sqkm) predicted unsuitable in future that was in current range
# area_retained: area (sqkm) predicted suitable in future that was in current range
# Note: Can calculate % of current range that's still suitable in future as 
# area_retained/(area retained + area lost)

range_info <- read.csv(file = "output/summary-stats/overlap-summary-allspp.csv")

# Restrict to data where insect suitability includes overlap with area suitable 
# for at least one host plant
range_info <- range_info %>%
  filter(distribution == "insect + host") %>%
  select(-distribution)

# Need to drop "current" climate rows and separate climate into year 
# and ssp columns
range_info <- range_info %>%
  filter(climate != "current") %>%
  mutate(ssp = substr(climate, start = 4, stop = 6),
         year = substr(climate, start = 8, stop = 11)) %>%
  # Create year information for facet labels
  #     2041 -> 2050s
  #     2071 -> 2080s
  mutate(year_text = if_else(year == 2041, 
                             true = "2050s", 
                             false = "2080s")) %>%
  # Create cleaner ssp labels
  mutate(ssp_text = paste0("SSP",
                           substr(ssp, start = 1, stop = 1),
                           "-",
                           substr(ssp, start = 2, stop = 2),
                           ".",
                           substr(ssp, start = 3, stop = 3)))

# Add the percent of current range retained to the data frame
range_info <- range_info %>%
  mutate(perc_retained = 100 * (area_retained/(area_retained + area_lost)))
# Total change in area (area_retained + area_gained) - (area_retained + area_lost)
#   reduces to area_gained - area_lost
range_info <- range_info %>%
  mutate(area_delta = area_gained - area_lost)
# Change in area relative to current range (area_retained + area_lost)
range_info <- range_info %>% 
  mutate(perc_delta = (100 * ((area_gained + area_retained)/(area_lost + area_retained))) - 100)

# Now add in east/west information for coloring
ew <- read.csv(file = "data/insect-eastwest.csv")
range_info <- range_info %>%
  left_join(ew)

# From here on out, the species names should have the genus abbreviated
range_info <- range_info %>%
  mutate(insect = gsub(pattern = "Papilio", 
                       replacement = "P.", 
                       x = insect))

ew_colors <- c("#3eafa3", "#ce932a")

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
  facet_grid(ssp_text ~ year_text)
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
  facet_grid(ssp_text ~ year_text)
northern_lollipop

# Western (lon_min)
western_lollipop <- ggplot(data = range_info %>%
                             filter(!is.na(lon_min_shift)), 
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
  facet_grid(ssp_text ~ year_text)
western_lollipop

# Eastern (lon_max)
eastern_lollipop <- ggplot(data = range_info %>%
                             filter(!is.na(lon_max_shift)), 
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
  facet_grid(ssp_text ~ year_text)
eastern_lollipop

# For now, just saving each lollipop as separate file
ggsave(plot = southern_lollipop, filename = "output/plots/shift-south.png")
ggsave(plot = northern_lollipop, filename = "output/plots/shift-north.png")
ggsave(plot = western_lollipop, filename = "output/plots/shift-west.png")
ggsave(plot = eastern_lollipop, filename = "output/plots/shift-east.png")

# Some paired lollipop plots for a single ssp & year, where longitude is one 
# plot (two facets) and latitude is another plot (two facets).

facet_labels <- c("lon_min_shift" = "Western edge",
                  "lon_max_shift" = "Eastern edge",
                  "lat_min_shift" = "Southern edge",
                  "lat_max_shift" = "Northern edge")

# Start with data wrangling to get long-formatted data for SSP 3-7.0, 2055
range_info_long <- range_info %>%
  select(insect, lat_min_shift, lat_max_shift, lon_min_shift, lon_max_shift,
         ssp, year, ew) %>%
  filter(ssp == "370", year == 2055) %>%
  pivot_longer(cols = c(lat_min_shift, lat_max_shift, lon_min_shift, lon_max_shift),
               names_to = "shift",
               values_to = "value")

# We can drop P. appalachiensis & P. palamedes 
range_info_long <- range_info_long  %>% 
  filter(!(insect %in% c("P. appalachiensis", "P. palamedes")))

# Also want to re-level so east groups with east etc.
range_info_long <- range_info_long %>%
  arrange(ew, insect) %>%
  mutate(insect = factor(x = insect,
                         levels = rev(unique(insect))))

# Longitude plots (west shifts on left, east shifts on right)
longitude_lollipop <- ggplot(data = range_info_long %>%
                             filter(shift %in% c("lon_min_shift", "lon_max_shift")) %>%
                               mutate(shift = factor(x = shift,
                                                     levels = c("lon_min_shift",
                                                                "lon_max_shift"))),
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
        strip.background = element_blank())
longitude_lollipop
ggsave(file = "output/plots/lon-shift-ssp370_2041.png",
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
        strip.background = element_blank()) +
  coord_flip()
latitude_lollipop
ggsave(file = "output/plots/lat-shift-ssp370_2041.png",
       plot = latitude_lollipop)


################################################################################
# Area changes plots
area_long <- range_info %>%
  filter(ssp == "370", year == 2055) %>%
  select(insect, area_gained, area_lost, area_retained, area_delta, perc_delta, ew) %>%
  pivot_longer(-c(insect, ew),
               names_to = "change",
               values_to = "area_km") %>%
  filter(!(insect %in% c("P. appalachiensis", "P. palamedes"))) %>%
  arrange(ew, insect) %>%
  mutate(insect = factor(x = insect,
                         levels = rev(unique(insect))))

# Area changes points, where the size of the circle indicates how much area was 
# lost, retained, or gained. Not sure if it works.
changes_points <- ggplot(data = area_long %>%
                           filter(change %in% c("area_lost", 
                                                "area_retained", 
                                                "area_gained",
                                                "area_delta")) %>%
                           mutate(change = factor(x = change,
                                                  levels = c("area_lost", 
                                                             "area_retained", 
                                                             "area_gained",
                                                             "area_delta"))), 
                         mapping = aes(x = insect,
                                       y = change,
                                       size = area_km,
                                       color = ew)) +
  geom_point() +
  scale_color_manual(name = "East/West",
                     values = ew_colors) +
  theme_bw() +
  coord_flip()
changes_points

# We have delta_area, could just do plot of that. The faceted plot works OK, 
# it might be better to label the points with the perc_delta value
delta_points <- ggplot(data = area_long %>%
                         filter(change %in% c("area_delta", "perc_delta")),
                       mapping = aes(x = insect, 
                                     y = area_km,
                                     color = ew)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.5) +
  geom_point(size = 4) +
  scale_color_manual(name = "East/West",
                     values = ew_colors) +
  coord_flip() +
  theme_bw() +
  facet_grid(~ change, scale = "free_x") +
  ylab(label = bquote("Change in Area"~(km^2))) +
  theme(axis.text.y = element_text(face = "italic", size = 10),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())
delta_points

# Area gained 
gained <- ggplot(data = range_info, mapping = aes(x = area_gained,
                                                  group = ew,
                                                  fill = ew)) +
  geom_histogram(position = "dodge", bins = 20) +
  scale_fill_manual(name = "East/West",
                    values = ew_colors) +
  xlab(label = bquote("Area gained"~(km^2))) +
  ylab(label = expression(paste("# ", italic("Papilio"), " species"))) +
  facet_grid(ssp ~ year) +
  theme_bw()
gained

# Area lost
lost <- ggplot(data = range_info, mapping = aes(x = area_lost,
                                                group = ew,
                                                fill = ew)) +
  geom_histogram(position = "dodge", bins = 20) +
  scale_fill_manual(name = "East/West",
                    values = ew_colors) +
  xlab(label = bquote("Area lost"~(km^2))) +
  ylab(label = expression(paste("# ", italic("Papilio"), " species"))) +
  facet_grid(ssp ~ year) +
  theme_bw()
lost

# Area retained
retained <- ggplot(data = range_info, mapping = aes(x = area_retained,
                                                    group = ew,
                                                    fill = ew)) +
  geom_histogram(position = "dodge", bins = 20) +
  scale_fill_manual(name = "East/West",
                    values = ew_colors) +
  xlab(label = bquote("Area retained"~(km^2))) +
  ylab(label = expression(paste("# ", italic("Papilio"), " species"))) +
  facet_grid(ssp ~ year) +
  theme_bw()
retained

# Percent contemporary retained
retained_perc <- ggplot(data = range_info, mapping = aes(x = perc_retained,
                                                         group = ew,
                                                         fill = ew)) +
  geom_histogram(position = "dodge", bins = 20) +
  scale_fill_manual(name = "East/West",
                    values = ew_colors) +
  xlab(label = "Percent area retained") +
  ylab(label = expression(paste("# ", italic("Papilio"), " species"))) +
  facet_grid(ssp ~ year) +
  theme_bw()
retained_perc

# Total change in area
delta_area <- ggplot(data = range_info, mapping = aes(x = area_delta,
                                                      group = ew,
                                                      fill = ew)) +
  geom_histogram(position = "dodge", bins = 10) +
  geom_vline(xintercept = 0, lty = 2, lwd = 0.5) +
  scale_fill_manual(name = "East/West",
                    values = ew_colors) +
  xlab(label = bquote("Change in area"~(km^2))) +
  ylab(label = expression(paste("# ", italic("Papilio"), " species"))) +
  facet_grid(ssp ~ year) +
  theme_bw()
delta_area

# Percentage change in area
delta_perc <- ggplot(data = range_info, mapping = aes(x = perc_delta,
                                                      group = ew,
                                                      fill = ew)) +
  geom_histogram(position = "dodge") +
  scale_fill_manual(name = "East/West",
                    values = ew_colors) +
  xlab(label = "Relative change in area (%)") +
  ylab(label = expression(paste("# ", italic("Papilio"), " species"))) +
  facet_grid(ssp ~ year) +
  theme_bw()
delta_perc
