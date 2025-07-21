# Currently unused plots showing changes in area
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-10-07

library(dplyr)
library(tidyr)
library(ggplot2)
source(file = "functions/get_colors.R")

# Plots based on the output of src/summary/summary-2-compare-ranges.R, which is 
# written to output/summary-stats/overlap-summary-allspp.csv

# area_gained: area (sqkm) predicted suitable in future that wasn't in current 
#   range
# area_lost: area (sqkm) predicted unsuitable in future that was in current 
#   range
# area_retained: area (sqkm) predicted suitable in future that was in current 
#   range
# Note: Can calculate % of current range that's still suitable in future as 
# area_retained/(area retained + area lost)

range_info <- read.csv(file = "output/summary-stats/overlap-summary-allspp.csv")
model_info <- read.csv(file = "data/climate-models.csv") %>%
  mutate(name = gsub(pattern = "ensemble_",
                     replacement = "",
                     x = name))

# Restrict to data where insect suitability includes overlap with area suitable 
# for at least one host plant
range_info <- range_info %>%
  filter(distribution == "insect + host") %>%
  select(-distribution)

# Add model info, mostly for text strings to use for plots
range_info <- range_info %>%
  filter(climate != "current") %>%
  left_join(model_info, by = c("climate" = "name"))

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
  left_join(ew, by = join_by(insect)) %>%
  mutate(ew = tolower(ew)) # a hack for dealing with color names

# From here on out, the species names should have the genus abbreviated
range_info <- range_info %>%
  mutate(insect = gsub(pattern = "Papilio", 
                       replacement = "P.", 
                       x = insect))

# What is change in *current* suitable area (and only current, so we ignore any 
# gains of suitable area)
range_info <- range_info %>%
  mutate(perc_current = (area_retained/(area_lost + area_retained)) * 100)

ew_colors <- get_colors(palette = "eastwest")

################################################################################
# Area changes plots
area_long <- range_info %>%
  filter(climate == "ssp370_2041") %>%
  select(insect, area_gained, area_lost, area_retained, area_delta, 
         perc_delta, perc_current, ew) %>%
  pivot_longer(-c(insect, ew),
               names_to = "change",
               values_to = "area_km") %>%
  # filter(!(insect %in% c("P. appalachiensis", "P. palamedes"))) %>%
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

# TODO: This is the plot to use. May still combine by just using area, and 
# labelling points with the percentage change
# bquote("Area gained"~(km^2))
facet_names <- c("area_delta" = "Change in total suitable area (1K km^2)",
                 "perc_delta" = "Change in total suitable area (%)",
                 "perc_current" = "Currently suitable area retained (%)")

# We have delta_area, could just do plot of that. The faceted plot works OK, 
# it might be better to label the points with the perc_delta value
# Note just about everything (points, text) looks too small on screen but is 
# sized for output file
delta_points <- ggplot(data = area_long %>%
                         filter(change %in% c("area_delta", "perc_delta", "perc_current")) %>%
                         mutate(hline = if_else(change == "perc_current",
                                                true = 100,
                                                false = 0)) %>%
                         mutate(area_km = if_else(change == "area_delta",
                                                  area_km/1000,
                                                  area_km)),
                       mapping = aes(x = insect, 
                                     y = area_km,
                                     color = ew)) +
  # geom_hline(yintercept = 0, linetype = 2, linewidth = 0.5) +
  geom_hline(mapping = aes(yintercept = hline), linetype = 2, linewidth = 0.5) +
  geom_point(size = 2) +
  scale_color_manual(name = "East/West",
                     values = ew_colors) +
  coord_flip() +
  theme_bw() +
  facet_grid(~ change, scale = "free_x", labeller = as_labeller(facet_names)) +
  # expand_limits(y = -100) +
  theme(axis.text.y = element_text(face = "italic", size = 9),
        axis.text.x = element_text(size = 6),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 6),
        legend.position = "none")
delta_points
ggsave(file = "output/manuscript/Figure-Area-Change.png",
       plot = delta_points,
       width = 6,
       height = 4)

# Area gained 
gained <- ggplot(data = range_info, mapping = aes(x = area_gained,
                                                  group = ew,
                                                  fill = ew)) +
  geom_histogram(position = "dodge", bins = 20) +
  scale_fill_manual(name = "East/West",
                    values = ew_colors) +
  xlab(label = bquote("Area gained"~(km^2))) +
  ylab(label = expression(paste("# ", italic("Papilio"), " species"))) +
  facet_grid(ssp ~ yr_text) +
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
