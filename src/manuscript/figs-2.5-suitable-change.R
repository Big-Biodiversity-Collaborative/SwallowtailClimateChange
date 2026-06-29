# Plots showing proportion change total and with >= 1 host
# Jeff Oliver
# jcoliver@arizona.edu
# 2026-06-29

require(dplyr)
require(tidyr)
require(ggplot2)

# Read in calculated values for change in suitable area
suitable_file <- "output/summary-stats/overlap-summary-allspp.csv"
suitable <- read.csv(file = suitable_file)

# Want to plot proportion change on Y, with two X-axis values: one for change 
# in total area categorized as suitable for the insect (total insect) and one 
# for change in suitable area that is also suitable for at least one host plant 
# (insect + host)
# Will do this for each species, so each species is represented by two points 
# (total insect, insect + host) and a line connecting them
# We have all the information we need about change in the three area_* columns: 
# area_gained, area_lost, and area_retained
# The proportion change is (Forecast Area - Current Area) / Current Area
# Considering the three columns, the formula above becomes
# ( (area_retained + area_gained ) - (area_retained + area_lost)) / (area_retained + area_lost)
# Simplifying terms, we get
# (area_gained - area_lost) / (area_retained + area_lost)

# For this first plot, we will just do the time period / climate model of 
# interest: SSP3-7.0, 2050s, pulling out those three columns of interest
suitable_subset <- suitable %>%
  dplyr::filter(climate == "ssp370_2041") %>%
  select(c(insect, distribution, starts_with("area_")))

# Now add the column with the proportion we want to plot
suitable_subset <- suitable_subset %>%
  mutate(prop_area_change = (area_gained - area_lost)/(area_retained + area_lost))

# We want the total area to show up first on the X-axis, so relevel that column
suitable_subset <- suitable_subset %>%
  mutate(distribution = factor(x = distribution, 
                               levels = c("total insect", "insect + host")))

# Ready to plot? 
# brevicauda kind blows things up, so drop that one first
suitable_subset <- suitable_subset %>%
  filter(insect != "Papilio brevicauda")
ggplot(data = suitable_subset, mapping = aes(x = distribution, 
                                             y = prop_area_change,
                                             group = insect)) +
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2, color = "#8F8F8F") +
  labs(y = "Proportion Area Change") +
  theme_bw()

# What if we color the lines, with things that have noticible changes due to 
# consideration of host plants? We have to calculate the "slope" of the line.
suitable_slopes <- suitable_subset %>%
  dplyr::select(insect, distribution, prop_area_change) %>%
  tidyr::pivot_wider(id_cols = insect, 
                     names_from = distribution, 
                     values_from = prop_area_change) %>%
  mutate(area_slope = `insect + host` - `total insect`) %>%
  select(insect, area_slope)
# And join this back to the suitable_subset dataframe
suitable_subset <- suitable_subset %>%
  left_join(suitable_slopes)

ggplot(data = suitable_subset, mapping = aes(x = distribution, 
                                             y = prop_area_change,
                                             group = insect)) +
  # geom_point() + 
  geom_line(mapping = aes(color = area_slope), linewidth = 1.75) +
  scale_color_gradient2(low = "#d95f02",
                        mid = "#7570b3",
                        high = "#1b9e77", 
                        # low = "#ce932a",
                        # mid = "#AFAFAF",
                        # high = "#3eafa3", 
                        midpoint = 0) +
  geom_hline(yintercept = 0, linetype = 2, color = "#8F8F8F") +
  labs(y = "Proportion Area Change") +
  theme_bw() +
  theme(legend.position = "none")

# What if we want to label the lines with species names?
# reduce to just one row per species and add shortened names
suitable_labels <- suitable_subset %>%
  filter(distribution == "insect + host") %>%
  select(insect, prop_area_change) %>%
  mutate(short_name = gsub(pattern = "Papilio",
                           replacement = "P.",
                           x = insect))

# Super-cludging label positioning
suitable_labels <- suitable_labels %>%
  mutate(nudge_y = case_when(insect == "Papilio multicaudata" ~ 0.025,
                             insect == "Papilio rutulus" ~ -0.025,
                             insect == "Papilio eurymedon" ~ 0.02,
                             insect == "Papilio indra" ~ -0.02,
                             .default = 0))

ggplot(data = suitable_subset, mapping = aes(x = distribution, 
                                             y = prop_area_change,
                                             group = insect)) +
  # geom_point() + 
  geom_line(mapping = aes(color = area_slope), linewidth = 1.75) +
  scale_color_gradient2(low = "#E47200", # "#d95f02",
                        mid = "#AFAFAF", #7570b3",
                        high = "#1b9e77", 
                        # low = "#ce932a",
                        # mid = "#AFAFAF",
                        # high = "#3eafa3", 
                        midpoint = 0) +
  geom_hline(yintercept = 0, linetype = 2, color = "#8F8F8F") +
  geom_text(data = suitable_labels, 
            mapping = aes(x = 2, label = short_name),
            size = 4,
            hjust = 0.0,
            nudge_x = 0.025,
            nudge_y = suitable_labels$nudge_y) +
            # If not log scale, use:
            # nudge_y = c(-2000, 4500, -2000, 2000)) +
            # If using log scale, use:
            # nudge_y = c(-0.03, 0, -0.34, -0.05)) +
  
  labs(y = "Proportion Area Change") +
  theme_bw() +
  theme(legend.position = "none")
