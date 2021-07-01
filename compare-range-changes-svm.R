# Compare range areas between current and forecast estimates
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-18

require(dplyr)
require(tidyr)
require(ggplot2)

model <- "svm"
area_file <- paste0("output/ranges/range-areas-", model, ".csv")

# Read in range size estimates
range_areas <- read.csv(file = area_file)

# Grab the information on arid/non-arid
arid <- read.csv(file = "data/arid-estimates.csv")

# Join arid estimate with range areas
range_areas <- range_areas %>%
  dplyr::inner_join(arid, by = c("species" = "insect"))

# Calculate deltas
range_areas <- range_areas %>%
  dplyr::mutate(area_proportion = GFDL.ESM4_RCP45_area / current_area,
         overlap_proportion = GFDL.ESM4_RCP45_overlap_area / current_overlap_area,
         alone_proportion = GFDL.ESM4_RCP45_alone_area / current_alone_area) %>%
  dplyr::mutate(area_change = -100 * (1 - area_proportion),
         overlap_change = -100 * (1 - overlap_proportion),
         alone_change = -100 * (1 - alone_proportion))

# Start by looking at insect areas alone, ignoring whether or not the range 
# overlaps with host(s)

# Create a long-formatted data set for easier plotting areas
areas_long <- range_areas %>%
  dplyr::select(species, current_area, GFDL.ESM4_RCP45_area, arid) %>%
  tidyr::pivot_longer(cols = -c(species, arid), 
               names_to = "timepoint",
               values_to = "area") %>%
  dplyr::mutate(arid_text = if_else(condition = arid,
                             true = "Arid", 
                             false = "Non-arid"))

# Plot areas
areas_plot <- ggplot(data = areas_long, 
                     mapping = aes(x = timepoint, 
                                   y = area, 
                                   group = species, 
                                   color = arid_text)) +
  geom_line() + 
  geom_point() +
  scale_color_manual(values = c("Arid" = "#fc8d62", 
                                "Non-arid" = "#66c2a5")) +
  scale_x_discrete(labels = c("current_area" = "Contemporary",
                              "GFDL.ESM4_RCP45_area" = "Forecast")) +
  labs(x = "Timepoint", y = "Area (sq. km)") +
  theme_bw() +
  theme(legend.title = element_blank())
ggsave(filename =paste0("output/plots/area-changes-", model, ".pdf"),
       plot = areas_plot)

# See what Wilcox says about change
wilcox_result <- wilcox.test(x = range_areas$area_change[range_areas$arid]/100,
                             y = range_areas$area_change[!range_areas$arid]/100,
                             paired = FALSE)
# 2021-07-01
# W = 32, p-value = 0.1375

# Same as above, but only considering areas where insect overlaps with at least 
# one host plant
# Create a long-formatted data set for easier plotting areas
overlaps_long <- range_areas %>%
  dplyr::select(species, current_overlap_area, GFDL.ESM4_RCP45_overlap_area, arid) %>%
  tidyr::pivot_longer(cols = -c(species, arid), 
               names_to = "timepoint",
               values_to = "area") %>%
  dplyr::mutate(arid_text = if_else(condition = arid,
                             true = "Arid", 
                             false = "Non-arid"))

# Plot areas
overlaps_plot <- ggplot(data = overlaps_long, 
                        mapping = aes(x = timepoint, 
                                      y = area, 
                                      group = species, 
                                      color = arid_text)) +
  geom_line() + 
  geom_point() +
  scale_color_manual(values = c("Arid" = "#fc8d62", 
                                "Non-arid" = "#66c2a5")) +
  scale_x_discrete(labels = c("current_overlap_area" = "Contemporary",
                              "GFDL.ESM4_RCP45_overlap_area" = "Forecast")) +
  labs(x = "Timepoint", y = "Area (sq. km)") +
  theme_bw() +
  theme(legend.title = element_blank())
ggsave(filename =paste0("output/plots/overlap-changes-", model, ".pdf"),
       plot = overlaps_plot)

# See what Wilcox says about change
wilcox_result <- wilcox.test(x = range_areas$overlap_change[range_areas$arid]/100,
                             y = range_areas$overlap_change[!range_areas$arid]/100,
                             paired = FALSE)
# 2021-07-01
# W = 44, p-value = 0.0496

# Also go ahead and do t-test
t_result <- t.test(x = range_areas$overlap_proportion[range_areas$arid],
                   y = range_areas$overlap_proportion[!range_areas$arid])
# Meh

# What about plotting changes?
change_boxplot <- ggplot(data = range_areas,
                         mapping = aes(x = arid, y = overlap_proportion)) +
  geom_boxplot() +
  scale_y_log10()
change_boxplot
