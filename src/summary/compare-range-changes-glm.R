# Compare range areas between current and forecast estimates
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-18

require(dplyr)
require(tidyr)
require(ggplot2)

model <- "glm"
area_file <- paste0("output/ranges/range-areas-", model, ".csv")
output_format <- "png" # pdf

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
ggsave(filename =paste0("output/plots/area-changes-", model, ".", output_format),
       plot = areas_plot)

# See what Wilcox says about change
wilcox_result <- wilcox.test(x = range_areas$area_change[range_areas$arid]/100,
                             y = range_areas$area_change[!range_areas$arid]/100,
                             paired = FALSE)
# 2021-07-01
# W = 8, p-value = 0.02557

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
                                      y = area/1e6, 
                                      group = species, 
                                      color = arid_text)) +
  geom_line(alpha = 0.8) + 
  # geom_point(alpha = 0.8) +
  scale_color_manual(values = c("Arid" = "#fc8d62", 
                                "Non-arid" = "#66c2a5"),
                     alpha) +
  scale_x_discrete(labels = c("current_overlap_area" = "2020",
                              "GFDL.ESM4_RCP45_overlap_area" = "2070"),
                   expand = expansion(add = c(0.3, 0.78))) +
  # labs(x = "Timepoint", y = "Area (sq. km)") +
  ylab(label = bquote("Area (sq. km x"~10^6*")")) + # ~ is space, * is no space
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = c(0.82, 0.48),
        # legend.position = "top",
        legend.text = element_text(size = 8),
        legend.key.size = unit(x = 3, units = "mm"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        axis.text = element_text(size = 10))
overlaps_plot
ggsave(filename =paste0("output/plots/overlap-changes-", model, ".", output_format),
       plot = overlaps_plot,
       width = 56,
       height = 30,
       units = "mm")

# See what Wilcox says about change
wilcox_result <- wilcox.test(x = range_areas$overlap_change[range_areas$arid]/100,
                             y = range_areas$overlap_change[!range_areas$arid]/100,
                             paired = FALSE)
# 2021-07-01
# W = 11, p-value = 0.04196

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
