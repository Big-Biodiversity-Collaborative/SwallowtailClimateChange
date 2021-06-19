# Compare range areas between current and forecast estimates
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-18

require(dplyr)
require(tidyr)
require(ggplot2)

model <- "glm"
area_file <- paste0("output/ranges/", model, "-range-areas.csv")

# Read in range size estimates
range_areas <- read.csv(file = area_file)

# Grab the information on arid/non-arid
arid <- read.csv(file = "data/arid-estimates.csv")

# Join arid estimate with range areas
range_areas <- range_areas %>%
  inner_join(arid, by = c("species" = "insect"))

# Calculate deltas
range_areas <- range_areas %>%
  mutate(area_proportion = forecast_area / current_area,
         overlap_proportion = forecast_overlap / current_overlap) %>%
  mutate(area_change = -100 * (1 - area_proportion),
         overlap_change = -100 * (1 - overlap_proportion))

# Start by looking at insect areas alone, ignoring whether or not the range 
# overlaps with host(s)

# Create a long-formatted data set for easier plotting areas
areas_long <- range_areas %>%
  select(species, current_area, forecast_area, arid) %>%
  pivot_longer(cols = -c(species, arid), 
               names_to = "timepoint",
               values_to = "area") %>%
  mutate(arid_text = if_else(condition = arid,
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
  scale_x_discrete(labels = c("current_overlap" = "Contemporary",
                              "forecast_overlap" = "Forecast")) +
  labs(x = "Timepoint", y = "Area (sq. km)") +
  theme_bw() +
  theme(legend.title = element_blank())
ggsave(filename =paste0("output/plots/", model, "-area-changes.pdf"),
       plot = areas_plot)

# See what Wilcox says about change
wilcox_result <- wilcox.test(x = range_areas$area_change[range_areas$arid]/100,
                             y = range_areas$area_change[!range_areas$arid]/100,
                             paired = FALSE)
# W = 12, p-value = 0.05594

# Same as above, but only considering areas where insect overlaps with at least 
# one host plant
# Create a long-formatted data set for easier plotting areas
overlaps_long <- range_areas %>%
  select(species, current_overlap, forecast_overlap, arid) %>%
  pivot_longer(cols = -c(species, arid), 
               names_to = "timepoint",
               values_to = "area") %>%
  mutate(arid_text = if_else(condition = arid,
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
  scale_x_discrete(labels = c("current_overlap" = "Contemporary",
                              "forecast_overlap" = "Forecast")) +
  labs(x = "Timepoint", y = "Area (sq. km)") +
  theme_bw() +
  theme(legend.title = element_blank())
ggsave(filename =paste0("output/plots/", model, "-overlap-changes.pdf"),
       plot = overlaps_plot)

# See what Wilcox says about change
wilcox_result <- wilcox.test(x = range_areas$overlap_change[range_areas$arid]/100,
                             y = range_areas$overlap_change[!range_areas$arid]/100,
                             paired = FALSE)
# W = 11, p-value = 0.04196

# Also go ahead and do t-test
t_result <- t.test(x = range_areas$overlap_proportion[range_areas$arid],
                   y = range_areas$overlap_proportion[!range_areas$arid])
# Meh