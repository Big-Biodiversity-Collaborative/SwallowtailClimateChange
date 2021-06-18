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

# Calculate delta
range_areas <- range_areas %>%
  mutate(area_proportion = forecast_area / current_area) %>%
  mutate(area_change = -100 * (1 - area_proportion))

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
ggplot(data = areas_long, 
       mapping = aes(x = timepoint, y = area, group = species, color = arid_text)) +
  geom_line() + 
  geom_point() +
  scale_color_manual(values = c("#66c2a5", "#fc8d62")) +
  theme_bw() +
  theme(legend.title = element_blank())

# See what Wilcox says about change
wilcox_result <- wilcox.test(x = range_areas$area_change[range_areas$arid]/100,
                             y = range_areas$area_change[!range_areas$arid]/100,
                             paired = FALSE)
# W = 12, p-value = 0.05594