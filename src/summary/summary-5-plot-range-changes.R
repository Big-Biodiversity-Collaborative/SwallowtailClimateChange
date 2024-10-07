# Create plots of individual species' range changes, noting east/west 
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-10-07

library(dplyr)
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
  # Update year information with midpoint of year data 
  #     2041 -> 2055
  #     2071 -> 2085
  mutate(year = if_else(year == 2041, 
                        true = 2055, 
                        false = 2085))
  
# Add the percent of current range retained to the data frame
range_info <- range_info %>%
  mutate(perc_retained = 100 * (area_retained/(area_retained + area_lost)))
# Total change in area
range_info <- range_info %>%
  mutate(area_delta = (area_gained - area_lost))
# Change in area relative to current range (area_retained + area_lost)
range_info <- range_info %>% 
  mutate(perc_delta = 100 * ((area_gained + area_retained)/(area_lost + area_retained)))

# Now add in east/west information for coloring
ew <- read.csv(file = "data/insect-eastwest.csv")
range_info <- range_info %>%
  left_join(ew)

ew_colors <- c("#80b1d3", "#fb8072")

# Southern (lat_min)
southern <- ggplot(data = range_info, mapping = aes(x = lat_min_shift,
                                                    group = ew,
                                                    fill = ew)) +
  geom_histogram(position = "dodge") +
  scale_fill_manual(name = "East/West",
                    values = ew_colors) +
  xlab(label = "Southern range shift (km)") +
  ylab(label = expression(paste("# ", italic("Papilio"), " species"))) +
  facet_grid(ssp ~ year) +
  theme_bw()
southern

# Northern (lat_max)
northern <- ggplot(data = range_info, mapping = aes(x = lat_max_shift,
                                                    group = ew,
                                                    fill = ew)) +
  geom_histogram(position = "dodge") +
  scale_fill_manual(name = "East/West",
                    values = ew_colors) +
  xlab(label = "Northern range shift (km)") +
  ylab(label = expression(paste("# ", italic("Papilio"), " species"))) +
  facet_grid(ssp ~ year) +
  theme_bw()
northern

# Western (lon_min)
western <- ggplot(data = range_info, mapping = aes(x = lon_min_shift,
                                                   group = ew,
                                                   fill = ew)) +
  geom_histogram(position = "dodge") +
  scale_fill_manual(name = "East/West",
                    values = ew_colors) +
  xlab(label = "Western range shift (km)") +
  ylab(label = expression(paste("# ", italic("Papilio"), " species"))) +
  facet_grid(ssp ~ year) +
  theme_bw()
western

# Eastern (lon_max)
eastern <- ggplot(data = range_info, mapping = aes(x = lon_max_shift,
                                                   group = ew,
                                                   fill = ew)) +
  geom_histogram(position = "dodge") +
  scale_fill_manual(name = "East/West",
                    values = ew_colors) +
  xlab(label = "Eastern range shift (km)") +
  ylab(label = expression(paste("# ", italic("Papilio"), " species"))) +
  facet_grid(ssp ~ year) +
  theme_bw()
eastern

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
