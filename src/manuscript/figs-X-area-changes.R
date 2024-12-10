# Currently unused plots showing changes in area
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-10-07

library(dplyr)
library(tidyr)
library(ggplot2)

# Plots based on the output of src/summary/summary-2-compare-ranges.R, which is 
# written to output/summary-stats/overlap-summary-allspp.csv

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
