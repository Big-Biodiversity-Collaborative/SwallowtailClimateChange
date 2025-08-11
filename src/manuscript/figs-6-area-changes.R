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

# Add horizontal line position and change scale for area values
delta_data <- area_long %>%
  filter(change %in% c("area_delta", "perc_current", "perc_delta")) %>%
  # Add an hline column for where to draw the line of no change
  mutate(hline = if_else(change == "perc_current",
                         true = 100,
                         false = 0))%>%
  # Make the area_delta easier to read by converting it to 1000s of km
  mutate(area_km = if_else(change == "area_delta",
                           area_km/1000,
                           area_km))

# Now we need to move the perc_delta values (which are in the inappropriately 
# named column area_km) to their own column, at least for rows where change is 
# "area_delta". Getting kind of lazy and just making a new data frame
label_data <- delta_data %>%
  filter(change %in% c("area_delta", "perc_delta")) %>%
  select(-hline) %>%
  # Transform to wide
  pivot_wider(id_cols = c(insect, ew), names_from = change, values_from = area_km) %>%
  # Create a label that will look nice on the plot
  mutate(label_text = if_else(perc_delta > 0,
                              true = paste0("+", round(perc_delta, 0), "%"),
                              false = paste0(round(perc_delta, 0), "%"))) %>%
  # Add back the change column so it shows up only in the area_delta facet
  mutate(change = "area_delta") %>%
  # For facets to show up in the correct order, we need to re-level the 
  # change column with all levels (two, in this case) TODO: Doesn't fix??
  mutate(change = factor(x = change,
                         levels = c("perc_current", "area_delta")))

# Pull out area_delta and perc_current rows (dropping perc_delta)
delta_data <- delta_data %>%
  filter(change %in% c("area_delta", "perc_current")) %>%
  # Re-level change factor so facets show up in preferred order (if this is not 
  # done, geom_label Fs everything up!!!)
  mutate(change = factor(x = change,
                         levels = c("perc_current", "area_delta")))

# A little vector we can use to nudge the labels left (net loss) or right (net 
# gain)
nudge_y <- ifelse(test = label_data$perc_delta > 0,
                  yes = 100,
                  no = -100)
# Color box outlines & text to reflect increase (black) or decrease (red)
box_color <- ifelse(test = label_data$perc_delta > 0,
                   yes = "black",
                   no = "red")

facet_names <- c("perc_current" = "Currently suitable area retained (%)",
                 "area_delta" = "Change in total suitable area (1K km^2)")

delta_points <- ggplot(data = delta_data, 
                       mapping = aes(x = insect, 
                                     y = area_km,
                                     color = ew)) +
  geom_hline(mapping = aes(yintercept = hline), linetype = 2, linewidth = 0.5) +
  geom_label(data = label_data, mapping = aes(y = area_delta,
                                              label = label_text),
             color = box_color, #"black",
             nudge_x = 0.25,
             nudge_y = nudge_y,
             size = 2) +
  geom_point(size = 2) +
  scale_color_manual(name = "East/West",
                     values = ew_colors) +
  coord_flip() +
  theme_bw() +
  facet_grid(~ change, scale = "free_x", labeller = as_labeller(facet_names)) +
  theme(axis.text.y = element_text(face = "italic", size = 9),
        axis.text.x = element_text(size = 9),
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
