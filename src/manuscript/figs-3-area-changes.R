# Plots showing individual species' changes in area (total and proportional)
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
# range_info <- range_info %>%
#   filter(distribution == "insect + host") %>%
#   select(-distribution)

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

# From here on out, the species names should have the genus abbreviated
range_info <- range_info %>%
  mutate(insect = gsub(pattern = "Papilio", 
                       replacement = "P.", 
                       x = insect))

# What is change in *current* suitable area (and only current, so we ignore any 
# gains of suitable area)
range_info <- range_info %>%
  mutate(perc_current = (area_retained/(area_lost + area_retained)) * 100)

################################################################################
# Area changes plots
area_long <- range_info %>%
  filter(climate == "ssp370_2041") %>%
  select(insect, distribution, area_gained, area_lost, area_retained, 
         area_delta, perc_delta, perc_current) %>%
  pivot_longer(-c(insect, distribution),
               names_to = "change",
               values_to = "area") %>%
  # filter(!(insect %in% c("P. appalachiensis", "P. palamedes"))) %>%
  arrange(insect) %>%
  mutate(insect = factor(x = insect,
                         levels = rev(unique(insect))))

# Want percentage change values to be included as a text annotation to the 
# area_delta facet. To ensure this _only_ shows up in that facet (and not the 
# perc_current facet), will need to set the value of change to "area_delta" in 
# the dataset we pass to geom_text.

# Start by pulling out the two change values for facets
delta_data <- area_long %>%
  filter(change %in% c("area_delta", "perc_current", "perc_delta")) %>%
  # Add an hline column for where to draw the line of no change
  mutate(hline = if_else(change == "perc_current",
                         true = 100,
                         false = 0))%>%
  # Make the area_delta easier to read by converting it to 1000s of km
  mutate(area = if_else(change == "area_delta",
                        area/1000,
                        area))

# Now we need to move the perc_delta values (which are in the inappropriately 
# named column area) to their own column, at least for rows where change is 
# "area_delta". Getting kind of lazy and just making a new data frame
label_data <- delta_data %>%
  filter(change %in% c("area_delta", "perc_delta")) %>%
  select(-hline) %>%
  # Transform to wide
  pivot_wider(id_cols = c(insect, distribution), 
              names_from = change, 
              values_from = area) %>%
  # Create a label that will look nice on the plot
  mutate(label_text = if_else(perc_delta > 0,
                              true = paste0("+", round(perc_delta, 0), "%"),
                              false = paste0(round(perc_delta, 0), "%"))) %>%
  # Add back the change column so it shows up only in the area_delta facet
  mutate(change = "area_delta")

nudge_y <- ifelse(test = label_data$perc_delta > 0,
                  yes = 100,
                  no = -100)
box_color <- ifelse(test = label_data$perc_delta > 0,
                   yes = "black",
                   no = "red")

# A lazy cludge given only want labels for insect + host
nudge_y <- ifelse(test = label_data$perc_delta[label_data$distribution == "insect + host"] > 0,
                  yes = 150,
                  no = -150)
box_color <- ifelse(test = label_data$perc_delta[label_data$distribution == "insect + host"] > 0,
                    yes = "black",
                    no = "red")

facet_names <- c("perc_current" = "Currently suitable area retained (%)",
                 "area_delta" = "Change in total suitable area (1K km^2)")

# TODO: Will probably want to do something better with the legend
# For the facet showing total suitable area, we only want to include values for 
# areas that are suitable for at least one host plant. So we drop any row for 
# change == "area_delta" and
# distribution == "total insect"
delta_points <- ggplot(data = delta_data %>%
                         filter(change %in% c("area_delta", "perc_current")) %>%
                         filter(!(change == "area_delta" & distribution == "total insect")) %>%
                         mutate(change = factor(x = change,
                                                levels = c("perc_current", "area_delta"))), 
                       mapping = aes(x = insect, 
                                     y = area)) +
  geom_hline(mapping = aes(yintercept = hline), linetype = 2, linewidth = 0.5) +
  geom_label(data = label_data %>% filter(distribution != "total insect"),
             mapping = aes(x = insect,
                           y = area_delta,
                           label = label_text),
             color = box_color, #"black",
             fill = "#FFFFFF",
             nudge_x = 0.25,
             nudge_y = nudge_y,
             size = 2) +
  geom_point(size = 2, mapping =aes(shape = distribution)) +
  scale_shape_manual(values = c(19, 1)) +
  # Cludgiest of cludges to get one facet large enough so percentage labels are 
  # not cut off
  geom_blank(data = data.frame(change = c("area_delta", "area_delta"),
                               area = c(-1500, 1000),
                               insect = c("P. zelicaon", "P. zelicaon"))) +
  coord_flip() +
  theme_bw() +
  facet_grid(~ change, scale = "free_x", labeller = as_labeller(facet_names)) +
  theme(axis.text.y = element_text(face = "italic", size = 9),
        axis.text.x = element_text(size = 9),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        # legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 6))
delta_points
# ggsave(file = "output/manuscript/Figure-Area-Change.png",
#        plot = delta_points,
#        width = 6,
#        height = 4)
 
# Here's the same plot, but only considering areas suitable for at least one 
# host plant
delta_points <- ggplot(data = delta_data %>%
                         filter(change %in% c("area_delta", "perc_current")) %>%
                         filter(distribution != "total insect") %>%
                         mutate(change = factor(x = change,
                                                levels = c("perc_current", "area_delta"))), 
                       mapping = aes(x = insect, 
                                     y = area)) +
  geom_hline(mapping = aes(yintercept = hline), linetype = 2, linewidth = 0.5) +
  geom_label(data = label_data %>% filter(distribution != "total insect"),
             mapping = aes(x = insect,
                           y = area_delta,
                           label = label_text),
             color = box_color, #"black",
             fill = "#FFFFFF",
             nudge_x = 0.25,
             nudge_y = nudge_y,
             size = 2) +
  geom_point(size = 2) +
  # Cludgiest of cludges to get one facet large enough so percentage labels are 
  # not cut off
  geom_blank(data = data.frame(change = c("area_delta", "area_delta"),
                               area = c(-1500, 1000),
                               insect = c("P. zelicaon", "P. zelicaon"))) +
  coord_flip() +
  theme_bw() +
  facet_grid(~ change, scale = "free_x", labeller = as_labeller(facet_names)) +
  theme(axis.text.y = element_text(face = "italic", size = 9),
        axis.text.x = element_text(size = 9),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 6))
delta_points
ggsave(file = "output/manuscript/Figure-Area-Change.png",
       plot = delta_points,
       width = 6,
       height = 4)
