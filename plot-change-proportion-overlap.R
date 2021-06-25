# Plot change in proportion of range overlap from SVM models
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-06

# DEPRECATED

require(ggplot2)
require(dplyr)
require(tidyr)

overlaps <- read.csv(file = "output/overlaps/svm-overlaps.csv")

# Drop any that have missing
overlaps <- na.omit(overlaps)

# To color biggest winner & loser
overlaps <- overlaps %>%
  mutate(delta = current - forecast) %>%
  mutate(delta_col = case_when(delta == max(delta) ~ "down",
                               delta == min(delta) ~ "up",
                               TRUE ~ "none"))

# Convert to long for plotting
overlaps_long <- overlaps %>%
  select(-delta) %>%
  pivot_longer(cols = -c(species, delta_col), 
               names_to = "predictor",
               values_to = "proportion")

prop_change_plot <- ggplot(data = overlaps_long, 
                           mapping = aes(x = predictor, 
                                         y = proportion,
                                         group = species, 
                                         color = delta_col)) +
  geom_line() + 
  geom_point() +
  scale_x_discrete(labels = c("current" = "Current ranges",
                              "forecast" = "Forecast ranges")) +
  scale_color_discrete(type = c("up" = "#1b9e77",
                                "down" = "#d95f02",
                                "none" = "#4f4f4f")) +
  labs(y = "Proportion overlap") +
  # ylim(c(0, 1)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.position = "none")

ggsave(filename = "output/plots/svm-overlap-changes.pdf", plot = prop_change_plot)
