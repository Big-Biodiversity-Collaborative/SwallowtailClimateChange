# Plotting changes in protected areas for hotspots
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-08-08

library(dplyr)
library(ggplot2)
library(tidyr)

hotspots <- read.csv(file = "output/summary-stats/protected-areas-hotspots.csv")

# Want a faceted plot, one facet with area, one facet with protected area, and 
# maybe one plot with proportion protected area
# X-axis has year
# Y-axis varies
# One line for each climate model

# Focus on 3 species minimum as "hotspot" criterion, and insect + host 
# distribution
hotspots <- hotspots %>%
  filter(min_num_spp == 3,
         distribution == "insect + host")

# Need to transform to long, so we can facet
hotspots_long <- hotspots %>%
  pivot_longer(cols = c(area_sqkm, area_protected_sqkm, proportion_protected),
               names_to = "stat",
               values_to = "value")

# Need to separate out climate model from year (currently combined in climate
# column)
hotspots_long <- hotspots_long %>%
  mutate(model_year = if_else(climate == "current",
                              true = 2010.5,
                              false = as.numeric(substr(climate, 8, 11)) + 14.5))

# Now grab climate model information
hotspots_long <- hotspots_long %>%
  mutate(model_name = if_else(climate == "current",
                              true = NA,
                              false = substr(climate, 4, 6)))

# Some ugliness next. Need to have a "contemporary" row for each of the three 
# climate models. Already have one, which we can just assign to first of the 
# climate models (likely 2-4.5). But need to manually add two more rows of 
# three
# First, just assign three rows to 245
hotspots_long <- hotspots_long %>%
  mutate(model_name = if_else(climate == "current",
                              true = "245",
                              false = model_name))
# Bind three more rows for 370
hotspots_long <- hotspots_long %>%
  bind_rows(hotspots_long %>% 
              filter(climate == "current") %>%
              mutate(model_name = "370"))

# And three more rows for 585 (with a final sort)
hotspots_long <- hotspots_long %>%
  bind_rows(hotspots_long %>% 
              filter(climate == "current", model_name == "370") %>%
              mutate(model_name = "585")) %>%
  arrange(climate, stat)

# Now plot
ggplot(data = hotspots_long, mapping = aes(x = model_year, 
                                           y = value, 
                                           group = model_name,
                                           color = model_name)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~ stat, nrow = 3, ncol = 1, scales = "free_y") +
  theme_bw()
