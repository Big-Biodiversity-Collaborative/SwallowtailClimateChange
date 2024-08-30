# Plotting changes in protected areas for hotspots
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-08-08

library(dplyr)
library(ggplot2)
library(tidyr)

hotspots <- read.csv(file = "output/summary-stats/protected-areas-hotspots.csv")

# Want a double faceted plot
# X facet climate model
# Y facet area, protected area, proportion protected
# X-axis has year
# Y-axis varies
# One line for each management category

# Focus on 4 species minimum as "hotspot" criterion, and insect + host 
# distribution
hotspots <- hotspots %>%
  filter(min_num_spp == 4,
         distribution == "insect + host")

# Need to transform to long, so we can facet
hotspots_long <- hotspots %>%
  pivot_longer(cols = -c(min_num_spp, distribution, climate),
               names_to = "stat",
               names_prefix = "area_|proportion_",
               values_to = "value")
  
# Proportion stat values just have category name, so we need to first update 
# them, remove leading "prot" from stat for area calculations, then split stat 
# column into state & category
hotspots_long <- hotspots_long %>%
  mutate(stat = if_else(stat %in% c("national", "state", "local", "private"),
                        true = paste0("prop_", stat),
                        false = stat)) %>%
  mutate(stat = gsub(pattern = "prot_",
                     replacement = "",
                     x = stat)) %>%
  mutate(stat = gsub(pattern = "sqkm_",
                     replacement = "area_",
                     x = stat))

# String splitting fun
hotspots_long <- hotspots_long %>%
  mutate(category = if_else(stat == "sqkm",
                            true = "total",
                            false = gsub(pattern = "area_|prop_",
                                         replacement = "",
                                         x = stat))) 

# Category information has moved to separate column so we can just pull out 
# first four characters of stat column
hotspots_long <- hotspots_long %>%
  mutate(stat = substr(x = stat, start = 1, stop = 4))

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

# Re-order (and transform to factor) the stat column so things plot in the 
# order we would like them (total area, protected area, proportion protected).
# hotspots_long <- hotspots_long %>%
#   mutate(stat = factor(stat, levels = c("area_sqkm", "area_protected_sqkm", "proportion_protected")))

# Make a name for the climate models that is more than just numbers
# Alternatively could do this with substr & pasting
hotspots_long <- hotspots_long %>%
  mutate(model_ssp = case_when(model_name == "245" ~ "2-4.5",
                               model_name == "370" ~ "3-7.0",
                               model_name == "585" ~ "5-8.5"))

# For plot labeling, will also want to have management category title case
hotspots_long <- hotspots_long %>%
  mutate(category = tools::toTitleCase(category))

# Order category levels for plots
hotspots_long <- hotspots_long %>%
  mutate(category = factor(category,
                           levels = c("National", "State", "Local", "Private", "Total")))

# Now plot
pa_hs_plot <- ggplot(data = hotspots_long, mapping = aes(x = model_year,
                                                         y = value,
                                                         group = category,
                                                         color = category)) +
  geom_point() +
  geom_line() +
  facet_grid(stat ~ model_ssp, scales = "free_y",
             labeller = as_labeller(c(area = "Protected Area (sq km)", 
                                      sqkm = "Total Area (sq km)", 
                                      prop = "Proportion Protected",
                                      `2-4.5` = "SSP2-4.5",
                                      `3-7.0` = "SSP3-7.0",
                                      `5-8.5` = "SSP5-8.5"))) +
  scale_color_brewer(palette = "Dark2",
                     name = "Management") +
  theme_bw() +
  ylab(NULL) +
  xlab("Year") +
  theme(strip.background = element_blank(),
                      strip.placement = "outside")
pa_hs_plot

ggsave(filename = "output/manuscript/protected-changes-hotspots-faceted.png",
       plot = pa_hs_plot)

# What about stacked area plot?
pa_hs_plot_area <- ggplot(data = hotspots_long %>%
                            filter(stat != "sqkm"), 
                          mapping = aes(x = model_year,
                                                         y = value,
                                                         fill = category)) +
  geom_area() +
  facet_grid(stat ~ model_ssp, scales = "free_y",
             labeller = as_labeller(c(area = "Protected Area (sq km)", 
                                      sqkm = "Total Area (sq km)", 
                                      prop = "Proportion Protected",
                                      `2-4.5` = "SSP2-4.5",
                                      `3-7.0` = "SSP3-7.0",
                                      `5-8.5` = "SSP5-8.5"))) +
  scale_fill_brewer(palette = "Dark2",
                     name = "Management") +
  theme_bw() +
  ylab(NULL) +
  xlab("Year") +
  theme(strip.background = element_blank(),
        strip.placement = "outside")
pa_hs_plot_area
ggsave(filename = "output/manuscript/protected-changes-hotspots-stack.png",
       plot = pa_hs_plot_area)

# Now just plot SSP3-7.0, only using x-facet for the three measurements
ssp370 <- hotspots_long %>%
  filter(model_ssp == "3-7.0")

pa_hs_370_plot <- ggplot(data = ssp370, mapping = aes(x = model_year,
                                                      y = value,
                                                      group = category,
                                                      color = category)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ stat, nrow = 3, ncol = 1, scales = "free_y",
             labeller = as_labeller(c(area = "Protected Area (sq km)", 
                                      sqkm = "Total Area (sq km)", 
                                      prop = "Proportion Protected"))) +
  scale_color_brewer(palette = "Dark2",
                     name = "Management") +
  theme_bw() +
  ylab(NULL) +
  xlab("Year") +
  theme(strip.background = element_blank(),
        strip.placement = "outside")
pa_hs_370_plot
ggsave(filename = "output/manuscript/protected-changes-hotspots-370.png",
       plot = pa_hs_370_plot)
