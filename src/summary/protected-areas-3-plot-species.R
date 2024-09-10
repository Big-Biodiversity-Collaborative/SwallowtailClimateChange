# Plotting protected area change over time
# Jeff Oliver; Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2024-03-13

library(dplyr)
library(ggplot2)
library(tidyr)

# Load results for each species
pa_results <- read.csv("output/summary-stats/protected-areas-allspp.csv")

# Focusing on results for areas that are suitable for insect and at least one 
# host species

# Separate SSP and year data into two separate columns
pa_results <- pa_results %>%
  filter(distribution == "insect + host") %>%
  mutate(ssp = if_else(climate == "current", 
                       true = "current",
                       false = substr(x = climate, start = 4, stop = 6))) %>%
  mutate(year = if_else(climate == "current",
                        true = 2020,
                        false = as.numeric(substr(x = climate, 
                                                  start = 8, 
                                                  stop = 11)))) %>%
  mutate(year = if_else(year == 2041, 
                        true = 2055, if_else(year == 2071,
                                             true = 2085, 2020))) %>%
  select(-c(climate, distribution))

# Now we want to duplicate the climate data so we effectively have a row of 
# current climate for each SSP (as starting points)
curr_df <- tidyr::complete(data.frame(insect = unique(pa_results$insect),
                                      ssp = c("245", "370", "585")),
                           insect, ssp)

# Will want to pull out all those area & proportion data for current climate 
# and add those to the "starting point" rows we have for each of the three SSPs
curr_prop <- pa_results %>%
  filter(ssp == "current")

# Join the species X ssp table (curr_df) with protected areas/proportions from 
# current climate (curr_prop). Start by dropping the ssp column from the 
# curr_prop table (ssp is always "current" in that table). Then add it to the 
# curr_df, by insect (will duplicate rows as necessary).
new_df <- curr_prop %>%
  select(-ssp) %>%
  left_join(curr_df, by = c("insect" = "insect"))

# Now we have a data frame with "starting points" (i.e. the current climate) 
# for each of the three SSPs. Add those rows to the results, and drop rows 
# where ssp is "current". We still have information indicating "current" 
# climate through the year column (at this point, year = 2020 is "current")

# And bind those rows back in to all forecast results
pa_all <- pa_results %>%
  bind_rows(new_df) %>%
  filter(ssp != "current") %>%
  arrange(insect, ssp, year)

# Create east/west indicator and add to data.frame
east <- "appalachiensis|brevicauda|canadensis|cresphontes|glaucus|palamedes|polyxenes|troilus"
west <- "euymedon|indra|machaon|multicaudata|rumiko|rutulus|zelicaon"
pa_all <- pa_all %>%
  mutate(ew = if_else(grepl(east, insect), "East", "West"))

# Optional: Add column (group3) calling out P. machaon (in case we want that 
# option for visualization purposes).
# Not currently used; need to update pivoting if implemented
# pa_all <- pa_all %>%
#   mutate(group3 = if_else(insect == "Papilio machaon", "P. machaon", ew))

# Transform to long for faceting
pa_long <- pa_all %>%
  pivot_longer(cols = -c(insect, ssp, year, ew),
               names_to = "stat",
               names_prefix = "area_|proportion_",
               values_to = "value")

# Extract info from stat value, which could be 
#   + "sqkm": total area in square kilometers
#   + "prot_sqkm_<category>": area in square kilometers for each management 
#     category
#   + "<category>": proportion of range in each management category
  
# Proportion stat values just have category name, so we need to first update 
# them, remove leading "prot" from stat for area calculations, then split stat 
# column into state & category
pa_long <- pa_long %>%
  mutate(stat = if_else(stat %in% c("national", "state", "local", "private"),
                        true = paste0("prop_", stat),
                        false = stat)) %>%
  mutate(stat = gsub(pattern = "prot_",
                     replacement = "",
                     x = stat)) %>%
  mutate(stat = gsub(pattern = "sqkm_",
                     replacement = "area_",
                     x = stat))

# String splitting fun, to create category column with the management category 
# of the measurement (or total, in the case of total area)
pa_long <- pa_long %>%
  mutate(category = if_else(stat == "sqkm",
                            true = "total",
                            false = gsub(pattern = "area_|prop_",
                                         replacement = "",
                                         x = stat))) 

# Category information has moved to separate column so we can just pull out 
# first four characters of stat column to indicate if we are looking at total 
# area (sqkm), protected area (area), or protected proportion (prop)
pa_long <- pa_long %>%
  mutate(stat = substr(x = stat, start = 1, stop = 4))

# For each year, SSP, and East/West, sum total area covered by all insects, so 
# we can use that to weight the means. Dropping two species that are forecast 
# to have no suitable area (in North America)
area_wgts <- pa_long %>%
  filter(!(insect %in% c("Papilio appalachiensis", "Papilio palamedes"))) %>%
  filter(stat == "sqkm") %>%
  group_by(ew, year, ssp) %>%
  summarize(area_total = sum(value), .groups = "keep")

# Take that area_wgts, add it back to pa_all, and create a column with weight 
# for each species, year, ssp
wgts_df <- pa_all %>%
  left_join(area_wgts, by = c("ssp", "year", "ew")) %>%
  mutate(area_wgt = area_sqkm/area_total) %>%
  select(insect, ssp, year, area_wgt) %>%
  data.frame()

# Now add those weights to our long-formatted data
pa_long <- pa_long %>%
  left_join(wgts_df, by = c("insect", "ssp", "year"))

# Now we calculate the means, with and without weighting by total area 
# predicted to be suitable for each insect. We are calculating means for east 
# and west so we can draw mean lines for each of those two groups.
pa_means <- pa_long %>%
  filter(!(insect %in% c("Papilio appalachiensis", "Papilio palamedes"))) %>%
  filter(stat != "sqkm") %>%
  group_by(ew, ssp, year, stat, category) %>%
  summarize(mean_protected = mean(value),
            mean_protected_wgtd = sum(value * area_wgt),
            .groups = "keep")

# For means and long data, re-level categories so they show up in preferred 
# order
pa_long$category <- factor(x = pa_long$category,
                           levels = c("national", "state", "local", "private"))
pa_means$category <- factor(x = pa_means$category,
                           levels = c("national", "state", "local", "private"))

facet_labels <- c(`245` = "SSP2-4.5", 
                  `370` = "SSP3-7.0", 
                  `585` = "SSP5-8.5",
                  "national" = "National",
                  "state" = "State",
                  "local" = "Local",
                  "private" = "Private")
ew_colors <- c("#80b1d3", "#fb8072")

prop_protected_plot <- ggplot(data = pa_long %>% 
                                filter(!(insect %in% c("Papilio appalachiensis", "Papilio palamedes"))) %>%
                                filter(stat == "prop"),
                              mapping = aes(x = year, y = value, group = insect, color = ew)) +
  geom_line(linewidth = 0.6, alpha = 0.3,
            show.legend = FALSE) +
  geom_point(data = pa_means %>% filter(stat == "prop"),
             mapping = aes(x = year, 
                           y = mean_protected_wgtd, 
                           group = ew, 
                           color = ew)) +
  geom_line(data = pa_means %>% filter(stat == "prop"),
            mapping = aes(x = year, 
                          y = mean_protected_wgtd, 
                          group = ew, 
                          color = ew),
            linewidth = 1.0) +
  scale_color_manual(name = "East/West", values = ew_colors) +
  facet_grid(category ~ ssp, 
             scales = "free_y",
             labeller = as_labeller(facet_labels)) +
  theme_bw() +
  ylab("Proportion Protected") +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

ggsave(filename = "output/manuscript/protected-species-prop.png",
       plot = prop_protected_plot)

# Now just plot SSP3-7.0, only using x-facet for the three measurements
ssp370 <- pa_long %>%
  filter(ssp == "370")
ssp370_means <- pa_means %>%
  filter(ssp == "370")

prop_prot_370_plot <- ggplot(data = ssp370 %>% 
                                filter(!(insect %in% c("Papilio appalachiensis", "Papilio palamedes"))) %>%
                                filter(stat == "prop"),
                              mapping = aes(x = year, y = value, group = insect, color = ew)) +
  geom_line(linewidth = 0.6, alpha = 0.3,
            show.legend = FALSE) +
  geom_point(data = ssp370_means %>% filter(stat == "prop"),
             mapping = aes(x = year, 
                           y = mean_protected_wgtd, 
                           group = ew, 
                           color = ew)) +
  geom_line(data = ssp370_means %>% filter(stat == "prop"),
            mapping = aes(x = year, 
                          y = mean_protected_wgtd, 
                          group = ew, 
                          color = ew),
            linewidth = 1.0) +
  scale_color_manual(name = "East/West", values = ew_colors) +
  facet_wrap(~ category,
             nrow = 2,
             ncol = 2,
             scales = "free_y",
             labeller = as_labeller(facet_labels)) +
  theme_bw() +
  ylab("Proportion Protected") +
  xlab("Year") +
  theme(strip.background = element_blank(),
        strip.placement = "outside")
prop_prot_370_plot

ggsave(filename = "output/manuscript/protected-species-prop-370.png",
       plot = prop_prot_370_plot)
