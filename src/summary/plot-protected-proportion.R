# Plotting protected area change over time
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-03-11

# EXPLORATORY

library(dplyr)
library(ggplot2)
library(tidyr)

pa_results <- read.csv(file = "output/summary-stats/protected-areas-allspp.csv")

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
                                                  stop = 11))))

# Now we want to duplicate the climate data so we effectively have a row of 
# current climate for each ssp (as starting points)
curr_df <- tidyr::complete(data.frame(insect = unique(pa_results$insect),
                                      ssp = c("245", "370", "585")),
                           insect, ssp)
curr_prop <- pa_results %>%
  filter(climate == "current")

# Add in the proportion of area protected
new_df <- curr_prop %>%
  select(-ssp) %>%
  left_join(curr_df, by = c("insect" = "insect"))

# And bind those rows back in
pa_all <- pa_results %>%
  bind_rows(new_df) %>%
  filter(ssp != "current")

# Quick plot to see patterns, excluding P. machaon because the proportion in 
# protected areas is so high it makes it tough to see patterns
ggplot(data = pa_all %>% filter(insect != "Papilio machaon"), 
       mapping = aes(x = year, 
                     y = proportion_protected,
                     group = interaction(insect, ssp),
                     color = ssp)) +
  geom_line() +
  geom_point() +
  theme_bw()

# Next bits just print out lists of species in each group
# The "low" protected
pa_all %>% 
  filter(year == 2071) %>%
  filter(proportion_protected < 0.1) %>% 
  select(insect) %>% 
  distinct()

# The "medium" protected
pa_all %>% 
  filter(year == 2071) %>%
  filter(proportion_protected > 0.1) %>% 
  filter(proportion_protected < 0.16) %>% 
  select(insect) %>% 
  distinct()

# The "high" protected
pa_all %>% 
  filter(year == 2071) %>%
  filter(proportion_protected > 0.16) %>% 
  select(insect) %>% 
  distinct()
