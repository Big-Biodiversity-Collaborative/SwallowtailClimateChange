# Summarize how much contemporary area overlaps with at least one host plant
# Jeff Oliver
# jcoliver@arizona.edu
# 2024-10-11

# A figure/table for manuscript (calculations occur in 
# src/summary/summary-2-compare-ranges.R)

library(dplyr)
library(ggplot2)

range_info <- read.csv(file = "output/summary-stats/overlap-summary-allspp.csv")
# East/west info, mostly for coloring
ew <- read.csv(file = "data/insect-eastwest.csv")
# Host info, to do quick generalist vs specialist comparison
ih <- read.csv(file = "data/insect-host.csv")
# Need to exclude some plant species that are not in North America (i.e. a fair 
# number of P. machaon's recorded host plants)
ih_reconcile <- read.csv(file = "data/gbif-reconcile.csv")
ih_include <- ih_reconcile %>%
  filter(include) %>%
  pull(accepted_name)

ih_counts <- ih %>%
  select(insect, host_accepted) %>%
  filter(host_accepted %in% ih_include) %>%
  distinct() %>%
  group_by(insect) %>%
  summarize(num_hosts = n())

# Want to look at all areas suitable for insect and what percentage of that 
# area overlaps with area suitable for at least one host plant
area_info <- range_info %>%
  filter(distribution == "total insect") %>%
  select(insect, climate, area, pinsect_withhost) %>%
  left_join(ew) %>%
  left_join(ih_counts)

current_area_info <- area_info %>%
  filter(climate == "current") %>%
  select(-climate)

ew_colors <- c("#3eafa3", "#ce932a")

# X-Y plot: Do specialists have less overlap than generalists?
ggplot(data = current_area_info, mapping = aes(x = num_hosts, 
                                                y = pinsect_withhost,
                                                color = ew)) +
  geom_point() +
  scale_color_manual(name = "East/West",
                    values = ew_colors) +
  xlab(label = "Number of hostplant species") +
  ylab(label = "% Suitable range with ≥ 1 host species") +
  theme_bw()

# Will through non-integer successes warning, but that's fine; use 
# quasibinomial if the warning is tiresome
# bi_glm <- glm(prop_withhost ~ num_hosts + ew,
#               data = current_area_info %>%
#                 mutate(prop_withhost = pinsect_withhost/100),
#               family = "binomial")
# summary(bi_glm)
# N.S., but not sure this is the right test...

# Now do same plot with ssp370, 2041
ssp370_area_info <- area_info %>%
  filter(climate == "ssp370_2041") %>%
  select(-climate)

ggplot(data = ssp370_area_info, mapping = aes(x = num_hosts, 
                                              y = pinsect_withhost,
                                              color = ew)) +
  geom_point() +
  scale_color_manual(name = "East/West",
                     values = ew_colors) +
  xlab(label = "Number of hostplant species") +
  ylab(label = "% Suitable range with ≥ 1 host species") +
  theme_bw()
