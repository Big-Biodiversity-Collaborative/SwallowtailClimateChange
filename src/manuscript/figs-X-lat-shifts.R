# 2x2 Quadrat plot showing latitudinal shifts for each species
# Jeff Oliver
# jcoliver@arizona.edu
# 2026-07-06

library(dplyr)
library(ggplot2)

# See also Figure 1 in Van Nuland et al. 2024 
# https://doi.org/10.1073/pnas.2308811121

sp_changes <- read.csv(file = "output/summary-stats/overlap-summary-allspp.csv")
sp_changes <- sp_changes %>%
  filter(climate == "ssp370_2041") %>%
  filter(distribution == "insect + host") %>%
  select(insect, lat_max_shift, lat_min_shift)

ggplot(data = sp_changes, mapping = aes(x = lat_min_shift, lat_max_shift)) + 
  geom_hline(yintercept = 0, linetype = 2, color = "#999999") + 
  geom_vline(xintercept = 0, linetype = 2, color = "#999999") + 
  geom_point() + 
  geom_text(mapping = aes(label = insect), hjust = 0) +
  xlab(label = "<--Shifts south     Southern edge    Shifts north -->") +
  ylab(label = "<--Shifts south     Northern edge    Shifts north -->") +
  theme_bw()
