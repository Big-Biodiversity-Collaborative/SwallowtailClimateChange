# Plot area occupied vs number of host plant species
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-10-15

require(dplyr)
# require(tidyr)
require(ggplot2)
# require(cowplot)
require(betareg)
require(lmtest)

# Get insect host plant list
ih <- read.csv(file = "data/insect-host.csv")
# Want to drop those plant species that were not included (too few 
# observations, which we will use as proxy to say plant is not available as a 
# host in North America)
pa_summary <- read.csv(file = "data/gbif-pa-summary.csv")
# Drop species that we did not create a presence/absence file for
pa_summary <- pa_summary[pa_summary$pa_csv == "yes", ]
keep_plant <- ih$host_accepted %in% pa_summary$species

# Drop those records for the plants we just excluded 
ih <- ih[keep_plant, ]

# Do host plant species count
host_counts <- ih %>%
  group_by(insect) %>%
  summarize(num_hosts = n())

# Now get the range dynamics information
range_info <- read.csv(file = "output/summary-stats/overlap-summary-allspp.csv")

# Want the percentage of the insect's suitable habitat that is also suitable 
# for at least one host plant species. So we use distribution == "total insect" 
# and pull out the column pinsect_withhost
# For now, just look at current distribution and SSP370-2041
forecast_model <- "ssp370_2041"
range_info <- range_info %>%
  filter(distribution == "total insect") %>%
  filter(climate %in% c("current", forecast_model)) %>%
  select(insect, pinsect_withhost, climate)

# Join the host plant count to the range information
range_info <- range_info %>%
  left_join(host_counts, by = join_by(insect))

# For now, dropping P. appalaciensis, as it has forecast range of 0
# range_info <- range_info %>%
#   filter(insect != "Papilio appalachiensis")

# Now plot % occupied vs. # hosts, separately for the two climate models
ggplot(data = range_info, mapping = aes(x = num_hosts, y = pinsect_withhost)) +
  geom_point() +
  facet_wrap(~ climate, ncol = 2) +
  scale_x_log10() +
  geom_smooth(method = "lm") +
    theme_bw()

# Because we have percentage (proportion) response data, we can use beta 
# regression
# First transform percentage to proportion
range_info <- range_info %>%
  mutate(prop_withhost = pinsect_withhost/100)

current_info <- range_info %>% 
  filter(climate == "current") %>%
  select(num_hosts, prop_withhost)

beta_model <- betareg(prop_withhost ~ num_hosts,
                      data = current_info)
summary(beta_model)
lrtest(beta_model)

# Can cludge our way to a line with predict
pred_data <- data.frame(prop_withhost = NA,
                        num_hosts = seq(from = min(current_info$num_hosts), 
                                        to = max(current_info$num_hosts), 
                                        by = 0.5))
pred_data$prop_withhost <- predict(beta_model,
                                   newdata = pred_data)

occupied_plot <- ggplot(data = current_info, 
                        mapping = aes(x = num_hosts, y = prop_withhost)) +
  geom_point() +
  geom_line(data = pred_data) +
  ylab("Proportion occupied") +
  xlab("Number of host plant species") +
  theme_bw()
ggsave(plot = occupied_plot,
       filename = "output/manuscript/Figure-Occupied-Hosts.png")
