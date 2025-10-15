# Plot change in area vs number of host plant species
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-10-13

require(dplyr)
require(tidyr)
require(ggplot2)
require(cowplot)

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

# Restrict this to the one model of interest
climate_model <- "ssp370_2041"
range_info <- range_info %>%
  filter(climate == climate_model) %>%
  filter(distribution == "insect + host") %>%
  select(insect, area, area_gained, area_lost, area_retained)

range_info <- range_info %>%
  mutate(prop_retained = area_retained/(area_lost + area_retained)) %>%
  mutate(area_net_change = area_gained - area_lost) %>%
  mutate(perc_net_change = (area/(area_lost + area_retained) - 1) * 100) %>%
  mutate(current_area = area_retained + area_lost)

# Add range information and diet breadth together
host_counts <- host_counts %>%
  left_join(range_info, by = join_by(insect))

# drop insect species that we do not have predictions for (should just be 
# P. aristodemus)
host_counts <- na.omit(host_counts)

# We now have our various measures of change. I am loath to try vanilla 
# regression on proportion or percentages, but I also don't want to use raw 
# area change as a response, given the variation in current area. Maybe include
# current area as a covariate?
host_area <- lm(area_net_change ~ num_hosts + current_area,
                data = host_counts)
summary(host_area)
plot(x = host_counts$num_hosts, y = host_counts$area_net_change)

# Just for visualization, show also current and change
plot(x = host_counts$current_area, y = host_counts$area_net_change)

# We could also look at percentage change (no upper bound, although lower bound 
# is -100 for complete loss of suitable habitat)
host_area_change <- lm(perc_net_change ~ num_hosts, data = host_counts)
summary(host_area_change)

plot(x = host_counts$num_hosts, y = host_counts$perc_net_change)
plot(x = host_counts$current_area, y = host_counts$perc_net_change)

# Setting aside brevicauda for a moment, what is relationship between 
# percentage change and current area?
sans_brevi <- host_counts %>%
  filter(insect != "Papilio brevicauda")

plot(x = sans_brevi$current_area, y = sans_brevi$perc_net_change)
# TODO: The plot below is probably the one to show
plot(x = sans_brevi$num_hosts, y = sans_brevi$perc_net_change)

# Does current area predict future area?
plot(x = sans_brevi$current_area, y = sans_brevi$area)
current_future <- lm(area ~ current_area, data = sans_brevi)
# No, hints to disruptive nature of climate change
summary(current_future)
# With brevicauda? Same.
summary(lm(area ~ current_area, data = host_counts))

# But current area *does* predict response, in terms of total area change,
# big range = bigger losers, area-wise
summary(lm(area_net_change ~ current_area, data = host_counts))
summary(lm(area_net_change ~ current_area, data = sans_brevi))

# This is not true for percentage of range, though - no relationship between 
# the percentage change in the amount of suitable area and the current area 
# that is suitable
summary(lm(perc_net_change ~ current_area, data = host_counts))
summary(lm(perc_net_change ~ current_area, data = sans_brevi))

# Make a longer data set for plotting with ggplot facets
# sans_brevi_long <- sans_brevi %>%
#   select(insect, area_net_change, perc_net_change, num_hosts, current_area) %>%
#   pivot_longer(-insect, names_to = "measurement", values_to = "value")
  
# Plot percentage net change vs number of hosts
# Plot area net change vs current area
current_area_plot <- ggplot(data = sans_brevi, mapping = aes(x = current_area, 
                                                             y = area_net_change)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(y = "Net change in area (sq. km)", x = "Current area (sq. km)") +
  theme_bw()
perc_area_plot <- ggplot(data = sans_brevi, mapping = aes(x = current_area, 
                                                             y = perc_net_change)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(y = "% Change in area", x = "Current area (sq. km)") +
  theme_bw()
num_hosts_plot <- ggplot(data = sans_brevi, mapping = aes(x = num_hosts, 
                                                          y = perc_net_change)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(y = "% Change in area", x = "# Host plant species") +
  theme_bw()

area_plots <- plot_grid(plotlist = list(current_area_plot, perc_area_plot, num_hosts_plot),
                        align = "v",
                        ncol = 3, 
                        labels = "auto")
ggsave(filename = "output/manuscript/Figure-Area-Change.png",
       plot = area_plots,
       width = 8,
       height = 2.5,
       units = "in")

# Forecast suitable area, as a proportion of current suitable area
host_counts <- host_counts %>%
  mutate(prop_forecast = area/(area_retained + area_lost)) %>%
  mutate(log_prop_forecast = log10(prop_forecast))

sans_app_brevi <- host_counts %>%
  filter(!(insect %in% c("Papilio appalachiensis", "Papilio brevicauda")))

log_forecast_model <- lm(log_prop_forecast ~ num_hosts,
                         data = sans_app_brevi)
summary(log_forecast_model)

pred_forecast <- data.frame(log_prop_forecast = NA,
                            num_hosts = seq(from = 1, to = 40, by = 0.5))
pred_forecast$log_prop_forecast <- predict(log_forecast_model,
                                           newdata = pred_forecast)

pred_forecast <- pred_forecast %>%
  mutate(prop_forecast = 10^log_prop_forecast)

prop_forecast_plot <- ggplot(data = sans_app_brevi, 
                             mapping = aes(x = num_hosts, y = prop_forecast)) +
  geom_point() +
  geom_line(data = pred_forecast) +
  ylab("Relative suitable area") +
  xlab("Number of host plant species") +
  theme_bw()
ggsave(plot = prop_forecast_plot,
       filename = "output/manuscript/Figure-Proportion-Hosts.png")
