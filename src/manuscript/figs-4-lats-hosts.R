# 2x2 Quadrat plot showing latitudinal shifts for each species
# Jeff Oliver
# jcoliver@arizona.edu
# 2026-07-06

library(dplyr)
library(ggplot2)
require(cowplot)   # Multi-panel figure

# See also Figure 1 in Van Nuland et al. 2024 
# https://doi.org/10.1073/pnas.2308811121

# TODO: Want error bars on those shifts. May need to get into the file that 
# creates these lat band measurements and add the sd alongside the central 
# tendency measure (either median or mean).

sp_changes <- read.csv(file = "output/summary-stats/overlap-summary-allspp.csv")
sp_changes <- sp_changes %>%
  filter(climate == "ssp370_2041") %>%
  filter(distribution == "insect + host") %>%
  select(insect, lat_max_shift, lat_min_shift)

lat_shifts_plot <- ggplot(data = sp_changes, mapping = aes(x = lat_min_shift, lat_max_shift)) + 
  geom_hline(yintercept = 0, linetype = 2, color = "#999999") + 
  geom_vline(xintercept = 0, linetype = 2, color = "#999999") + 
  geom_point() + 
  geom_text(mapping = aes(label = insect), hjust = 0, vjust = 0) +
  xlab(label = "Southern edge") +
  ylab(label = "Northern edge") +
  theme_bw()

lat_shifts_plot

################################################################################
# Plotting area change versus number of host plants

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

# Drop duplicate host/insect rows (some associations have more than one source, 
# and thus, more than one row)
ih <- ih %>%
  select(insect, host_accepted) %>%
  distinct()

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

# Is there a relationship between number of hosts and current area?
host_current <- lm(current_area ~ num_hosts, data = host_counts)
summary(host_current)
# Yes, more hosts = more area. But not super surprising.

# TODO: regression line is for simple linear regression and does not reflect 
# estimate from model where current area is included as a covariate.
change_v_hosts_plot <- ggplot(data = host_counts, 
                         mapping = aes(x = num_hosts, y = area_net_change)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  xlab(label = "Number of hosts") + 
  ylab(label = "Change in suitable area (sq. km)") +
  theme_bw()
change_v_hosts_plot
# ggsave(plot = change_v_hosts_plot,
#        file = "output/manuscript/Figure-Area-Change-Hosts.png")

# Combine the plots into one image
two_panel <- cowplot::plot_grid(plotlist = list(lat_shifts_plot, 
                                                change_v_hosts_plot),
                               byrow = FALSE,
                               ncol = 2,
                               labels = "auto", 
                               vjust = 1,
                               hjust = 0)
two_panel_file <- paste0("output/manuscript/Figure-4-Lats-Hosts.png")
ggsave(filename = two_panel_file,
       plot = two_panel,
       width = 6,
       height = 4,
       units = "in")
