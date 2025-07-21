# Look at the edge locations (10 N/S/E/W points) & median bands
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-07-09

library(terra)
library(tidyterra)
library(dplyr)
library(ggplot2)
library(tidyr)
source(file = "functions/get_colors.R")

model <- "current"
insect_name <- "Papilio glaucus"

# Grab overlap data for rutulus and re-categorize
nice_name <- tolower(gsub(x = insect_name,
                          pattern = " ",
                          replacement = "_"))
overlap_ras <- readRDS(paste0("output/overlaps/", nice_name,
                          "-overlap-", model, ".rds"))
overlap_ras <- terra::unwrap(overlap_ras)
# Four categories:
# 0: Hosts and insect absent
# 1: One or more hosts, but insect absent
# 2: Insect present, no hosts
# 3: Both insect and at least one host
overlap_ras[overlap_ras %in% 1:2] <- 1
overlap_ras[overlap_ras == 3] <- 2
overlap_ras[overlap_ras %in% 4:5] <- 3
overlap_ras <- as.factor(overlap_ras)

# Load in the min/max bands info
overlap_stats <- read.csv(file = "output/summary-stats/overlap-summary-allspp.csv")

# Just grab one species, one climate model, and the insect+host info
overlap_pr <- overlap_stats %>%
  dplyr::filter(insect == insect_name) %>%
  dplyr::filter(climate == "current") %>%
  dplyr::filter(distribution == "insect + host")

# Get colors to use
dist_cols <- get_colors(palette = "overlap")
# Re-level with names of colors for plot
levels(overlap_ras) <- data.frame(value = 0:3, label = names(dist_cols))

# Need to know where to draw these points
# Need to know what longitude to draw the min/max latitude points
map_ext <- terra::ext(overlap_ras)
lon_middle <- mean(map_ext[1:2])
lat_middle <- mean(map_ext[3:4])

# Get points for min/max and bands min/max
min_max_points <- overlap_pr %>%
  select(starts_with(c("lon", "lat"))) %>%
  select(!ends_with(c("shift"))) %>%
  pivot_longer(cols = everything(),
               names_to = "description",
               values_to = "value")
# Now move lon/lat values to new "lon"/"lat" column
min_max_points <- min_max_points %>%
  mutate(lon = if_else(substr(description, start = 1, stop = 3)  == "lon",
                       value,
                       lon_middle)) %>%
  mutate(lat = if_else(substr(description, start = 1, stop = 3)  == "lat",
                       value,
                       lat_middle)) %>%
  select(-value)

# Add column to distinguish band values, 10 most extreme points, and 5% extreme
min_max_points <- min_max_points %>%
  mutate(point_type = case_when(substr(description, start = 9, stop = 13) == "bands" ~ "bands",
                                substr(description, start = 9, stop = 10) == "05" ~ "05",
                                .default = "extreme"))

# Add column for whether this is a longitude edge or a latitude edge
min_max_points <- min_max_points %>%
  mutate(edge_type = if_else(substr(description, start = 1, stop = 3) == "lon",
                             "lon",
                             "lat"))

# Draw the map
ggplot() +
  geom_spatraster(data = overlap_ras, maxcell = Inf) +
  scale_fill_manual(name = "Area", values = dist_cols, na.translate = FALSE) +
  geom_point(data = min_max_points, mapping = aes(x = lon, y = lat, color = point_type)) +
  scale_color_manual(name = "Point Type", values = c("black", "#d95f02", "#7570b3")) +
  theme_bw()

# Lines would be nice, but can't get them different colors...
# ggplot() +
#   geom_spatraster(data = overlap_ras, maxcell = Inf) +
#   scale_fill_manual(name = "Area", values = dist_cols, na.translate = FALSE) +
#   geom_hline(yintercept = min_max_points$lat[min_max_points$edge_type == "lat"]) +
#   theme_bw()


library(dplyr)
library(tidyr)
library(ggplot2)
ov <- read.csv(file = "output/summary-stats/overlap-summary-allspp.csv")
ov <- ov %>% filter(climate %in% c("current", "ssp370_2041"))
ov_area <- ov %>% select(insect, distribution, climate, area)
ggplot(data = ov_area, mapping = aes(x = climate, y = area, group = interaction(insect, distribution), color = distribution)) + geom_line() + facet_wrap(~ insect, scales = "free_y")
