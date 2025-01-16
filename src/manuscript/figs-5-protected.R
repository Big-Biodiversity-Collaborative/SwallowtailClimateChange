# Plotting changes in protected areas for hotspots
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-01-16

require(dplyr)
require(ggplot2)
require(tidyr)
require(cowplot)
source(file = "functions/get_colors.R")

# Making two figures (well, one figure for manuscript, and multi-panel figure 
# for supplemental material)
# Main: Three panel plot for SSP3-7.0 for all three time points:
#     + Total area of hotspots (line plot)
#     + Area (sq km) of hotspots on protected lands (stacked area)
#     + Percent of area of hotspots on protected lands (stacked area)
# Supplemental: Same as Main, but for other two climate scenarios (SSP2-4.5 and
#     SSP5-8.5)

hotspots <- read.csv(file = "output/summary-stats/protected-areas-hotspots.csv")
climate_models <- read.csv(file = "data/climate-models.csv")
climate_models <- climate_models %>%
  mutate(name = gsub(pattern = "ensemble_",
                     replacement = "", 
                     x = name))
# The SSP to use in the main manuscript
main_ssp <- "370"

# Focus on 4 species minimum as "hotspot" criterion, and insect + host 
# distribution
hotspots <- hotspots %>%
  filter(min_num_spp == 4,
         distribution == "insect + host") %>%
  select(-c(min_num_spp, distribution))

# Need to transform to long for easier subsetting. Will need
#     + climate ("current" or one of the SSPs)
#     + stat (the name of the thing being measured)
#     + value
#     + type of protection category ("total" for total area)
#     + year
hotspots_long <- hotspots %>%
  pivot_longer(cols = -climate,
               names_to = "stat",
               names_prefix = "area_|proportion_",
               values_to = "value")
  
# Proportion stat values just have category name (e.g. "national", "state"), so 
# we need to first update them by pre-appending "prop_" so we can later 
# distinguish them from area values. We also remove leading "prot_" from 
# protected area stat values and immediately replace the leading "sqkm_" with 
# "area_"  (e.g. "prot_sqkm_national" -> "sqkm_national" -> "area_national"). 
# This leaves protected area/proportion values in the stat column a bit more 
# standardized ("area_national", "prop_national") for subsequent wrangling.
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

# We will go ahead and update values in the stat column for total area to have 
# the value "total" instead of "sqkm"
hotspots_long <- hotspots_long %>%
  mutate(stat = if_else(stat == "sqkm",
                        true = "total",
                        false = stat))

# String splitting fun, where we create a new column that will have the type of 
# protected category. For total area (protected and not protected), we use NA 
# for rows of total area (where stat == "total")
hotspots_long <- hotspots_long %>%
  mutate(category = if_else(stat == "total",
                            true = NA_character_,
                            false = gsub(pattern = "area_|prop_",
                                         replacement = "",
                                         x = stat))) 

# Protection category information has moved to separate column so we can trim 
# the stat character to just the first four characters for the measurements of 
# protected area/proportion (e.g. "area_national" -> "area"). For 
# stat == "total", we leave as is
# "area" for protected area, and "prop" for proportion)
hotspots_long <- hotspots_long %>%
  mutate(stat = if_else(stat == "total",
                        true = stat, 
                        false = substr(x = stat, start = 1, stop = 4)))

# Now we add in information about climate, specifically the year to be used for 
# x-axis plotting
hotspots_long <- hotspots_long %>%
  left_join(climate_models, by = c("climate" = "name"))

# Some ugliness next. Need to have a "contemporary" row for each of the three 
# climate models. Already have one set of rows that we can use for this (the 
# rows with values for the current climate model), which we can just assign to 
# first of the climate models (likely 2-4.5). But need to manually add two more 
# rows of three

# First, update all those rows of current data to have SSP 2-4.5
hotspots_long <- hotspots_long %>%
  mutate(ssp = if_else(climate == "current",
                              true = 245,
                              false = ssp))

# Now bind another set of those current climate rows for 370, updating the 
# value from 245 to 370 as we go
hotspots_long <- hotspots_long %>%
  bind_rows(hotspots_long %>% 
              filter(climate == "current") %>%
              mutate(ssp = 370))

# And one more bind for 585 (note updated filter so we only bring in one, 
# instead of two, copies of those rows), with a final sort
hotspots_long <- hotspots_long %>%
  bind_rows(hotspots_long %>% 
              filter(climate == "current", ssp == 370) %>%
              mutate(ssp = 585)) %>%
  arrange(climate, stat)

# For plot labeling, will want to have management category title case
hotspots_long <- hotspots_long %>%
  mutate(category = tools::toTitleCase(category))

# Order category levels for plots
hotspots_long <- hotspots_long %>%
  mutate(category = factor(category,
                           levels = c("National", "State", "Local", "Private", NA)))

# Data are now (finally!) ready to plot. We need three plots for each SSP
#   1. Line plot total area
#   2. Stacked area amount sqkm protected
#   3. Stacked area percentage protected
ssps <- unique(hotspots_long$ssp)

# List of three elements (one element per SSP). Each element is list of three 
# ggplot objects
ssp_plots <- vector(mode = "list",
                    length = length(ssps))
names(ssp_plots) <- ssps

# TODO: Will probably want to make sure scales (y axis limits) are consistent
# across all SSPs. Add that in here.

# TODO: For Total Area line plot, could we use space more efficiently by 
# plotting thousands of square kilometers? e.g. 675000 -> 675

# Get the colors for the different management categories
management_cols <- get_colors(palette = "protected")
# Blech
names(management_cols) <- tools::toTitleCase(names(management_cols))

for (ssp_i in 1:length(ssps)) {
  # Pull out data for this SSP
  ssp_data <- hotspots_long %>%
    filter(ssp == ssps[ssp_i])
  
  # The list that will hold the three plots for this ssp
  plot_list <- vector(mode = "list", length = 3)
  
  # Line plot of total area
  line_total <- ggplot(data = ssp_data %>% filter(stat == "total"), 
                       mapping = aes(x = yr_midpoint, y = value)) +
    geom_point() +
    geom_line() +
    labs(x = "Year", y = "Total Area (sq km)") +
    theme_bw()
  plot_list[[1]] <- line_total
    
  # Stacked area amount of area protected (in sq km)
  stacked_area <- ggplot(data = ssp_data %>% filter(stat == "area"),
                         mapping = aes(x = yr_midpoint, 
                                       y = value, 
                                       fill = category)) +
    geom_area() +
    scale_fill_manual(name = "Management",
                      values = management_cols) +
    labs(x = "Year", y = "Protected Area (sq km)") +
    theme_bw()
  plot_list[[2]] <- stacked_area

  # Stacked area percentage of area protected (100 X prop values)
  stacked_perc <- ggplot(data = ssp_data %>% filter(stat == "prop"),
                         mapping = aes(x = yr_midpoint, 
                                       y = value * 100, # to make it percentage 
                                       fill = category)) +
    geom_area() +
    scale_fill_manual(name = "Management",
                      values = management_cols) +
    labs(x = "Year", y = "% Area Protected") +
    theme_bw()
  plot_list[[3]] <- stacked_perc
  
  # Finally, add that list to the appropriate spot in the ssp list
  ssp_plots[[ssp_i]] <- plot_list
}

# Now we make multi-panel figures. For SSP of interest (3-7.0 for now), this is 
# one column, three row figure for main manuscript. For other SSPs, it will be 
# one column per SSP (so two columns), and three rows

# Starting with ssp of interest
main_plot_list <- ssp_plots[[main_ssp]]
main_plot <- plot_grid(plotlist = main_plot_list,
                       align = "h",
                       ncol = 1,
                       labels = "auto")
main_plot

# Now create same thing, but with remaining SSPs
main_plot_i <- which(names(ssp_plots) == main_ssp)
# We can flatten the list of lists to just a list
supp_plot_list <- unlist(ssp_plots[-main_plot_i], recursive = FALSE)
# The number of columns is dictated by the number of remaining SSPs
num_cols <- length(ssp_plots) - 1

supp_plot <- plot_grid(plotlist = supp_plot_list,
                       align = "h",
                       ncol = num_cols,
                       byrow = FALSE,
                       labels = "auto")
supp_plot
