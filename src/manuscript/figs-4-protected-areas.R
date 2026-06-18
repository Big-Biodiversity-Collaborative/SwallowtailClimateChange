# Plotting nationally protected areas in map and plot change in area
# Jeff Oliver
# jcoliver@arizona.edu
# 2026-06-18

require(dplyr)
require(tidyr)
require(terra)
require(tidyterra)
require(ggplot2)
require(cowplot)   # Multi-panel figure

# Base of output filenames
output_basename <- "output/manuscript/"

# Get vector data on protected areas in North America
# First, see if rds version is available for each; if so load them, if not, 
# load the shapefile version.
pa_rds <- "data/protected-areas/protected-areas-categorized.rds"
if (file.exists(pa_rds)) {
  message("Reading protected areas RDS (may take a few seconds)...")
  pa <- readRDS(pa_rds)
  message("Protected areas RDS read.")
} else {
  pa_shp <- "data/protected-areas/protected-areas-categorized.shp"
  message("Reading protected areas shapefile (may take a few minutes)...")
  pa <- terra::vect(pa_shp)
  message("Protected areas shapefile read.")
}

################################################################################
# Panel (a): A map of North America showing areas with national protection

# Pull out just those areas under national protection
national_areas <- pa[which(pa$AGNCY_SHOR == "National")]

# Grab spatial files with political boundaries
countries <- vect("data/political-boundaries/countries.shp")
states <- vect("data/political-boundaries/states.shp")  

# Reproject states and countries (can take a couple moments with first call to 
# project)
states <- project(states, "ESRI:102009")
countries <- project(countries, crs(states))

# For plotting purposes, we only need Canada, Mexico, and US because the LCC 
# projection is centered in the US, and country boundaries become very 
# distorted for Greenland and Central America
countries <- countries %>%
  tidyterra::filter(adm0_a3 %in% c("USA", "CAN", "MEX"))

# We need to do some cropping of data to align with other hotspot plotting
# Load richness rasters
rich_current_file <- "output/richness/current-richness-ov.rds"
rich_current <- readRDS(rich_current_file)

# Re-project the richness raster to same CRS as states
rich_current <- project(rich_current, crs(states), method = "near")
# Get rid of NA values in richness raster and calculate extent for plot areas
rich_current <- tidyterra::drop_na(rich_current)
rich_ext <- ext(rich_current) * 1.01
# Set western boundary of extent in order to make sure Aleutians are included 
# in map
rich_ext <- ext(c(-5e6, rich_ext[2:4]))
xlim <- c(ext(rich_ext)[1], ext(rich_ext)[2])
ylim <- c(ext(rich_ext)[3], ext(rich_ext)[4])

# Crop the states & countries shapes to this expanded extent based on richness
states <- crop(states, rich_ext)
countries <- crop(countries, rich_ext)

# Crop national areas vector, too
national_areas <- crop(national_areas, rich_ext)

linewidth <- 0.1
margins <- c(2, 0, 6, 0)
# Plot all national protected areas. Can take a moment.
national_areas_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = "white") +
  geom_spatvector(data = national_areas, color = NA, fill = "#1B9E77") +
  # Now state/federal draw boundaries
  geom_spatvector(data = states, color = "gray50", linewidth = linewidth,
                  fill = NA) +
  geom_spatvector(data = countries,
                  color = "black", linewidth = linewidth, fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim,
           expand = FALSE) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        legend.spacing.y = unit(10, 'pt'))
# national_areas_plot

################################################################################
# Panel (b): Plot showing change between current and SSP3-7.0 2050s climate

# Read in data with changes in area for hotspots
hotspots <- read.csv(file = "output/summary-stats/protected-areas-hotspots.csv")

# Start by limiting data to 
#  + min_num_spp == 4: hotspot defined as having four or more species
#  + distribution == "insect + host": hotspots require at least one host plant
#  + climate %in% c("current", "ssp370_2041"): Climate model SSP3-7.0 for 2050s 
#    and current climate
# We'll drop the now uninformative min_num_spp and distribution columns, as 
# well as the proportion values, which we aren't going to use.
ssp370_2041_current <- hotspots %>%
  filter(min_num_spp == 4) %>%
  filter(distribution == "insect + host") %>%
  filter(climate %in% c("current", "ssp370_2041")) %>%
  select(-c(min_num_spp, distribution)) %>%
  select(-starts_with("proportion_"))

# Now pivot to wide and drop proportion data
ssp370_2041_current <- ssp370_2041_current %>%
  pivot_longer(cols = -climate, 
               names_to = "area",
               values_to = "sqkm")

# Update values in the area column to be more useful
ssp370_2041_current <- ssp370_2041_current %>%
  # Replace the rows for total area with more informative value ("total")
  mutate(area = if_else(area == "area_sqkm",
                        true = "total",
                        false = area)) %>%
  # Pull of common prefix in remaining rows
  mutate(area = gsub(pattern = "area_prot_sqkm_",
                     replacement = "",
                     x = area))

# At this point, create a wide format version of data we will use to label the 
# lines (with percentage change); drop row of "total" first
ssp370_2041_current_wide <- ssp370_2041_current %>%
  filter(area != "total")  %>%
  pivot_wider(id_cols = area, names_from = climate, values_from = sqkm)

# Create a column with the percentage change; first as a numeric column, then 
# as a character version, adding + for positive changes
ssp370_2041_current_wide <- ssp370_2041_current_wide %>%
  mutate(perc_change = round(100*((ssp370_2041 - current)/current), 0)) %>%
  mutate(perc_change_text = if_else(perc_change > 0,
                                    true = paste0("+", perc_change),
                                    false = as.character(perc_change)))

# Make the text we'll use for the plot and a field with the color to use for 
# the label
ssp370_2041_current_wide <- ssp370_2041_current_wide %>%
  mutate(label_text = paste0(tools::toTitleCase(area),
                             ": ", perc_change_text, "%")) %>%
  mutate(increase = perc_change > 0)

# Let's add some position information!
ssp370_2041_current_wide <- ssp370_2041_current_wide %>%
  mutate(ypos = (current + ssp370_2041)/2) %>%
  mutate(xpos = 2050) # c(2020, 2020, 2040, 2045, 2015))
  
# Back to the long-formatted data, add column with a value for x-axis in 
# plotting
ssp370_2041_current <- ssp370_2041_current %>%
  mutate(year = case_when(climate == "ssp370_2041" ~ 2055.5,
                          climate == "current" ~ 2010.5,
                          .default = NA_real_)) 

text_colors <- c("FALSE" = "#D10000", "TRUE" = "black")
# We drop "total" and "none" from plot - they are 10x higher than others
ssp370_2041_current_plot <- ggplot(data = ssp370_2041_current %>%
                                     filter(!(area %in% c("total", "none"))), 
                               mapping = aes(x = year, 
                                             y = sqkm, 
                                             group = area)) + 
  geom_point() +
  geom_line() +
  geom_text(data = ssp370_2041_current_wide %>%
              filter(area != "none"), 
            mapping = aes(x = xpos, y = ypos,
                          label = label_text,
                          color = increase),
            size = 3,
            # If not log scale, use:
            # nudge_y = c(-2000, 4500, -2000, 2000)) +
            # If using log scale, use:
            nudge_y = c(-0.03, 0, -0.34, -0.05)) +
  scale_color_manual(values = text_colors) +
  scale_y_log10(limits = c(1e3, 1e5)) +
  labs(x = "Year", y = "Area (sq km)") +
  theme_bw() +
  theme(legend.position = "none")
# ssp370_2041_current_plot

################################################################################
# And combine the two plots into a panels (a) and (b)
pa_plots <- cowplot::plot_grid(plotlist = list(national_areas_plot, 
                                               ssp370_2041_current_plot),
                               byrow = FALSE,
                               ncol = 2,
                               labels = "auto", 
                               vjust = 1,
                               hjust = 0)
                               # rel_widths = c(1.75, 1)) # trying to get same height
pa_plots_file <- paste0(output_basename, "Figure-Protected-Areas.png")
ggsave(filename = pa_plots_file,
       plot = pa_plots,
       width = 6,
       height = 4,
       units = "in")

