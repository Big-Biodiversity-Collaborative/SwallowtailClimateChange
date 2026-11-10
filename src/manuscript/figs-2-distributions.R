# Predicted distributions and deltas
# Erin Zylstra, Jeff Oliver
# ezylstra@arizona.edu, jcoliver@arizona.edu
# 2025-01-16

require(dplyr)
require(terra)
require(ggplot2)
require(tidyterra)
require(cowplot)   # Multi-panel figure
require(quarto)    # Assembly of supplemental figure file
source(file = "functions/get_colors.R")

# Figures are based on 15 x (1 + 2 x 3 x 2) = 195 ggplot objects!!!
# 15 species: one current plot, plus forecast and delta plots of three SSPs for
# two time periods
# It seems too much to ask to create *ALL* the ggplot objects (thus requiring 
# R to hold all of this in memory) before plotting. So will be doing things on 
# a figure/page basis. So start with the two-species figure for the main 
# manuscript (a single page), then do the supplemental figure. The latter 
# could be done multiple steps: first, create the pages (3) for the current 
# distributions, then go species-by-species to create the pages (2/species) for 
# forecast distributions.

# Two figures
# Main manuscript: six panel figure, one column each for P. indra & P. 
#   cresphontes
#   + Predicted suitable areas for contemporary climate
#   + Predicted suitable areas for 2050s, under SSP3-7.0
#   + Difference in area between two predictions
# Supplemental: woo boy. Two things, really.
#   Multi-panel figure with predicted area for current climate for each species
#     (a total of 15 species, including P. appalachiensis and P. palamedes), 
#     this probably works best as two pages of 6 and one page of 3 panels, AND 
#   Two 12-panel figures for each species with forecast predictions (I think 
#     it is just 13 species, excluding P. appalachiensis and P. palamedes):
#   + Page 1, column 1: suitable areas for 2041 for three SSPs
#   + Page 1, column 2: differences in area from current predicted areas
#   + Page 2, column 1: suitable areas for 2071 for three SSPs
#   + Page 2, column 2: differences in area from current predicted areas
ih <- read.csv("data/insect-host.csv")
gbif <- read.csv("data/gbif-pa-summary.csv")

# Figure out which species have current predictions (currently based on 
# whether or not the presence/absence data were created)
gbif <- gbif %>%
  dplyr::filter(species %in% unique(ih$insect)) %>%
  dplyr::filter(pa_csv == "yes")

# Pull species with current predictions out of the gbif data.frame
species <- gbif$species
nice_names <- tolower(gsub(pattern = " ",
                           replacement = "_",
                           x = species))

# Grab spatial files with political boundaries
states <- vect("data/political-boundaries/states.shp")  
countries <- vect("data/political-boundaries/countries.shp")

# Directory where figures are heading
output_basename <- "output/manuscript/"

# Supplemental image file format (should be png or pdf)
supp_image_ext <- ".png" # .png or .pdf

# Reproject states and countries (can take a couple moments with first call to 
# project)
states <- project(states, "ESRI:102009")
countries <- project(countries, crs(states))

# For plotting purposes, we only need Canada, Mexico, and US because the LCC 
# projection is centered in the US, and country boundaries become very 
# distorted for Greenland and Central America
countries <- countries %>%
  filter(adm0_a3 %in% c("USA", "CAN", "MEX"))

# Forecast scenarios and time points
scenarios <- c("ssp245", "ssp370", "ssp585")
times <- c("2041", "2071")

# Get colors for plots
dist_cols <- get_colors(palette = "overlap")
# TODO: Consider moving this to the get_colors() function
names(dist_cols) <- c("Absent", "Hosts only", "Swallowtail only", 
                      "Swallowtail and hosts")  
# names(dist_cols) <- tools::toTitleCase(gsub(pattern = "_",
#                                             replacement = " ", 
#                                             x = names(dist_cols)))

delta_cols <- get_colors(palette = "distdelta")
names(delta_cols) <- tools::toTitleCase(names(delta_cols))  

# Setting colors for state & country lines
state_fill <- "white"
state_color <- "gray50"
countries_color <- "black"

# A list to pass to plotting functions. Are we going overboard on the Law of 
# Demeter? Maybe. Maybe we are.
plot_params <- list(states = states,
                    countries = countries,
                    dist_cols = dist_cols,
                    delta_cols = delta_cols,
                    state_fill = state_fill, 
                    state_color = state_color,
                    countries_color = countries_color)
################################################################################
# Define the functions that will make the plots
# Current plot

#' Make plot of current suitable area
#' 
#' @param nice_name character Compute-friendly name, i.e. "papilio_rumiko"
#' @param plot_params list Plot parameters, such as colors and state/country 
#' border objects
#' @param title character If not null, includes string as title to plot; also 
#' accepts \code{expression} objects
#' @param title_size integer Font size for plot title
#' @param axis_size integer Font size for axis labels
#' @param legend_position character indicating position of legend; defaults to 
#' no legend; see documentation for \code{ggplot2::theme()} for possible values
#' @param plot_extent SpatExtent for map; if \code{NULL} (default) uses extent 
#' of raster file
#' @return ggplot object
current_ggplot <- function(nice_name, plot_params, title = NULL, 
                           title_size = 10, axis_size = 10, 
                           legend_position = "none", plot_extent = NULL) {
  current <- readRDS(paste0("output/overlaps/", nice_name,
                            "-overlap-current.rds"))
  # Update the rasters to have values we want (1, 2, 3, 4). Rasters coming in 
  # should have the following values:
  # 0 = (Insect and hosts absent) Insect and all host plants predicted absent
  # 1 = (1 host only) Insect predicted absent, only 1 host predicted present
  # 2 = (2 or more hosts) Insect predicted absent, >= 2 hosts predicted present
  # 3 = (Insect, no hosts) Insect predicted present, all hosts predicted absent
  # 4 = (Insect, only 1 host) Insect and only 1 host predicted present
  # 5 = (Insect, 2 or more hosts) Insect and >= 2 hosts predicted present
  # Update these to 0-3 scale
  
  current[current %in% 1:2] <- 1
  current[current == 3] <- 2
  current[current %in% 4:5] <- 3
  current <- as.factor(current)
  levels(current) <- data.frame(value = 0:3, label = names(plot_params$dist_cols))
  
  # Do re-projection to Lambert & drop missing
  current <- project(current, crs(plot_params$states), method = "near")
  current <- drop_na(current)

  # If map_extent is not NULL, attempt to expand raster
  if (!is.null(plot_extent)) {
    current <- terra::extend(x = current, y = plot_extent)
  }
  
  # Figure out extent for plot (otherwise extent is based on CAN, MEX, USA)
  xlim_current <- c(ext(current)[1], ext(current)[2])
  ylim_current <- c(ext(current)[3], ext(current)[4])
  
  # Make plot for contemporary predictions
  current_plot <- ggplot() +
    geom_spatvector(data = plot_params$states, color = NA, fill = plot_params$state_fill) +
    geom_spatraster(data = current, maxcell = Inf) +
    scale_fill_manual(name = "label", values = plot_params$dist_cols, na.translate = FALSE) +
    geom_spatvector(data = plot_params$states, color = plot_params$state_color, fill = NA) +
    geom_spatvector(data = plot_params$countries, color = plot_params$countries_color, fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), 
             xlim = xlim_current, ylim = ylim_current) +
    theme_bw() +
    theme(legend.position = legend_position,
          legend.title = element_blank(),
          axis.text = element_text(size = axis_size))

  if (!is.null(title)) {
    current_plot <- current_plot +
      labs(title = title) +
      theme(plot.title = element_text(size = title_size))
  }
  
  ggplot2::set_last_plot(NULL)
  invisible(gc())
  return(current_plot)
}

#' Make plot of forecast suitable area
#' 
#' @param nice_name character Compute-friendly name, i.e. "papilio_rumiko"
#' @param model character Descriptor of climate model and time period, e.g. 
#' "ssp370_2041"
#' @param plot_params list Plot parameters, such as colors and state/country 
#' border objects
#' @param title character If not null, includes string as title to plot; also 
#' accepts \code{expression} objects
#' @param title_size integer Font size for plot title
#' @param axis_size integer Font size for axis labels
#' @param legend_position character indicating position of legend; defaults to 
#' no legend; see documentation for \code{ggplot2::theme()} for possible values
#' @param plot_extent SpatExtent for map; if \code{NULL} (default) uses extent 
#' of raster file
#' @return ggplot object
forecast_ggplot <- function(nice_name, model, plot_params, title = NULL, 
                            title_size = 10, axis_size = 10, 
                            legend_position = "none", plot_extent = NULL) {
  # Get forecast raster
  forecast <- readRDS(paste0("output/overlaps/", nice_name,
                             "-overlap-ensemble_", model, ".rds"))
  
  # Update to 0-3 scale
  forecast[forecast %in% 1:2] <- 1
  forecast[forecast == 3] <- 2
  forecast[forecast %in% 4:5] <- 3
  forecast <- as.factor(forecast)
  levels(forecast) <- data.frame(value = 0:3, label = names(plot_params$dist_cols))
  
  # Do re-projection to Lambert & drop missing
  forecast <- project(forecast, crs(plot_params$states), method = "near")
  forecast <- drop_na(forecast)
  
  # If map_extent is not NULL, attempt to expand raster
  if (!is.null(plot_extent)) {
    forecast <- terra::extend(x = forecast, y = plot_extent)
  }
  
  # Figure out extent for plot (otherwise extent is based on CAN, MEX, USA)
  xlim_forecast <- c(ext(forecast)[1], ext(forecast)[2])
  ylim_forecast <- c(ext(forecast)[3], ext(forecast)[4])
  
  # Make plot for forecast predictions
  forecast_plot <- ggplot() +
    geom_spatvector(data = plot_params$states, color = NA, fill = plot_params$state_fill) +
    geom_spatraster(data = forecast, maxcell = Inf) +
    scale_fill_manual(name = "label", values = plot_params$dist_cols, na.translate = FALSE) +
    geom_spatvector(data = plot_params$states, color = plot_params$state_color, fill = NA) +
    geom_spatvector(data = plot_params$countries, color = plot_params$countries_color, fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), 
             xlim = xlim_forecast, 
             ylim = ylim_forecast) +
    theme_bw() +
    theme(legend.position = legend_position,
          legend.title = element_blank(),
          axis.text = element_text(size = axis_size))
  
  if (!is.null(title)) {
    forecast_plot <- forecast_plot +
      labs(title = title) +
      theme(plot.title = element_text(size = title_size))
  }
  
  ggplot2::set_last_plot(NULL)
  invisible(gc())
  return(forecast_plot)
}

#' Make plot of difference between current and forecast suitable area
#' 
#' @param nice_name character Compute-friendly name, i.e. "papilio_rumiko"
#' @param model character Descriptor of climate model and time period, e.g. 
#' "ssp370_2041"
#' @param plot_params list Plot parameters, such as colors and state/country 
#' border objects
#' @param title character If not null, includes string as title to plot; also 
#' accepts \code{expression} objects
#' @param title_size integer Font size for plot title
#' @param axis_size integer Font size for axis labels
#' @param legend_position character indicating position of legend; defaults to 
#' no legend; see documentation for \code{ggplot2::theme()} for possible values
#' @param plot_extent SpatExtent for map; if \code{NULL} (default) uses extent 
#' of raster file
#' @return ggplot object
delta_ggplot <- function(nice_name, model, plot_params, title = NULL, 
                         title_size = 10, axis_size = 10, 
                         legend_position = "none", plot_extent = NULL) {
  # Get delta raster
  delta <- readRDS(paste0("output/deltas/", nice_name,
                          "-delta-insecthost-", model, ".rds"))
  
  # TODO: Before proceeding, do we need to unwrap?
  # [1] "This is a PackedSpatRaster object. Use 'terra::unwrap()' to unpack it"

  # See src/summary/summary-2-compare-ranges.R
  delta <- as.factor(delta)
  levels(delta) <- data.frame(value = 0:3, label = names(plot_params$delta_cols))
  
  # Do re-projection to Lambert & drop missing
  delta <- project(delta, crs(plot_params$states), method = "near")
  delta <- drop_na(delta)
  
  # If map_extent is not NULL, attempt to expand raster
  if (!is.null(plot_extent)) {
    delta <- terra::extend(x = delta, y = plot_extent)
  }
  
  # Figure out extent for plot (otherwise extent is based on CAN, MEX, USA)
  xlim_delta <- c(ext(delta)[1], ext(delta)[2])
  ylim_delta <- c(ext(delta)[3], ext(delta)[4])
  
  # Make plot for change in predicted ranges
  delta_plot <- ggplot() +
    geom_spatvector(data = plot_params$states, color = NA, fill = plot_params$state_fill) +
    geom_spatraster(data = delta, maxcell = Inf) +
    scale_fill_manual(name = "label", values = plot_params$delta_cols, na.translate = FALSE) +
    geom_spatvector(data = plot_params$states, color = plot_params$state_color, fill = NA) +
    geom_spatvector(data = plot_params$countries, color = plot_params$countries_color, fill = NA) +
    coord_sf(datum = sf::st_crs("EPSG:4326"), 
             xlim = xlim_delta, 
             ylim = ylim_delta) +
    theme_bw() +
    theme(legend.position = legend_position,
          legend.title = element_blank(),
          axis.text = element_text(size = axis_size))
  
  if (!is.null(title)) {
    delta_plot <- delta_plot +
      labs(title = title) +
      theme(plot.title = element_text(size = title_size))
  }
  
  ggplot2::set_last_plot(NULL)
  invisible(gc())
  return(delta_plot)
}

################################################################################
# Figure [1] (not Figure 1 in manuscript)
# Main manuscript figure for two species
main_species <- c("papilio_indra", "papilio_cresphontes")

# + Predicted suitable areas for contemporary climate
# + Predicted suitable areas for 2050s, under SSP3-7.0
# + Difference in area between two predictions

main_plots_list <- vector(mode = "list", length = 6)

model <- "ssp370_2041"

# Iterate over the species of interest
for (i in 1:length(main_species)) {
  one_species <- main_species[i]
  # Indices for list elements so things get put in the right place
  current_index <- (i - 1) * 3 + 1
  forecast_index <- current_index + 1
  delta_index <- current_index + 2
  
  # Make a printable version of the name
  species_name <- gsub(pattern = "papilio_",
                       replacement = "Papilio ",
                       x = one_species)

  # Ultimately, we want all plots for a species to have the same geographic 
  # extent; to this end, we need to find the extents of all three rasters 
  # first, then feed that in to the respective plotting functions
  current_ext <- terra::ext(project(readRDS(paste0("output/overlaps/", one_species,
                                           "-overlap-current.rds")),
                            crs(plot_params$states), method = "near") |>
                              tidyterra::drop_na())
  forecast_ext <- terra::ext(project(readRDS(paste0("output/overlaps/", one_species,
                                            "-overlap-ensemble_", model, ".rds")),
                             crs(plot_params$states), method = "near") |>
                               tidyterra::drop_na())
  delta_ext <- terra::ext(project(readRDS(paste0("output/deltas/", one_species,
                                         "-delta-insecthost-", model, ".rds")),
                          crs(plot_params$states), method = "near") |>
                            tidyterra::drop_na())
  xmin <- min(current_ext[1], forecast_ext[1], delta_ext[1])
  xmax <- max(current_ext[2], forecast_ext[2], delta_ext[2])
  ymin <- min(current_ext[3], forecast_ext[3], delta_ext[3])
  ymax <- max(current_ext[4], forecast_ext[4], delta_ext[4])
  plot_ext <- terra::ext(c(xmin, xmax, ymin, ymax))
  
  message("Plotting ", species_name, " for main manuscript.")
  main_plots_list[[current_index]] <- current_ggplot(nice_name = one_species,
                                                     plot_params = plot_params,
                                                     # including a title messes with relative plot size...
                                                     # Some funkiness for bquote
                                                     title = bquote(italic(.(species_name))),
                                                     legend_position = "bottom",
                                                     axis_size = 6,
                                                     plot_extent = plot_ext)
  
  main_plots_list[[forecast_index]] <- forecast_ggplot(nice_name = one_species,
                                                       model = model,
                                                       plot_params = plot_params,
                                                       axis_size = 6)
  
  main_plots_list[[delta_index]] <- delta_ggplot(nice_name = one_species,
                                                 model = model,
                                                 plot_params = plot_params,
                                                 legend_position = "bottom",
                                                 axis_size = 6)
}

# For this merging of plots, we are going to do it in multiple steps.
# Each row has two plots, there is a row each for legends
# Row 1: Current plots
# Row 2: Legend for current & forecast plots
# Row 3: Forecast plots
# Row 4: Delta plots
# Row 5: Legend for delta plots
# Will need to manually label those plots

# Row 1: pull out the legend for row 2
row_2 <- cowplot::get_legend(main_plots_list[[1]] +
                               theme(legend.text = element_text(size = 6)))

row_0 <- cowplot::plot_grid(cowplot::get_title(main_plots_list[[1]]),
                            cowplot::get_title(main_plots_list[[4]]),
                            byrow = TRUE,
                            ncol = 2)

# titles <- c(cowplot::get_title(main_plots_list[[1]]),
#             cowplot::get_title(main_plots_list[[4]]))
# Pull out current plots into two-element list, making sure there's no legend; 
# we also add a Y-axis label to the left-most plot
current_plots <- c(main_plots_list[[1]] + 
                     theme(legend.position = "none") + 
                     labs(title = NULL) + 
                     ylab("Current"),
                   main_plots_list[[4]] + 
                     theme(legend.position = "none") + 
                     labs(title = NULL))

# row_0 <- cowplot::plot_grid(plotlist = titles,
#                             byrow = TRUE,
#                             ncol = 2,
#                             vjust = 2,
#                             hjust = -2,
#                             rel_widths = c(0.8, 1))

row_1 <- cowplot::plot_grid(plotlist = current_plots,
                            byrow = TRUE,
                            ncol = 2,
                            labels = c("a", "b"),
                            label_size = 10,
                            vjust = 2,
                            hjust = -2,
                            rel_widths = c(0.8, 1))

# Pull out forecast plots into two-element list and add a Y-axis label to the 
# left-most plot
forecast_plots <- c(main_plots_list[[2]] + 
                      ylab("Forecast"),
                    main_plots_list[[5]])
row_3 <- cowplot::plot_grid(plotlist = forecast_plots,
                            byrow = TRUE,
                            ncol = 2,
                            labels = c("c", "d"),
                            label_size = 10,
                            vjust = 2,
                            hjust = -2,
                            rel_widths = c(0.8, 1))

row_5 <- cowplot::get_legend(main_plots_list[[3]] +
                               theme(legend.text = element_text(size = 6)))

# Pull out delta plots into two-element list, making sure there's no legend; we 
# also add a Y-axis label to the left-most plot
delta_plots <- c(main_plots_list[[3]] + 
                   theme(legend.position = "none") +
                   ylab("Forecast - Current"),
                 main_plots_list[[6]] + 
                   theme(legend.position = "none"))
row_4 <- cowplot::plot_grid(plotlist = delta_plots,
                            byrow = TRUE,
                            ncol = 2,
                            labels = c("e", "f"),
                            label_size = 10,
                            vjust = 2,
                            hjust = -2,
                            rel_widths = c(0.8, 1))

main_plots <- cowplot::plot_grid(row_0, row_1, row_2, row_3, row_4, row_5,
                                 ncol = 1,
                                 rel_heights = c(1, 5, 1, 5, 5, 1))

# This will look terrible in RStudio, but fine in a file
main_plots

main_dist_file <- paste0(output_basename, "Figure-Distributions-2.png")
ggsave(filename = main_dist_file,
       plot = main_plots,
       width = 6,
       height = 8,
       units = "in")

# Use the list to create the multi-panel figure
main_plots <- cowplot::plot_grid(plotlist = main_plots_list,
                                 byrow = FALSE,
                                 ncol = 2,
                                 labels = "auto", 
                                 vjust = 1,
                                 hjust = 0)

main_dist_file <- paste0(output_basename, "Figure-Distributions.png")
ggsave(filename = main_dist_file,
       plot = main_plots,
       width = 6,
       height = 8,
       units = "in")

rm(main_plots, main_plots_list)
ggplot2::set_last_plot(NULL)
invisible(gc())

################################################################################
# Figure [2] (not Figure 2 in manuscript)
# Supplemental manuscript figures for all species. Will write [XX] image files 
# to disk and paste together later.

# TODO: For all the supplemental figures, it is an open question of whether or 
# not the plot labels (a, b, c, etc.) are necessary, given that the titles 
# include pretty much all the information necessary to interpret the plot.

# TODO: Legends are currently turned off, but might be useful, if not absurdly 
# duplicative

################################################################################
# Step 2.1: Create the three pages for the current distributions

# iterate over all species for contemporary suitability plots
current_plots_list <- NULL
for (species_i in 1:length(nice_names)) {
  one_species <- nice_names[species_i]

  print_name <- gsub(x = one_species,
                     pattern = "papilio_",
                     replacement = "P. ")
  
  message("Plotting ", print_name, " for supplemental figures")

  # Make the title of the plot (just the species name, for now)
  # Using bquote, need to wrap any variables we want evaluated inside .()
  title <- bquote(italic(.(print_name)))

  # Draw plot
  current_plots_list[[one_species]] <- current_ggplot(nice_name = one_species,
                                                      plot_params = plot_params,
                                                      title = title,
                                                      title_size = 8,
                                                      axis_size = 6)
  ggplot2::set_last_plot(NULL)
  invisible(gc())
  
  # We are on the sixth or last species, so create the output plot
  if (species_i %% 6 == 0 || species_i == length(nice_names)) {
    message("Saving multi-panel page ", ceiling(species_i/6),
            " of ", ceiling(length(nice_names)/6))
    
    # TODO: all the plots are currently center-aligned, but it would be nice if 
    # they were instead left-justified. The align and axis parameters of 
    # plot_grid do not help in accomplishing this. Most of the plots have 
    # different aspect ratios from one another...
    
    # Use the list to create the multi-panel figure
    current_plots <- cowplot::plot_grid(plotlist = current_plots_list,
                                        byrow = TRUE,
                                        ncol = 2,
                                        nrow = 3,
                                        labels = "auto", 
                                        vjust = 1,
                                        hjust = 0)
    
    current_page_file <- paste0(output_basename, 
                             "distribution-pages/current-p-",
                             ceiling(species_i/6),
                             supp_image_ext)
    
    ggsave(filename = current_page_file,
           plot = current_plots,
           width = 6,
           height = 8,
           units = "in")

    
    current_plots_list <- NULL
    ggplot2::set_last_plot(NULL)
    invisible(gc())
  }
}

################################################################################
# Step 2.2: Create the pages (2/species) for forecast distributions and 
# accompanying delta maps.
#   + Page 1, column 1: suitable areas for 2041 for three SSPs
#   + Page 1, column 2: differences in area from current predicted areas
#   + Page 2, column 1: suitable areas for 2071 for three SSPs
#   + Page 2, column 2: differences in area from current predicted areas

# Get information about models (mostly for text to use in plot titles)
climate_models <- read.csv(file = "data/climate-models.csv")
climate_models$name <- gsub(x = climate_models$name,
                            pattern = "ensemble_",
                            replacement = "")

# Iterate over all species
for (species_i in 1:length(nice_names)) {
  one_species <- nice_names[species_i]
  
  print_name <- gsub(x = one_species,
                     pattern = "papilio_",
                     replacement = "P. ")
  
  # Iterate over all years
  for (time_i in 1:length(times)) {
    time <- times[time_i]
    message("Plotting ", time, " forecasts & deltas for ", print_name)
    # Create a list for the six plots for this year
    time_plots_list <- NULL
    # Iterate over all three SSPs
    for (scenario_i in 1:length(scenarios)) {
      scenario <- scenarios[scenario_i]
      # Get the name of the model for plot title
      model <- paste0(scenario, "_", time)
      model_name <- paste0(climate_models$yr_text[climate_models$name == model],
                           ", ",
                           climate_models$ssp_text[climate_models$name == model])
      
      # Create title for forecast plot
      # title <- bquote(italic(.(print_name)), ", ", .(model_name), " forecast")
      title <- bquote(italic(.(print_name))~.(model_name))
      forecast_element_name <- paste0(model, "-forecast")
      
      # Create forecast plot for this scenario + year
      time_plots_list[[forecast_element_name]] <- forecast_ggplot(nice_name = one_species,
                                                                  model = model,
                                                                  plot_params = plot_params,
                                                                  title = title,
                                                                  title_size = 8,
                                                                  axis_size = 6)
      # Create title for delta plot
      # title <- bquote(italic(.(print_name)), ", ", .(model_name), " changes")
      title <- bquote(italic(.(print_name))~.(paste0(model_name, ", changes")))
      delta_element_name <- paste0(model_name, "-delta")
      
      # Create delta plot for this scenario + year
      time_plots_list[[delta_element_name]] <- delta_ggplot(nice_name = one_species,
                                                            model = model,
                                                            plot_params = plot_params,
                                                            title = title,
                                                            title_size = 8,
                                                            axis_size = 6)
    }
    # Finished creating the forecast and delta plots for the year, for the 
    # three SSPs. Now combine them all into six panel plot.
    
    # Create the six-panel plot with plot_grid
    time_plots <- cowplot::plot_grid(plotlist = time_plots_list,
                                     byrow = TRUE,
                                     ncol = 2,
                                     nrow = 3,
                                     labels = "auto", 
                                     vjust = 1,
                                     hjust = 0)

    # Save the plot to file
    one_time_file <- paste0(output_basename, 
                            "distribution-pages/forecast-",
                            one_species, "-", time,
                            supp_image_ext)
    
    ggsave(filename = one_time_file,
           plot = time_plots,
           width = 6,
           height = 8,
           units = "in")
    
    time_plots_list <- NULL
    rm(time_plots)
    ggplot2::set_last_plot(NULL)
    invisible(gc())
  }
}

# Finally, use the Quarto template to put all the individual species's png 
# files into a single document.
# Use the execute_params argument in quarto_render in place of 
# rmarkdown::render's param argument
# e.g. execute_params = list(state = state)
# or, in our case, it would be something like
# execute_params = list(species = species)
quarto::quarto_render(input = "templates/figs-4-distributions-supplemental.qmd",
                      execute_params = list(species = species),
                      output_format = "pdf",
                      output_file = "Supplemental-Figure-Distributions.pdf")

# Move the file to where we want it to be
if (file.copy(from = "Supplemental-Figure-Distributions.pdf",
              to = "output/manuscript/Supplemental-Figure-Distributions.pdf",
              overwrite = TRUE)) {
  invisible(file.remove("Supplemental-Figure-Distributions.pdf"))
}

