# Predicted distributions and deltas
# Erin Zylstra, Jeff Oliver
# ezylstra@arizona.edu, jcoliver@arizona.edu
# 2025-01-16

require(dplyr)
require(terra)
require(ggplot2)
require(tidyterra)
require(cowplot)
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
# Main manuscript: six panel figure, one column each for P. cresphontes & P. 
#   rumiko
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

# DEPRECATED:
# To ensure consistency across figures, we will go ahead and make plot objects
# for each species (that we have the appropriate data for), and store these in 
# a list (of lists?). We'll do subsequent iteration across the list to create 
# the figure for the main manuscript and for the supplemental. Note 
# supplemental will be slightly duplicative, as it will also include the two 
# species in the main manuscript figure.
# UPDATE: Nope. Too RAM intensive.

# DEPRECATED:
# If replace_ggplots is TRUE, the script will re-create individual plots, save 
# them to local files (in output/manuscript/distribution-figs), and use those 
# figures to create manuscript figures. If FALSE, it will attempt to read files
# from disk and use those plots for manuscript figures (if file does not exist 
# on disk and replace_ggplots is FALSE, script will attempt to make the ggplot 
# object and save it)
# UPDATE: This doesn't really work, as the ggplot objects include references to 
# variables that may or may not exist.
replace_ggplots <- FALSE

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

# Reproject states and countries (can take a couple moments with first call to 
# project)
states <- project(states, "ESRI:102009")
countries <- project(countries, crs(states))

# For plotting purposes, we only need Canada, Mexico, and US because the LCC 
# projection is centered in the US, and country boundaries become very 
# distorted for Greenland and Central America
countries <- countries %>%
  filter(adm0_a3 %in% c("USA", "CAN", "MEX"))

# TODO: Are these two plots lists necessary?
# List to hold current species predictions
# current_plots <- vector(mode = "list", length = length(nice_names))
# names(current_plots) <- nice_names

# List to hold all the forecast and delta plots
# species_plots <- vector(mode = "list", length = length(nice_names))
# names(species_plots) <- nice_names

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
#' @return ggplot object
current_ggplot <- function(nice_name, plot_params, title = NULL) {
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
    theme(legend.position = "none")

  if (!is.null(title)) {
    current_plot <- current_plot +
      labs(title = title)
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
#' @return ggplot object
forecast_ggplot <- function(nice_name, model, plot_params, title = NULL) {
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
    theme(legend.position = "none")
  
  if (!is.null(title)) {
    forecast_plot <- forecast_plot +
      labs(title = title)
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
#' @return ggplot object
delta_ggplot <- function(nice_name, model, plot_params, title = NULL) {
  # Get delta raster
  delta <- readRDS(paste0("output/deltas/", nice_name,
                          "-delta-insecthost-", model, ".rds"))
  
  # See src/summary/summary-2-compare-ranges.R
  delta <- as.factor(delta)
  levels(delta) <- data.frame(value = 0:3, label = names(plot_params$delta_cols))
  
  # Do re-projection to Lambert & drop missing
  delta <- project(delta, crs(plot_params$states), method = "near")
  delta <- drop_na(delta)
  
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
    theme(legend.position = "none")
  
  if (!is.null(title)) {
    delta_plot <- delta_plot +
      labs(title = title)
  }
  
  ggplot2::set_last_plot(NULL)
  invisible(gc())
  return(delta_plot)
}

################################################################################
# Figure [1] (not Figure 1 in manuscript)
# Main manuscript figure for two species
main_species <- c("papilio_cresphontes", "papilio_rumiko")

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

  message("Plotting ", gsub(pattern = "papilio_",
                            replacement = "Papilio ",
                            x = one_species))
  main_plots_list[[current_index]] <- current_ggplot(nice_name = one_species,
                                                        plot_params = plot_params)
  
  main_plots_list[[forecast_index]] <- forecast_ggplot(nice_name = one_species,
                                                          model = model,
                                                          plot_params = plot_params)
  
  main_plots_list[[delta_index]] <- delta_ggplot(nice_name = one_species,
                                                    model = model,
                                                    plot_params = plot_params)
}

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

# Step 2.1: Create the three pages for the current distributions

# iterate over all species for contemporary suitability plots
current_plots_list <- NULL
for (species_i in 1:length(nice_names)) {
  one_species <- nice_names[species_i]

  print_name <- gsub(x = one_species,
                     pattern = "papilio_",
                     replacement = "P. ")
  
  message("Plotting ", print_name)

  # Make the title of the plot (just the species name, for now)
  # Using bquote, need to wrap any variables we want evaluated inside .()
  title <- bquote(italic(.(print_name)))

  # Draw plot
  current_plots_list[[one_species]] <- current_ggplot(nice_name = one_species,
                                                      plot_params = plot_params,
                                                      title = title)
  ggplot2::set_last_plot(NULL)
  invisible(gc())
  
  # We are on the sixth or last species, so create the output plot
  if (species_i %% 6 == 0 || species_i == length(nice_names)) {
    message("Saving multi-panel page ", ceiling(species_i/6),
            " of ", ceiling(length(nice_names)/6))
    
    # Use the list to create the multi-panel figure
    # TODO: all the plots are currently center-aligned, but it would be nice if 
    # they were instead left-justified. The align and axis parameters of 
    # plot_grid do not help in accomplishing this. Most of the plots have 
    # different aspect ratios from one another...
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
                             ".png")
    
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
  
  message("Plotting forecasts & deltas for ", print_name)
  
  # Iterate over all years
  for (time_i in 1:length(times)) {
    time <- times[time_i]
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
                                                                  title = title)
      # Create title for delta plot
      # title <- bquote(italic(.(print_name)), ", ", .(model_name), " changes")
      title <- bquote(italic(.(print_name))~.(paste0(model_name, ", changes")))
      delta_element_name <- paste0(model_name, "-delta")
      
      # Create delta plot for this scenario + year
      time_plots_list[[delta_element_name]] <- delta_ggplot(nice_name = one_species,
                                                            model = model,
                                                            plot_params = plot_params,
                                                            title = title)
    }
    # Finished creating the forecast and delta plots for the year, for the 
    # three SSPs. Now combine them all.
    
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
                            nice_name, "-", time,
                            ".png")
    
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




# Iterate over all species
for (species_i in 1:length(nice_names)) {
  # This list will hold all forecast & delta plots for one species (and become 
  # one element of the species_plots list)
  one_species_plots <- vector(mode = "list", 
                              length = length(scenarios) * length(times) * 2)

  one_species <- nice_names[species_i]
  current_plot_file <- paste0("output/manuscript/distribution-figs/",
                              one_species, "ggplot-current.rds")
  current_plot_file <- paste0("output/manuscript/distribution-figs/",
                              one_species, "-ggplot-current.RData")

  if (replace_ggplots | !file.exists(current_plot_file)) {
    message("Plotting current distribution for ", one_species)
    
    # Get raster of contemporary predictions
    current <- readRDS(paste0("output/overlaps/", one_species,
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
    levels(current) <- data.frame(value = 0:3, label = names(dist_cols))
    
    # Do re-projection to Lambert & drop missing
    current <- project(current, crs(states), method = "near")
    current <- drop_na(current)
    
    # Figure out extent for plot (otherwise extent is based on CAN, MEX, USA)
    xlim_current <- c(ext(current)[1], ext(current)[2])
    ylim_current <- c(ext(current)[3], ext(current)[4])
    
    # Make plot for contemporary predictions
    current_plot <- ggplot() +
      geom_spatvector(data = states, color = NA, fill = state_fill) +
      geom_spatraster(data = current, maxcell = Inf) +
      scale_fill_manual(name = "label", values = dist_cols, na.translate = FALSE) +
      geom_spatvector(data = states, color = state_color, fill = NA) +
      geom_spatvector(data = countries, color = countries_color, fill = NA) +
      coord_sf(datum = sf::st_crs("EPSG:4326"), 
               xlim = xlim_current, ylim = ylim_current) +
      # scale_x_continuous(breaks = c(-120, -110, -100)) +
      theme_bw() +
      theme(legend.position = "none")
    # theme(plot.margin = unit(margins, "pt"), 
    #       axis.text = element_text(size = 10))
    
    saveRDS(current_plot, file = current_plot_file)
    save(current_plot, file = current_plot_file)
  } else { # Plot is not on disk or replace_ggplot is TRUE
    message("Loading current distribution for ", one_species)
    current_plot <- readRDS(file = current_plot_file)
    load(current_plot_file)
  }
  current_plots[[one_species]] <- current_plot
  rm(current_plot)
  ggplot2::set_last_plot(NULL)
  invisible(gc())
  
  # Now make forecast and delta plot for each ssp and future time period, as 
  # supplemental output is going to have one time period per page, we use the 
  # time (year) as the outer loop and scenario (SSP) as the inner loop to make 
  # subsequent cowplot::plot_grid wrangling a little easier.
  # Keeps track of which spot in the list to insert the plot
  element_i <- 1
  for (time_i in 1:length(times)) {
    time <- times[time_i]
    for (scenario_i in 1:length(scenarios)) {
      scenario <- scenarios[scenario_i]
      # "model" is the text combination of the scenario (ssp) & time (year)
      model <- paste0(scenario, "_", time)

      forecast_plot_file <- paste0("output/manuscript/distribution-figs/",
                                   one_species, "-ggplot-", model, ".rds")
      
      if (replace_ggplots | !file.exists(forecast_plot_file)) {
        message("Plotting forecast distribution ", model, " for ", one_species)
        # Get forecast raster
        forecast <- readRDS(paste0("output/overlaps/", one_species,
                                   "-overlap-ensemble_", model, ".rds"))
        
        # Update to 0-3 scale
        forecast[forecast %in% 1:2] <- 1
        forecast[forecast == 3] <- 2
        forecast[forecast %in% 4:5] <- 3
        forecast <- as.factor(forecast)
        levels(forecast) <- data.frame(value = 0:3, label = names(dist_cols))
        
        # Do re-projection to Lambert & drop missing
        forecast <- project(forecast, crs(states), method = "near")
        forecast <- drop_na(forecast)
        
        # Figure out extent for plot (otherwise extent is based on CAN, MEX, USA)
        xlim_forecast <- c(ext(forecast)[1], ext(forecast)[2])
        ylim_forecast <- c(ext(forecast)[3], ext(forecast)[4])
        
        # Make plot for forecast predictions
        forecast_plot <- ggplot() +
          geom_spatvector(data = states, color = NA, fill = state_fill) +
          geom_spatraster(data = forecast, maxcell = Inf) +
          scale_fill_manual(name = "label", values = dist_cols, na.translate = FALSE) +
          geom_spatvector(data = states, color = state_color, fill = NA) +
          geom_spatvector(data = countries, color = countries_color, fill = NA) +
          coord_sf(datum = sf::st_crs("EPSG:4326"), 
                   xlim = xlim_forecast, 
                   ylim = ylim_forecast) +
          theme_bw() +
          theme(legend.position = "none")
        
        # Store forecast plot
        saveRDS(forecast_plot, file = forecast_plot_file)
      } else {
        message("Loading forecast distribution ", model, " for ", one_species)
        forecast_plot <- readRDS(file = forecast_plot_file)
      }
      one_species_plots[[element_i]] <- forecast_plot
      rm(forecast_plot)
      ggplot2::set_last_plot(NULL)
      invisible(gc())
      # Increment list element counter
      # TODO: Yuck yuck yuck
      element_i <- element_i + 1
      
      delta_plot_file <- paste0("output/manuscript/distribution-figs/",
                                   one_species, "-ggplot-delta-", model, ".rds")
      if (replace_ggplots | !file.exists(delta_plot_file)) {
        message("Plotting delta distribution ", model, " for ", one_species)
        # Get delta raster
        delta <- readRDS(paste0("output/deltas/", one_species,
                                "-delta-insecthost-", model, ".rds"))
        
        # See src/summary/summary-2-compare-ranges.R
        delta <- as.factor(delta)
        levels(delta) <- data.frame(value = 0:3, label = names(delta_cols))
        
        # Do re-projection to Lambert & drop missing
        delta <- project(delta, crs(states), method = "near")
        delta <- drop_na(delta)
        
        # Figure out extent for plot (otherwise extent is based on CAN, MEX, USA)
        xlim_delta <- c(ext(delta)[1], ext(delta)[2])
        ylim_delta <- c(ext(delta)[3], ext(delta)[4])
        
        # Make plot for change in predicted ranges
        delta_plot <- ggplot() +
          geom_spatvector(data = states, color = NA, fill = state_fill) +
          geom_spatraster(data = delta, maxcell = Inf) +
          scale_fill_manual(name = "label", values = delta_cols, na.translate = FALSE) +
          geom_spatvector(data = states, color = state_color, fill = NA) +
          geom_spatvector(data = countries, color = countries_color, fill = NA) +
          coord_sf(datum = sf::st_crs("EPSG:4326"), 
                   xlim = xlim_delta, 
                   ylim = ylim_delta) +
          theme_bw() +
          theme(legend.position = "none")
        
        # Store delta plot
        saveRDS(delta_plot, file = delta_plot_file)
      } else {
        message("Loading delta distribution ", model, " for ", one_species)
        delta_plot <- readRDS(file = delta_plot_file)
      }
      one_species_plots[[element_i]] <- delta_plot
      rm(delta_plot)
      ggplot2::set_last_plot(NULL)
      invisible(gc())
      # Increment list element counter
      # TODO: Still ick
      element_i <- element_i + 1
    } # end iterating over scenarios (SSPs)
  } # end iterating over times (years)
  species_plots[[one_species]] <- one_species_plots
  rm(one_species_plots)
  ggplot2::set_last_plot(NULL)
  invisible(gc())
} # end iterating over species

# We now have contemporary and forecast plots stored in lists. Need to pull out 
# specific plots to create main and supplemental figures

# Main manuscript: six panel figure, one column each for P. cresphontes & P. 
#   rumiko
#   + Predicted suitable areas for contemporary climate
#   + Predicted suitable areas for 2050s, under SSP3-7.0
#   + Difference in area between two predictions

# Pull out the two contemporary plots of interest

# Pull out the two list elements of forecast plot list corresponding to the two 
# species of interest

# Pull out the two forecast plots of interest (2050s + SSP3-7.0, and the delta) 
# for each of the two species

# Make six-panel plot with those six images

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











################################################################################
# Below here is pasted from create-manuscripts-objects.R
################################################################################
# Figures: 2-swallowtail comparison (overlaps, deltas) ------------------------#

# For now, picking P. indra (western species; some gain [incl GB], some loss; 
# not limited by host plants) and P. cresphontes (eastern species; some gain
# [incl midwest], some loss; limited by host plants in FL)

# Species
spp <- c("indra", "cresphontes")
nice_names <- paste0("papilio_", spp)
abbr_names <- paste0("P. ", spp)
full_names <- paste0("Papilio ", spp)

# Future scenario
scen <- "ssp370_2041"

# Load overlap maps
overlapc1 <- readRDS(paste0("output/overlaps/", nice_names[1], 
                            "-overlap-current.rds"))
overlapc2 <- readRDS(paste0("output/overlaps/", nice_names[2], 
                            "-overlap-current.rds"))
overlapf1 <- readRDS(paste0("output/overlaps/", nice_names[1], 
                            "-overlap-ensemble_", scen, ".rds"))
overlapf2 <- readRDS(paste0("output/overlaps/", nice_names[2], 
                            "-overlap-ensemble_", scen, ".rds"))

labels_o <- c("Absent", "Hosts only", "Swallowtail only", "Swallowtail and hosts")  

# Create new overlap rasters with specified levels
overlapc1[overlapc1 %in% 1:2] <- 1
overlapc1[overlapc1 == 3] <- 2
overlapc1[overlapc1 %in% 4:5] <- 3
overlapc1 <- as.factor(overlapc1)
levels(overlapc1) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)
overlapc2[overlapc2 %in% 1:2] <- 1
overlapc2[overlapc2 == 3] <- 2
overlapc2[overlapc2 %in% 4:5] <- 3
overlapc2 <- as.factor(overlapc2)
levels(overlapc2) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)
overlapf1[overlapf1 %in% 1:2] <- 1
overlapf1[overlapf1 == 3] <- 2
overlapf1[overlapf1 %in% 4:5] <- 3
overlapf1 <- as.factor(overlapf1)
levels(overlapf1) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)
overlapf2[overlapf2 %in% 1:2] <- 1
overlapf2[overlapf2 == 3] <- 2
overlapf2[overlapf2 %in% 4:5] <- 3
overlapf2 <- as.factor(overlapf2)
levels(overlapf2) <- data.frame(value = c(0, 1, 2, 3), desc = labels_o)

# Using Paired color palette
color_vec_o <- c("#ededed",   # Absent
                 "#b2df8a",   # Hosts only
                 "#a6cee3",   # Insect only
                 "#1f78b4")   # Hosts and insect
names(color_vec_o) <- levels(overlapc1)[[1]][,"desc"]

# Get rid of NA values and calculate extent for plot areas
overlapc1 <- drop_na(overlapc1)
overlapc2 <- drop_na(overlapc2)
overlapf1 <- drop_na(overlapf1)
overlapf2 <- drop_na(overlapf2)
xlim1 <- c(ext(overlapf1)[1], ext(overlapf1)[2])
ylim1 <- c(ext(overlapf1)[3], ext(overlapf1)[4])
xlim2 <- c(ext(overlapf2)[1], ext(overlapf2)[2])
ylim2 <- c(ext(overlapf2)[3], ext(overlapf2)[4])

margins <- c(4, 0, 6, 0)

# Create overlap map objects
overlapc1_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = "white") +
  geom_spatraster(data = overlapc1, maxcell = Inf) +
  scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
  geom_spatvector(data = states, color = "gray50", fill = NA) +
  geom_spatvector(data = countries, color = "black", fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
  scale_x_continuous(breaks = c(-120, -110, -100)) +
  theme_bw() +
  ylab("Current") +
  theme(plot.margin = unit(margins, "pt"), 
        axis.text = element_text(size = 10))
overlapc2_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = "white") +
  geom_spatraster(data = overlapc2, maxcell = Inf) +
  scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
  geom_spatvector(data = states, color = "gray50", fill = NA) +
  geom_spatvector(data = countries, color = "black", fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim2, ylim = ylim2) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"),
        axis.text = element_text(size = 10))
overlapf1_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = "white") +
  geom_spatraster(data = overlapf1, maxcell = Inf) +
  scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
  geom_spatvector(data = states, color = "gray50", fill = NA) +
  geom_spatvector(data = countries, color = "black", fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
  scale_x_continuous(breaks = c(-120, -110, -100)) +
  theme_bw() +
  ylab("Future") +
  theme(plot.margin = unit(margins, "pt"), 
        axis.text = element_text(size = 10))
overlapf2_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = "white") +
  geom_spatraster(data = overlapf2, maxcell = Inf) +
  scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
  geom_spatvector(data = states, color = "gray50", fill = NA) +
  geom_spatvector(data = countries, color = "black", fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim2, ylim = ylim2) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"))

# Load delta maps
delta1 <- readRDS(paste0("output/deltas/", nice_names[1], "-delta-insecthost-",
                         scen, ".rds"))
delta2 <- readRDS(paste0("output/deltas/", nice_names[2], "-delta-insecthost-",
                         scen, ".rds"))

labels_d <- c("Absent", "Loss", "Gain", "Stable")  

delta1 <- as.factor(delta1)
levels(delta1) <- data.frame(value = c(0, 1, 2, 3), desc = labels_d)
delta2 <- as.factor(delta2)
levels(delta2) <- data.frame(value = c(0, 1, 2, 3), desc = labels_d)

# Tweaked version of RDYlBu palette (so colors pop on light gray background)
color_vec_d <- c("#ededed",   # Absent
                 "#fc8d59",   # Loss
                 "#2c7bb6",   # Gain
                 "#ffffbf")   # Stable
names(color_vec_d) <- levels(delta1)[[1]][,"desc"]

# Get rid of NA values and calculate extent for plot areas
delta1 <- drop_na(delta1)
delta2 <- drop_na(delta2)

# Create delta map objects
delta1_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = "white") +
  geom_spatraster(data = delta1, maxcell = Inf) +
  scale_fill_manual(name = "desc", values = color_vec_d, na.translate = FALSE) +
  geom_spatvector(data = states, color = "gray50", fill = NA) +
  geom_spatvector(data = countries, color = "black", fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim1, ylim = ylim1) +
  scale_x_continuous(breaks = c(-120, -110, -100)) +
  theme_bw() +
  ylab("Future - Current") +
  theme(plot.margin = unit(margins, "pt"), 
        axis.text = element_text(size = 10))
delta2_plot <- ggplot() +
  geom_spatvector(data = states, color = NA, fill = "white") +
  geom_spatraster(data = delta2, maxcell = Inf) +
  scale_fill_manual(name = "desc", values = color_vec_d, na.translate = FALSE) +
  geom_spatvector(data = states, color = "gray50", fill = NA) +
  geom_spatvector(data = countries, color = "black", fill = NA) +
  coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim2, ylim = ylim2) +
  theme_bw() +
  theme(plot.margin = unit(margins, "pt"))

# Create legends (Had to change things up, since cowplot::get_legend didn't 
# seem to work anymore)
# Function to grab legend from ggplot object
get_leg <- function(myggplot) {
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
overlap_for_legend <- ggplot() +
  geom_spatraster(data = overlapc1, maxcell = Inf) +
  scale_fill_manual(name = "desc", values = color_vec_o, na.translate = FALSE) +
  theme(legend.position = "bottom",
        legend.title = element_blank())
legend_o <- get_leg(overlap_for_legend)
delta_for_legend <- ggplot() +
  geom_spatraster(data = delta1, maxcell = Inf) +
  scale_fill_manual(name = "desc", values = color_vec_d, na.translate = FALSE) +
  theme(legend.position = "bottom",
        legend.title = element_blank())
legend_d <- get_leg(delta_for_legend)

# Combine things
overlaps <- plot_grid(overlapc1_plot + theme(legend.position = "none"),
                      overlapc2_plot + theme(legend.position = "none"),
                      overlapf1_plot + theme(legend.position = "none"),
                      overlapf2_plot + theme(legend.position = "none"),
                      align = "vh",
                      nrow = 2,
                      byrow = TRUE)
deltas <- plot_grid(delta1_plot + theme(legend.position = "none"),
                    delta2_plot + theme(legend.position = "none"),
                    align = 'vh',
                    nrow = 1)
col_titles <- ggdraw() + 
  draw_label(full_names[1], fontface = "italic", size = 12, x = 0.29) +
  draw_label(full_names[2], fontface = "italic", size = 12, x = 0.79)

p <- plot_grid(col_titles, overlaps, legend_o, NULL, deltas, legend_d,
               ncol = 1, rel_heights = c(0.2, 5.6, 0.2, 0.2, 2.8, 0.2))

twospecies_name <- paste0(output_basename, "twospecies_6panel.png")
if (!file.exists(twospecies_name) | (file.exists(twospecies_name) & replace)) {
  ggsave(filename = twospecies_name,
         plot = p,
         width = 6.5,
         height = 9,
         units = "in")
}