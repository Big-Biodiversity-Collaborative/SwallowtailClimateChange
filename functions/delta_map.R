#' A ggplot object of map showing predicted change in suitabilities for 
#' individual species
#' 
#' @param species_name character vector with name of insect species, e.g. 
#' "Papilio multicaudata"
#' @param delta_raster raster with with differences in suitability between 
#' predictions for two climate models (one prediction will likely always be 
#' based on current climate) (raster values = 0:3)
#' @param clim_model character vector indicating climate variables on which  
#' predictions are based
#' @param include_legend logical indicating whether legend should be included
#' or not
#' @param horizontal_legend logical indicating whether legend should be 
#' below the plotting area and oriented horizontally. Argument will be ignored
#' if include_legend = FALSE.
#' @param reference_model character vector of the model which is the reference 
#' for determining, loss, gain, or stable. It is unlikely this should be 
#' anything other than "current".
#' @param prediction_area logical indicating whether to differentiate areas
#' inside the prediction area from those outside the prediction area.  If FALSE,
#' cells that were predicted to be unsuitable in both time periods would be
#' shaded the same color as cells where no predictions were made. Will default
#' to true if boundaries = FALSE.
#' @param projection character indicating which projection (if any) to use for 
#' the map: "latlong" will use WGS84 coordinate system; "lambert" will use the 
#' North America Lambert Conformal Conic projection (to limit distortion in 
#' northern Canada), and "auto" will use the Lambert projection if the 
#' prediction area extends north of 52 degree latitude and WGS84 if not.
#' @param obs_points logical indicating whether or not to include observation 
#' points to map; will only observations that have passed filtering process
#' @param full_title logical indicating whether the plot title should include
#' the reference and future climate models (TRUE) or whether the plot title
#' should only include the species name (FALSE).
delta_map <- function(species_name, 
                      delta_raster,
                      clim_model = clim_model,
                      include_legend = TRUE,
                      horizontal_legend = FALSE,
                      reference_model = "current",
                      prediction_area = TRUE,
                      projection = c("auto", "latlon", "lambert"),
                      obs_points = FALSE,
                      full_title = TRUE) {
  if (!require(terra)) {
    stop("overlap_map requires terra package, but it could not be loaded")
  }
  if (!require(tidyterra)) {
    stop("overlap_map requires tidyterra package, but it could not be loaded")
  }  
  if (!require(dplyr)) {
    stop("overlap_map requires dplyr package, but it could not be loaded")
  }
  if (!require(ggplot2)) {
    stop("overlap_map requires ggplot2 package, but it could not be loaded")
  }
  
  projection <- match.arg(arg = projection)
  
  # Want an abbreviated version of species name for title & legend
  name_split <- unlist(strsplit(x = species_name, split = " "))
  abbr_name <- paste0(substr(x = name_split[1], start = 1, stop = 1),
                      ". ", name_split[2])
  
  # Values in raster are 0:3, with 
  # 0 = Area unsuitable* in current and forecast climate
  # 1 = Area suitable in current climate only (= loss)
  # 2 = Area suitable in forecast climate only (= gain)
  # 3 = Area suitable in current and forecast climate (= stable)
  
  # Labels for legend
  labels <- c("Absent", 
              "Loss", 
              "Gain",
              "Stable")    
  
  delta_raster <- as.factor(delta_raster)
  levels(delta_raster) <- data.frame(value = c(0, 1, 2, 3), desc = labels)
  
  color_vec <- c("#e8e8e8",   # Absent
                 "#c2a5cf",   # Loss
                 "#5aae61",   # Gain
                 "#f6e8c3")   # Stable
  # Assign colors to each level so colors are consistent across maps (in case
  # not all levels are present in all maps)
  names(color_vec) <- levels(delta_raster)[[1]][,"desc"]
  
  # Grab files with political boundaries
  countries <- vect("data/political-boundaries/countries.shp")
  states <- vect("data/political-boundaries/states.shp")  
  
  # Calculate northern extent of prediction area
  maxlat <- ext(delta_raster)[4]  
  
  if (projection == "lambert" | (projection == "auto" & maxlat > 52)) {
    
    # Crop the eastern edge of states and countries layers
    state_ext <- ext(states)
    state_ext[2] <- -45
    states <- crop(states, state_ext)
    countries <- crop(countries, state_ext)
    
    # Project layers to Lambert Conformal Conic North America
    states <- project(states, "ESRI:102009")
    countries <- project(countries, crs(states))
    delta_raster <- project(delta_raster, crs(states))
    
    # Get rid of NA values and calculate extent for plot area
    delta_raster <- drop_na(delta_raster)
    xlim <- c(ext(delta_raster)[1], ext(delta_raster)[2])
    ylim <- c(ext(delta_raster)[3], ext(delta_raster)[4])
    
    # Note that when plotting, we're removing boundaries for all countries 
    # except the US, Canada, and Mexico because with this projection that is 
    # centered in the US, country boundaries become very distorted for Greenland 
    # and Central America
    
    if (prediction_area) {
      delta_plot_base <- ggplot() +
        geom_spatvector(data = states, color = NA, fill = "white") +
        geom_spatraster(data = delta_raster, maxcell = Inf) +
        scale_fill_manual(name = "desc", values = color_vec, na.translate = FALSE) +
        geom_spatvector(data = states, color = "gray50", fill = NA) +
        geom_spatvector(data = filter(countries, countries$adm0_a3 %in% c("USA", "CAN", "MEX")),
                        color = "black", fill = NA) +
        coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim,) +
        theme_bw()
    } else {
      # Same map as above, but background color for all land area is the same
      # as what we're using for areas predicted to be unsuitable
      delta_plot_base <- ggplot() +
        geom_spatvector(data = states, color = NA, fill = color_vec[1]) +
        geom_spatraster(data = delta_raster, maxcell = Inf) +
        scale_fill_manual(name = "desc", values = color_vec, na.translate = FALSE) +
        geom_spatvector(data = states, color = "gray50", fill = NA) +
        geom_spatvector(data = filter(countries, countries$adm0_a3 %in% c("USA", "CAN", "MEX")),
                        color = "black", fill = NA) +
        coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim) +
        theme_bw()
    }
    if (obs_points) {
      nice_name <- gsub(pattern = " ",
                        replacement = "_",
                        x = tolower(species_name))
      obs_file <- paste0("data/gbif/presence-absence/",
                         nice_name, "-pa.csv")
      if (file.exists(obs_file)) {
        obs <- read.csv(file = obs_file)
        obs <- obs %>%
          dplyr::filter(pa == 1) %>%
          dplyr::rename(lon = x, lat = y)
        obs <- vect(obs, geom = c("lon", "lat"), crs = "EPSG:4326")
        obs <- project(obs, "ESRI:102009")
        delta_plot_base <- delta_plot_base +
          geom_spatvector(data = obs, color = "red", size = 0.15,
                          shape = 16, alpha = 0.5) +
          coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim)
      }
    }
  } else {
    plot_ext <- ext(delta_raster)
    xlim = c(plot_ext[1], plot_ext[2])
    ylim = c(plot_ext[3], plot_ext[4])
    
    if (prediction_area) {
      delta_plot_base <- ggplot() +
        geom_spatvector(data = states, color = NA, fill = "white") +
        geom_spatraster(data = delta_raster, maxcell = Inf) +
        scale_fill_manual(name = "desc", values = color_vec, na.translate = FALSE) +
        geom_spatvector(data = states, color = "gray50", fill = NA) +
        geom_spatvector(data = filter(countries, countries$adm0_a3 %in% c("USA", "CAN", "MEX")),
                        color = "black", fill = NA) +
        coord_sf(xlim = xlim, ylim = ylim) +
        theme_bw()
    } else {
      delta_plot_base <- ggplot() +
        geom_spatvector(data = states, color = NA, fill = color_vec[1]) +
        geom_spatraster(data = delta_raster, maxcell = Inf) +
        scale_fill_manual(name = "desc", values = color_vec, na.translate = FALSE) +
        geom_spatvector(data = states, color = "gray50", fill = NA) +
        geom_spatvector(data = filter(countries, countries$adm0_a3 %in% c("USA", "CAN", "MEX")),
                        color = "black", fill = NA) +
        coord_sf(xlim = xlim, ylim = ylim) +
        theme_bw()
    }
    if (obs_points) {
      nice_name <- gsub(pattern = " ",
                        replacement = "_",
                        x = tolower(species_name))
      obs_file <- paste0("data/gbif/presence-absence/",
                         nice_name, "-pa.csv")
      if (file.exists(obs_file)) {
        obs <- read.csv(file = obs_file)
        obs <- obs %>%
          dplyr::filter(pa == 1) %>%
          dplyr::rename(lon = x, lat = y)
        obs <- vect(obs, geom = c("lon", "lat"), crs = "EPSG:4326")
        overlap_plot_base <- overlap_plot_base +
          geom_spatvector(data = obs, color = "red", size = 0.15,
                          shape = 16, alpha = 0.5) +
          coord_sf(xlim = xlim, ylim = ylim)
      }
    }
  }
  
  if (full_title) {
    delta_plot_base <- delta_plot_base +   
      labs(title = paste0(abbr_name, ", ", reference_model, " vs. ", clim_model))
  } else {
    delta_plot_base <- delta_plot_base +  
      labs(title = abbr_name)
  }
  
  if (include_legend == TRUE & horizontal_legend == TRUE) {
    delta_plot <- delta_plot_base +
      theme(axis.title = element_blank(),
            legend.title = element_blank(),
            legend.position = "bottom",
            axis.text = element_text(size = 8),
            plot.title = element_text(size = 10))
  } else if (include_legend == TRUE & horizontal_legend == FALSE) {
    delta_plot <- delta_plot_base +
      theme(axis.title = element_blank(),
            legend.title = element_blank(),
            axis.text = element_text(size = 8),
            plot.title = element_text(size = 10))   
  } else {
    delta_plot <- delta_plot_base +
      theme(axis.title = element_blank(),
            legend.position = "None",
            axis.text = element_text(size = 8),
            plot.title = element_text(size = 10))   
  }
  
  return(delta_plot)  
}
