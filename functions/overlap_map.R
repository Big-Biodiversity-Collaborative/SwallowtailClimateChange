#' A ggplot object of range overlaps
#' 
#' @param species_name character vector with name of insect species, e.g. 
#' "Papilio multicaudata"
#' @param overlap_raster raster with overlap between predicted ranges of insect
#' and its hostplants (raster values = 0:5)
#' @param clim_model character vector indicating climate variables on which  
#' predictions are based
#' @param include_legend logical indicating whether legend should be included
#' or not
#' @param horizontal_legend logical indicating whether legend should be 
#' below the plotting area and oriented horizontally. Argument will be ignored
#' if include_legend = FALSE.
#' @param generic_legend logical indicating whether the legend should refer to
#' butterflies (generic_legend = TRUE) or a particular butterfly species  
#' (generic_legend = FALSE). Argument will be ignored if include_legend = FALSE.
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
#' @param title_scenarioyear logical indicating whether the title (for future
#' maps only) should include the scenario and year (title_scenarioyear = TRUE)
#' or just say "future" (title_scenarioyear = FALSE)
overlap_map <- function(species_name, 
                        overlap_raster,
                        clim_model = clim_model,
                        include_legend = TRUE,
                        horizontal_legend = FALSE,
                        generic_legend = FALSE,
                        prediction_area = TRUE,
                        projection = c("auto", "latlon", "lambert"),
                        obs_points = FALSE,
                        title_scenarioyear = TRUE) {
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
  and_hosts <- paste0(abbr_name, " & hosts")
  
  # Important note: values in raster are 0:5, with:
  # 0 = no insect, no hosts
  # 1 = no insect, one host
  # 2 = no insect, 2 or more hosts
  # 3 = insect, no hosts
  # 4 = insect, 1 host
  # 5 = insect, 2 or more hosts  
  # For plotting, we'll want to combine the host information and have:
  # 0 = Both absent 
  # 1:2 = Hosts only
  # 3 = Insect only
  # 4:5 = Both present
  
  # Labels for legend
  if (generic_legend) {
    labels <- c("Absent", 
                "Hosts only", 
                "Butterfly only",
                "Butterfly and hosts")    
  } else {
    labels <- c("Absent", 
                "Hosts only",
                paste0(abbr_name, " only"),
                and_hosts)     
  }
  
  # Create new overlap raster with specified levels
  overlap2 <- overlap_raster
  overlap2[overlap2 %in% 1:2] <- 1
  overlap2[overlap2 == 3] <- 2
  overlap2[overlap2 %in% 4:5] <- 3
  overlap2 <- as.factor(overlap2)
  levels(overlap2) <- data.frame(value = c(0, 1, 2, 3), desc = labels)
  
  color_vec <- c("#e8e8e8",   # Absent
                 "#b2df8a",   # Hosts only
                 "#a6cee3",   # Insect only
                 "#1f78b4")   # Hosts and insect  
  # Assign colors to each level so colors are consistent across maps (in case
  # not all levels are present in all maps)
  names(color_vec) <- levels(overlap2)[[1]][,"desc"]
  
  # Plot title
  if (title_scenarioyear) {
    title_text <- labs(title = paste0(abbr_name, " and hosts, ", clim_model))
  } else {
    future_text <- ifelse(clim_model == "current", "current", "future")
    title_text <- labs(title = paste0(abbr_name, " and hosts, ",future_text)) 
  }
  
  # Grab files with political boundaries
  countries <- vect("data/political-boundaries/countries.shp")
  states <- vect("data/political-boundaries/states.shp")  
  
  # Calculate northern extent of prediction area
  maxlat <- ext(overlap2)[4]
  
  if (projection == "lambert" | (projection == "auto" & maxlat > 52)) {
    
    # Crop the eastern edge of states and countries layers
    state_ext <- ext(states)
    state_ext[2] <- -45
    states <- crop(states, state_ext)
    countries <- crop(countries, state_ext)
    
    # Project layers to Lambert Conformal Conic North America
    states <- project(states, "ESRI:102009")
    countries <- project(countries, crs(states))
    overlap2 <- project(overlap2, crs(states))
    
    # Get rid of NA values and calculate extent for plot area
    overlap2 <- drop_na(overlap2)
    xlim <- c(ext(overlap2)[1], ext(overlap2)[2])
    ylim <- c(ext(overlap2)[3], ext(overlap2)[4])
    
    # Note that when plotting, we're removing boundaries for all countries 
    # except the US, Canada, and Mexico because with this projection that is 
    # centered in the US, country boundaries become very distorted for Greenland 
    # and Central America
    
    if (prediction_area) {
      overlap_plot_base <- ggplot() +
        geom_spatvector(data = states, color = NA, fill = "white") +
        geom_spatraster(data = overlap2, maxcell = Inf) +
        scale_fill_manual(name = "desc", values = color_vec, na.translate = FALSE) +
        geom_spatvector(data = states, color = "gray50", fill = NA) +
        geom_spatvector(data = filter(countries, countries$adm0_a3 %in% c("USA", "CAN", "MEX")),
                        color = "black", fill = NA) +
        coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim) +
        title_text +
        theme_bw()
    } else {
      # Same map as above, but background color for all land area is the same
      # as what we're using for areas predicted to be unsuitable
      overlap_plot_base <- ggplot() +
        geom_spatvector(data = states, color = NA, fill = color_vec[1]) +
        geom_spatraster(data = overlap2, maxcell = Inf) +
        scale_fill_manual(name = "desc", values = color_vec, na.translate = FALSE) +
        geom_spatvector(data = states, color = "gray50", fill = NA) +
        geom_spatvector(data = filter(countries, countries$adm0_a3 %in% c("USA", "CAN", "MEX")),
                        color = "black", fill = NA) +
        coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim) +
        title_text +
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
        overlap_plot_base <- overlap_plot_base +
          geom_spatvector(data = obs, color = "red", size = 0.15,
                          shape = 16, alpha = 0.5) +
          coord_sf(datum = sf::st_crs("EPSG:4326"), xlim = xlim, ylim = ylim)
      }
    }
  } else {
    plot_ext <- ext(overlap2)
    xlim = c(plot_ext[1], plot_ext[2])
    ylim = c(plot_ext[3], plot_ext[4])
    
    if (prediction_area) {
      overlap_plot_base <- ggplot() +
        geom_spatvector(data = states, color = NA, fill = "white") +
        geom_spatraster(data = overlap2, maxcell = Inf) +
        scale_fill_manual(name = "desc", values = color_vec, na.translate = FALSE) +
        geom_spatvector(data = states, color = "gray50", fill = NA) +
        geom_spatvector(data = countries, color = "black", fill = NA) +
        coord_sf(xlim = xlim, ylim = ylim) +
        title_text +
        theme_bw() 
    } else {
      overlap_plot_base <- ggplot() +
        geom_spatvector(data = states, color = NA, fill = color_vec[1]) +
        geom_spatraster(data = overlap2, maxcell = Inf) +
        scale_fill_manual(name = "desc", values = color_vec, na.translate = FALSE) +
        geom_spatvector(data = states, color = "gray50", fill = NA) +
        geom_spatvector(data = countries, color = "black", fill = NA) +
        coord_sf(xlim = xlim, ylim = ylim) +
        title_text +
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
  
  if (include_legend == TRUE & horizontal_legend == TRUE) {
    overlap_plot <- overlap_plot_base +
      theme(axis.title = element_blank(),
            legend.title = element_blank(),
            legend.position = "bottom",
            axis.text = element_text(size = 8),
            plot.title = element_text(size = 10))
  } else if (include_legend == TRUE & horizontal_legend == FALSE) {
    overlap_plot <- overlap_plot_base +
      theme(axis.title = element_blank(),
            legend.title = element_blank(),
            axis.text = element_text(size = 8),
            plot.title = element_text(size = 10))    
  } else {
    overlap_plot <- overlap_plot_base +
      theme(axis.title = element_blank(),
            legend.position = "None",
            axis.text = element_text(size = 8),
            plot.title = element_text(size = 10))    
  }
  
  return(overlap_plot)  
}
