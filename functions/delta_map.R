#' A ggplot object of 
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
#' @param boundaries logical indicating whether or not to include political 
#' boundaries
#' @param full_title logical indicating whether the plot title should include
#' the reference and future climate models (TRUE) or whether the plot title
#' should only include the species name (FALSE).
delta_map <- function(species_name, 
                      delta_raster,
                      clim_model = clim_model,
                      include_legend = TRUE,
                      horizontal_legend = FALSE,
                      reference_model = "current",
                      boundaries = TRUE,
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
  if (!require(rnaturalearth)) {
    stop("delta_map_poster requires rnaturalearth package, but it could not be loaded")
  }
  if (!require(rnaturalearthdata)) {
    stop("delta_map_poster requires rnaturalearthdata package, but it could not be loaded")
  }
  
  # Want an abbreviated version of species name for title & legend
  name_split <- unlist(strsplit(x = species_name, split = " "))
  abbr_name <- paste0(substr(x = name_split[1], start = 1, stop = 1),
                      ". ", name_split[2])

  # Values in raster are 0:3, with 
  # 0 = Area unsuitable* in current and forecast climate
  # 1 = Area suitable in current climate only (= loss)
  # 2 = Area suitable in forecast climate only (= gain)
  # 3 = Area suitable in current and forecast climate (= stable)
  # src/summary/create-delta-rasters.R for more details.

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

  # Get limits for plot
  plot_ext <- ext(delta_raster) * 1.04
  xlim = c(plot_ext[1], plot_ext[2])
  ylim = c(plot_ext[3], plot_ext[4])
  
  # Start with base plot and add legend later based on settings  
  if (boundaries) {
    
    boundaries <- rnaturalearth::ne_countries(continent = "north america",
                                              scale = "medium",
                                              returnclass = "sf") %>% 
      dplyr::select(1) %>%
      terra::vect() %>%
      terra::project(y = delta_raster)
    
    delta_plot_base <- ggplot() +
      geom_spatraster(data = delta_raster, maxcell = Inf) +
      scale_fill_manual(name = "desc", values = color_vec, na.translate = FALSE) +
      geom_spatvector(data = boundaries, color = "black", fill = NA) +
      coord_sf(xlim = xlim, ylim = ylim) +
      theme_bw()
    
  } else {
    
    delta_plot_base <- ggplot() +
      geom_spatraster(data = delta_raster, maxcell = Inf) +
      scale_fill_manual(name = "desc", values = color_vec, na.translate = FALSE) +
      coord_sf(xlim = xlim, ylim = ylim) +
      theme_bw()
    
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
            legend.position = "bottom")
  } else if (include_legend == TRUE & horizontal_legend == FALSE) {
    delta_plot <- delta_plot_base +
      theme(axis.title = element_blank(),
            legend.title = element_blank())    
  } else {
    delta_plot <- delta_plot_base +
      theme(axis.title = element_blank(),
            legend.position = "None")    
  }
  return(delta_plot)  
}
