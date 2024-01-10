#' Species richness (or changes in richness) maps
#' 
#' @param r Raster* object where cells values are species richness 
#' @param predictor character vector of the name of the climate model the on 
#' which predictions were made
#' @param xlim longitudinal limits for map in decimal degrees. Warning: this 
#' will probably not work if map includes international date line
#' @param ylim latitudinal limits for map in decimal degrees.
#' @param palette character vector for color palette to use; see documentation 
#' for \code{tidyterra::scale_fill_whitebox_c()} for valid values. "gn_yl" is 
#' good for positive values (i.e. richness), while "purple" better accommodates
#' showing differences (high values are green, low values are purple, middle 
#' values are white)
#' @param legend_name character vector to use for legend title
#' @param direction numeric vector, either 1 or -1, for ordering color values; 
#' 1 indicates darkest to lightest, -1 indicates lightest to darkest
#' @param pal_limits numeric vector of length two indicating limits of color 
#' palette (minimum and maximum). If \code{NULL}, limits will be determined 
#' by values in \code{r}.
#' @param plot_title character vector of length one to use as plot title; if 
#' \code{NULL}, no title is included
#' 
#' @details Note that this script does no modeling or predicting, relying on 
#' predictions being passed through the list of rasters, \code{r}.
#' 
#' @return A ggplot object
richness_map <- function(r, 
                         predictor,
                         xlim = c(-170, -50),
                         ylim = c(10, 70),
                         palette = "gn_yl",
                         legend_name = "Richness",
                         direction = 1, 
                         pal_limits = NULL,
                         plot_title = NULL) {
  if (!require(terra)) {
    stop("overlap_map requires terra package, but it could not be loaded")
  }
  if (!require(tidyterra)) {
    stop("overlap_map requires tidyterra package, but it could not be loaded")
  }  
  if (!require(dplyr)) {
    stop("richness_map requires dplyr package, but it could not be loaded")
  }
  if (!require(ggplot2)) {
    stop("richness_map requires ggplot2 package, but it could not be loaded")
  }
  if (!require(rnaturalearth)) {
    stop("overlap_map requires rnaturalearth package, but it could not be loaded")
  }
  if (!require(rnaturalearthdata)) {
    stop("overlap_map requires rnaturalearthdata package, but it could not be loaded")
  }
  
  source(file = "load_functions.R")

  richness_raster <- r

  # Get limits for plot
  plot_ext <- terra::ext(richness_raster) * 1.04
  xlim = c(plot_ext[1], plot_ext[2])
  ylim = c(plot_ext[3], plot_ext[4])

  # Get political boundaries for map  
  boundaries <- rnaturalearth::ne_countries(continent = "north america",
                                            scale = "medium",
                                            returnclass = "sf") %>% 
    dplyr::select(1) %>%
    terra::vect() %>%
    terra::project(y = richness_raster)
  
  # Create plot object
  richness_plot <- ggplot() +
    tidyterra::geom_spatraster(data = richness_raster, 
                               maxcell = Inf) +
    tidyterra::scale_fill_whitebox_c(palette = palette,
                                     direction = direction,
                                     name = legend_name) +
    geom_spatvector(data = boundaries, color = "black", fill = NA) +
    coord_sf(xlim = xlim, ylim = ylim)

  # Add title if available
  if (!is.null(plot_title)) {
    richness_plot <- richness_plot + 
      labs(title = plot_title[1])
  }
  
  # Apply theme
  richness_plot <- richness_plot +
    theme_bw()
   
  return(richness_plot)
}