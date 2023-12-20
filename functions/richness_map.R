#' Make a map of species richness
#' 
#' @param r Raster* object where cells values are species richness 
#' @param predictor character vector of the name of the climate model the on 
#' which predictions were made
#' @param xlim longitudinal limits for map in decimal degrees. Warning: this 
#' will probably not work if map includes international date line
#' @param ylim latitudinal limits for map in decimal degrees.
#' @param pal_limits numeric vector of length two indicating limits of color 
#' palette (minimum and maximum). If \code{NULL}, limits will be determined 
#' by values in \code{r}.
#' 
#' @details Note that this script does no modeling or predicting, relying on 
#' predictions being passed through the list of rasters, \code{r}.
#' 
#' @return A ggplot object
richness_map <- function(r, 
                         predictor,
                         xlim = c(-170, -50),
                         ylim = c(10, 70),
                         pal_limits = NULL) {
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
  # Create new richness raster where values are factor
  # richness_raster <- as.factor(richness_raster)
  # levels(richness_raster) <- data.frame(value = 0:max(richness_raster))

  # Get limits for plot
  plot_ext <- terra::ext(richness_raster) * 1.04
  xlim = c(plot_ext[1], plot_ext[2])
  ylim = c(plot_ext[3], plot_ext[4])
  
  boundaries <- rnaturalearth::ne_countries(continent = "north america",
                                            scale = "medium",
                                            returnclass = "sf") %>% 
    dplyr::select(1) %>%
    terra::vect() %>%
    terra::project(y = richness_raster)
  
  richness_plot <- ggplot() +
    tidyterra::geom_spatraster(data = richness_raster, 
                               maxcell = Inf) +
    tidyterra::scale_fill_whitebox_c(palette = "gn_yl",
                                     direction = -1, # so 0 is lightest
                                     name = "Richness") +
    geom_spatvector(data = boundaries, color = "black", fill = NA) +
    coord_sf(xlim = xlim, ylim = ylim) +
    # title_text +
    theme_bw() 
  
  return(richness_plot)
}