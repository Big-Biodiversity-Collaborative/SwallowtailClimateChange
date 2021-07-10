#' Make a map of species richness
#' 
#' @param r Raster* object where cells values are species richness 
#' @param predictor the predictor values used in the model to create the 
#' map
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
                         predictor = c("current", "GFDL-ESM4_RCP45"),
                         xlim = c(-170, -50),
                         ylim = c(10, 70),
                         pal_limits = NULL) {
  if (!require(raster)) {
    stop("richness_map requires raster package, but it could not be loaded")
  }
  if (!require(dplyr)) {
    stop("richness_map requires dplyr package, but it could not be loaded")
  }
  if (!require(ggplot2)) {
    stop("richness_map requires ggplot2 package, but it could not be loaded")
  }

  source(file = "load_functions.R")

  predictor <- match.arg(predictor)
  
  # Stack the rasters on top of one another
  # bio_raster <- stack_rasters(r = r, out = "total")

  # Make a plot
  
  # First, to a SpatialPointsDataFrame
  bio_points <- raster::rasterToPoints(x = r, 
                                      spatial = TRUE)
  # Then to a 'conventional' dataframe
  bio_df  <- data.frame(bio_points)
  rm(bio_points)
  
  # Rename columns so they plot without extra ggplot commands
  bio_df <- bio_df %>%
    dplyr::rename(Longitude = x,
                  Latitude = y)

  if (is.null(pal_limits)) {
    pal_limits <- c(raster::cellStats(x = r, stat = "min"),
                    raster::cellStats(x = r, stat = "max"))
  }

  plot_title <- paste0("Species richness, ", predictor, " conditions")
  bio_plot <- ggplot(data = bio_df,
                      mapping = aes(x = Longitude,
                                    y = Latitude,
                                    fill = layer)) +
    geom_raster() +
    ggtitle(label = plot_title) +
    scale_fill_distiller(palette = "YlGn", # BuGn is an alternative
                         limits = pal_limits,
                         direction = 1) +  # So 0 is lightest
    coord_equal() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          legend.title = element_blank())
  
  if (!is.null(xlim)) {
    bio_plot <- bio_plot +
      xlim(xlim)
  }
  
  if (!is.null(ylim)) {
    bio_plot <- bio_plot + 
      ylim(ylim)
  }
  
  return(bio_plot)
}