#' Make a map of species richness
#' 
#' @param r list of rasters
#' @param predictor the predictor values used in the model to create the 
#' map
#' @param xlim longitudinal limits for map in decimal degrees. Warning: this 
#' will probably not work if map includes international date line
#' @param ylim latitudinal limits for map in decimal degrees.
#' 
#' @details Note that this script does no modeling or predicting, relying on 
#' predictions being passed through the list of rasters, \code{r}.
#' 
#' @return A ggplot object
richness_map <- function(r, 
                         predictor = c("current", "GFDL-ESM4_RCP45"),
                         xlim = c(-170, -50),
                         ylim = c(10, 70)) {
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

  predictor = match.arg(predictor)
  
  # Stack the rasters on top of one another
  bio_raster <- stack_rasters(r = r, out = "total")

  # Make a plot
  
  # First, to a SpatialPointsDataFrame
  bio_points <- raster::rasterToPoints(x = bio_raster, 
                                      spatial = TRUE)
  # Then to a 'conventional' dataframe
  bio_df  <- data.frame(bio_points)
  rm(bio_points)
  
  # Rename columns so they plot without extra ggplot commands
  bio_df <- bio_df %>%
    dplyr::rename(Longitude = x,
                  Latitude = y)

  
  plot_title <- paste0("Species richness, ", predictor, " conditions")
  bio_plot <- ggplot(data = bio_df,
                      mapping = aes(x = Longitude,
                                    y = Latitude,
                                    fill = layer)) +
    geom_raster() +
    ggtitle(label = plot_title) +
    scale_fill_distiller(palette = "YlGn", # BuGn is an alternative
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
  
  return(list(bio_plot = bio_plot,
              rasters = bio_raster))
}