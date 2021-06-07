#' Make a map of biodiversity hotspots
#' 
#' @param r list of rasters
#' @param predictor the predictor values used in the model to create the 
#' biodiversity map
#' 
#' @details Note that this script does no modeling or predicting, relying on 
#' predictions being passed through the list of rasters, \code{r}.
#' 
#' @return A ggplot object
biodiversity_map <- function(r, predictor = c("current", "GFDL-ESM4_RCP45")) {
  if (!require(raster)) {
    stop("biodiversity_map requires raster package, but it could not be loaded")
  }
  if (!require(dplyr)) {
    stop("biodiversity_map requires dplyr package, but it could not be loaded")
  }
  if (!require(ggplot2)) {
    stop("biodiversity_map requires ggplot2 package, but it could not be loaded")
  }
  # Load up the functions from the functions folder
  function_files <- list.files(path = "./functions", 
                               pattern = ".R$", 
                               full.names = TRUE)
  for(fun_file in function_files) {
    source(file = fun_file)
  }
  
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

  
  plot_title <- paste0("Biodiversity, ", predictor, " conditions")
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
  return(bio_plot)
}