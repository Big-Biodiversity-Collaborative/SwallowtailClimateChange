#' DEPRECATED
#' 
#' A ggplot object of a species' predicted range
#' 
#' @param species_name character vector with name of species, e.g. 
#' "Papilio multicaudata"
#' @param predictor character vector indicating whether we want a map based 
#' on current climate or one of future climate scenarios (projections)
#' @param model character vector of model used to generate species distribution
#' model
#' @param show_obs logical indicating whether GBIF observations that were used 
#' in the SDM should be overlaid on the current predicted distribution map
#' 
#' @return a ggplot object of a species' distribution
#' 
#' @examples
#' \dontrun{
#' my_plot <- single_map(species_name = "Papilio brevicauda",
#'                       predictor = "current",
#'                       model = "maxent-notune",
#'                       show_obs = TRUE)
#' print(my_plot)
#' }
single_map <- function(species_name, 
                       predictor,
                       model = c("brt", "gam", "glm", "lasso", "maxent-notune", 
                                 "maxent-tune", "rf", "svmw"),
                       show_obs = FALSE) {
  if (!require(raster)) {
    stop("single_map requires raster package, but it could not be loaded")
  }
  if (!require(dplyr)) {
    stop("single_map requires dplyr package, but it could not be loaded")
  }
  if (!require(ggplot2)) {
    stop("single_map requires ggplot2 package, but it could not be loaded")
  }

  model <- match.arg(model)
  
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species_name))
  
  if (show_obs == TRUE & predictor == "current") {
    
    pa_file <- paste0("data/gbif/presence-absence/",
                      tolower(genus), "_", species, "-pa.csv")
    if (!file.exists(pa_file)) {
      message(paste0("Could not find presence-absence file for ", species_name, 
                     "; no map produced"))
      return(NULL)
    }
    
    pa <- read.csv(pa_file)
    
    obs <- pa %>%
      dplyr::filter(pa == 1) %>%
      dplyr::rename(Longitude = x, Latitude = y)
  }  
  
  distribution_file <- paste0("output/distributions/",
                              nice_name, 
                              "-distribution-",
                              model, 
                              "-",
                              predictor, 
                              ".rds")
  
  if (!file.exists(distribution_file)) {
    message(paste0("Could not find distribution file for ", species_name, 
                   "; no map produced"))
    return(NULL)
  }
  
  distribution <- readRDS(file = distribution_file)
  
  # Use ggplot to plot the raster, but we need to extract raster information 
  # into a data frame
  
  # Convert to data frame (two steps to do so)
  # First, to a SpatialPointsDataFrame
  distribution_points <- raster::rasterToPoints(x = distribution, 
                                                spatial = TRUE)
  # Then to a dataframe
  distribution_df <- data.frame(distribution_points)
  rm(distribution_points)
  
  # Rename columns so they plot without extra ggplot commands
  distribution_df <- distribution_df %>%
    dplyr::rename(Longitude = x,
                  Latitude = y)
  
  # Want an abbreviated version of species name for title & legend
  name_split <- unlist(strsplit(x = species_name, split = " "))
  abbr_name <- paste0(substr(x = name_split[1], start = 1, stop = 1),
                      ". ", name_split[2])
  
  status_levels <- c("Absent", 
                     "Present")
  
  distribution_df <- distribution_df %>%
    mutate(Status = dplyr::case_when(layer == 0 ~ status_levels[1],
                                     layer == 1 ~ status_levels[2])) %>%
    dplyr::mutate(Status = factor(x = Status,
                                  levels = status_levels))
  
  status_colors <- c("#e5e5e5",   # Absent
                     "#2ca25f")   # Present
  
  names(status_colors) <- status_levels
  
  distribution_plot <- ggplot(data = distribution_df, 
                              mapping = aes(x = Longitude, 
                                            y = Latitude, 
                                            fill = Status)) +
    geom_raster() +
    scale_fill_manual(values = status_colors) +
    labs(title = paste0(abbr_name, " ", predictor)) +
    coord_equal() + 
    theme_bw() +
    theme(axis.title = element_blank(),
          legend.title = element_blank())
  
  if (show_obs == TRUE & predictor == "current") {  
    obs_points <- geom_point(data = obs,
                             aes(x = Longitude, 
                                 y = Latitude,
                                 fill = NULL),
                             show.legend = FALSE)
    distribution_wpoints <- distribution_plot + obs_points
    return(distribution_wpoints)
  } else {
    return(distribution_plot)
  }
}