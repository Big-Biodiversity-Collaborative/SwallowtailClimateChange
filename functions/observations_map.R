#' A ggplot object of observations points included in SDMs
#' 
#' @param species_name character vector with name of insect species, e.g. 
#' "Papilio multicaudata"
observations_map <- function(species_name) {
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
    stop("overlap_map requires rnaturalearth package, but it could not be loaded")
  }
  if (!require(rnaturalearthdata)) {
    stop("overlap_map requires rnaturalearthdata package, but it could not be loaded")
  }
  
  # Want an abbreviated version of species name for title & legend
  name_split <- unlist(strsplit(x = species_name, split = " "))
  abbr_name <- paste0(substr(x = name_split[1], start = 1, stop = 1),
                      ". ", name_split[2])
  title_text <- paste0(abbr_name, " filtered observations")

  # Load in post-filtering observation data
  nice_name <- gsub(pattern = " ",
                    replacement = "_",
                    x = tolower(species_name))
  obs_file <- paste0("data/gbif/presence-absence/",
                     nice_name, "-pa.csv")
  
  if (!file.exists(obs_file)) {
    message("No observations file found for ", species_name, 
            "; no map produced")
    return(NULL)
  } else {
    observations <- read.csv(file = obs_file)
    observations <- observations %>%
      dplyr::filter(pa == 1) %>% # pa == 1 is "present" point
      dplyr::rename(Longitude = x, Latitude = y)
    
    # Get limits for plot (add 4% to each direction)
    min_lon <- min(observations$Longitude)
    max_lon <- max(observations$Longitude)
    min_lat <- min(observations$Latitude)
    max_lat <- max(observations$Latitude)
    
    plot_ext <- terra::ext(c(min_lon, max_lon, min_lat, max_lat)) * 1.04
    xlim = c(plot_ext[1], plot_ext[2])
    ylim = c(plot_ext[3], plot_ext[4])
    
    # Get North American political boundaries map  
    boundaries <- rnaturalearth::ne_countries(continent = "north america",
                                              scale = "medium",
                                              returnclass = "sf") %>% 
      dplyr::select(1) 
    
    # Create ggplot object, adding political boundaries first, then adding points
    observations_plot <- ggplot() +
      geom_spatvector(data = boundaries, color = "black", fill = NA) +
      geom_point(data = observations, 
                 mapping = aes(x = Longitude,
                               y = Latitude),
                 size = 0.75,
                 alpha = 0.5) +
      coord_sf(xlim = xlim, ylim = ylim) +
      labs(title = title_text) +
      theme_bw() +
      theme(axis.title = element_blank())
    
    return(observations_plot)
  }
}