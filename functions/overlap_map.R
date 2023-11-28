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
#' @param boundaries logical indicating whether or not to include political 
#' boundaries 
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
                        boundaries = TRUE,
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
  and_hosts <- paste0(abbr_name, " & hosts")
  
  # Important note: values in raster are 0:5, with 
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
  
  # Get limits for plot
  plot_ext <- ext(overlap2) * 1.04
  xlim = c(plot_ext[1], plot_ext[2])
  ylim = c(plot_ext[3], plot_ext[4])
  
  if (boundaries) {
    boundaries <- rnaturalearth::ne_countries(continent = "north america",
                                              scale = "medium",
                                              returnclass = "sf") %>% 
      dplyr::select(1) %>%
      terra::vect() %>%
      terra::project(y = overlap2)

    # If we are drawing boundaries, we do so in two steps, with two calls 
    # to geom_spatvector():
    # 1. Add the *area*, filled in with the same color as "absent"
    # 2. Add the *outline*, colored black
    # The overlap raster information is plotted between these two calls
    overlap_plot_base <- ggplot() +
      geom_spatvector(data = boundaries, color = NA, fill = color_vec[1]) +
      geom_spatraster(data = overlap2, maxcell = Inf) +
      scale_fill_manual(name = "desc", values = color_vec, na.translate = FALSE) +
      geom_spatvector(data = boundaries, color = "black", fill = NA) +
      coord_sf(xlim = xlim, ylim = ylim) +
      title_text +
      theme_bw()
  } else {
    overlap_plot_base <- ggplot() +
      geom_spatraster(data = overlap2, maxcell = Inf) +
      scale_fill_manual(name = "desc", values = color_vec, na.translate = FALSE) +
      coord_sf(xlim = xlim, ylim = ylim) +
      title_text +
      theme_bw()
  }
  
  if (include_legend == TRUE & horizontal_legend == TRUE) {
    overlap_plot <- overlap_plot_base +
      theme(axis.title = element_blank(),
            legend.title = element_blank(),
            legend.position = "bottom",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 8),
            plot.title = element_text(size = 10))
  } else if (include_legend == TRUE & horizontal_legend == FALSE) {
    overlap_plot <- overlap_plot_base +
      theme(axis.title = element_blank(),
            legend.title = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 8),
            plot.title = element_text(size = 10))    
  } else {
    overlap_plot <- overlap_plot_base +
      theme(axis.title = element_blank(),
            legend.position = "None",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 8),
            plot.title = element_text(size = 10))    
  }

  if (obs_points) {
    nice_name <- gsub(pattern = " ",
                      replacement = "_",
                      x = tolower(species_name))
    obs_file <- paste0("data/gbif/presence-absence/",
                       nice_name, "-pa.csv")
    if (file.exists(obs_file)) {
      observations <- read.csv(file = obs_file)
      observations <- observations %>%
        dplyr::filter(pa == 1) %>%
        dplyr::rename(Longitude = x, Latitude = y)
      overlap_plot <- overlap_plot +
        geom_point(data = observations, 
                   mapping = aes(x = Longitude, y = Latitude),
                   color = "red",
                   size = 0.15,
                   shape = 16,
                   alpha = 0.5)
    }
  }
  return(overlap_plot)  
}
