#' A ggplot object of range overlaps
#' 
#' @param species_name character vector with name of insect species, e.g. 
#' "Papilio multicaudata"
#' @param overlap_raster raster with overlap between predicted ranges of insect
#' and its hostplants (raster values = 0:3)
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
#' @param title_scenarioyear logical indicating whether the title (for future
#' maps only) should include the scenario and year (title_scenarioyear = TRUE)
#' or just say "future" (title_scenarioyear = FALSE)
overlap_map <- function(species_name, 
                        overlap_raster,
                        clim_model = clim_model,
                        include_legend = TRUE,
                        horizontal_legend = FALSE,
                        generic_legend = FALSE,
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
  
  # Want an abbreviated version of species name for title & legend
  name_split <- unlist(strsplit(x = species_name, split = " "))
  abbr_name <- paste0(substr(x = name_split[1], start = 1, stop = 1),
                      ". ", name_split[2])
  and_hosts <- paste0(abbr_name, " & hosts")
  
  # Plotting details
  # Important note: values in raster are 0:3 with 1 = insect only, 2 = host only 
  # But for plotting, it's nicer to have the levels in this order:
  # 1 = Both absent, 2 = hosts only, 3 = insect only, 4 = both present
  
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
  
  # Set levels in categorical raster (note that we're switching 1 and 2
  # to list "hosts only" before "insect only" in legend)
  overlap2 <- as.factor(overlap)
  levels(overlap2) <- data.frame(value = c(0, 2, 1, 3), desc = labels)
  
  color_vec <- c("#e5e5e5",   # Absent
                          "#b2df8a",   # Hosts only
                          "#a6cee3",   # Insect only
                          "#1f78b4")   # Hosts and insect  
                          
  # Plot title
  if (title_scenarioyear) {
    title_text <- labs(title = paste0(abbr_name, " and hosts, ", clim_model))
  } else {
    future_text <- ifelse(clim_model == "current", "current", "future")
    title_text <- labs(title = paste0(abbr_name, " and hosts, ",future_text)) 
  }
  
  overlap_plot_base <- ggplot() +
    geom_spatraster(data = overlap2, maxcell = Inf) +
    scale_fill_manual(values = color_vec, na.translate = FALSE) +
    title_text +
    theme_bw()
  
  if (include_legend == TRUE & horizontal_legend == TRUE) {
    overlap_plot <- overlap_plot_base +
      theme(axis.title = element_blank(),
            legend.title = element_blank(),
            legend.position = "bottom")
  } else if (include_legend == TRUE & horizontal_legend == FALSE) {
    overlap_plot <- overlap_plot_base +
      theme(axis.title = element_blank(),
            legend.title = element_blank())    
  } else {
    overlap_plot <- overlap_plot_base +
      theme(axis.title = element_blank(),
            legend.position = "None")    
  }
  
  return(overlap_plot)  
}
