#' Create overlap maps of insect and hosts
#' 
#' @param species_name character vector with name of insect species, e.g. 
#' "Papilio multicaudata"
#' @param predictor character vector indicating which climate variables on 
#' which predictions are based
#' @param crop_to_insect logical indicating whether plot should be cropped to 
#' range of the insect
#' 
#' @return A list with two elements:
#' \describe{
#'   \item{prop_overlap}{double indicating the proportion of insect's range 
#'   that overlaps with host plant(s)' range}
#'   \item{overlap_plot}{ggplot object showing distribution of insect, host(s),
#'   and their overlap}
#' }
create_overlaps <- function(species_name, 
                            predictor = c("current", "GFDL-ESM4_RCP45"),
                            crop_to_insect = FALSE) {
  if (!require(raster)) {
    stop("create_overlaps requires raster package, but it could not be loaded")
  }
  if (!require(dplyr)) {
    stop("create_overlaps requires dplyr package, but it could not be loaded")
  }
  if (!require(ggplot2)) {
    stop("create_overlaps requires ggplot2 package, but it could not be loaded")
  }
  # if (!require(maptools)) {
  #   stop("create_overlaps requires maptools package, but it could not be loaded")
  # }
  # Load up the functions from the functions folder
  function_files <- list.files(path = "./functions", 
                               pattern = ".R$", 
                               full.names = TRUE)
  for(fun_file in function_files) {
    source(file = fun_file)
  }
  
  predictor <- match.arg(predictor)
  
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species_name))
  
  # identify all host plants used by that insect
  insects_hosts <- read.csv(file = "data/insect-host.csv")
  hosts <- insects_hosts$host[insects_hosts$insect == species_name]

  # Make sure we have the distribution of the insect before proceeding  
  insect_dist_file <- paste0("output/distributions/",
                             nice_name, 
                             "-distribution-svm-",
                             predictor,
                             ".rds")
  
  if (!file.exists(insect_dist_file)) {
    message(paste0("No distributions found for ", species_name, " based on ",
                   predictor, " predictors. No maps created."))
    return(NULL)
  }
  
  # pull in presence / absence raster of insect
  insect_pa <- readRDS(file = insect_dist_file)
  
  # pull in presence / absence rasters of plants this bug eats
  if (length(hosts) >= 1) {
    host_pa_list <- list()
    for (host_name in hosts) {
      host_nice_name <- tolower(x = gsub(pattern = " ",
                                         replacement = "_",
                                         x = host_name))
      host_dist_file <- paste0("output/distributions/",
                               host_nice_name,
                               "-distribution-svm-",
                               predictor,
                               ".rds")
      # Only try to add distributions if the file exists
      if (file.exists(host_dist_file)) {
        host_pa_list[[host_nice_name]] <- readRDS(file = host_dist_file)
      } else {
        message(paste0("Skipping host ", host_name, ", no distribution found."))
      }
    } # end iterating over all host species
    
    # Make sure at least only host had a map, otherwise go no further, message, 
    # and return NULL
    if (length(host_pa_list) == 0) {
      message(paste0("No corresponding host distributions for ", species_name,
                     " could be found. No map created."))
      return(NULL)
    }
    
    # Stack all those rasters (should work even for monophagous species)
    host_pa <- stack_rasters(r = host_pa_list, out = "binary")
    # Will need to set values to 2 for hosts to get different colors
    host_pa[host_pa >= 1] <- 2
    
    # Finally, we do one more stack, 
    all_pa <- stack_rasters(r = list(insect_pa, host_pa))
    
    if (crop_to_insect) {
      all_pa <- raster::crop(x = all_pa, y = insect_pa)
    }

    # Now we can use ggplot to plot these rasters, but we need to extract 
    # raster information into a data frame
    
    # Convert to data frame (two steps to do so)
    # First, to a SpatialPointsDataFrame
    pa_points <- raster::rasterToPoints(x = all_pa, 
                                        spatial = TRUE)
    # Then to a 'conventional' dataframe
    pa_df  <- data.frame(pa_points)
    rm(pa_points)
    
    # Rename columns so they plot without extra ggplot commands
    pa_df <- pa_df %>%
      dplyr::rename(Longitude = x,
                    Latitude = y)
    
    # Want an abbreviated version of species name for title & legend
    name_split <- unlist(strsplit(x = species_name, split = " "))
    abbr_name <- paste0(substr(x = name_split[1], start = 1, stop = 1),
                        ". ", name_split[2])
    
    # Create column indicating what each layer means
    pa_df <- pa_df %>%
      dplyr::mutate(Status = dplyr::case_when(layer == 0 ~ "Absent",
                                              layer == 1 ~ abbr_name,
                                              layer == 2 ~ "Hosts only",
                                              layer == 3 ~ paste0(abbr_name, " & hosts"))) %>%
      dplyr::mutate(Status = factor(x = Status,
                                    levels = c("Absent", 
                                               "Hosts only",
                                               abbr_name, 
                                               paste0(abbr_name, " & hosts"))))

    color_vec <- c("#e5e5e5",   # Absent
                   "#b2df8a",   # Hosts only
                   "#a6cee3",   # Insect only
                   "#1f78b4")   # Hosts and insect
    
    overlap_plot <- ggplot(data = pa_df, mapping = aes(x = Longitude, y = Latitude, fill = Status)) +
      geom_raster() +
      scale_fill_discrete(type = color_vec) +
      labs(title = paste0(abbr_name, " ", predictor)) +
      coord_equal() + 
      theme_bw() +
      theme(axis.title = element_blank(),
            legend.title = element_blank())
    
    # Now we can create a plot, using maptools for borders
    # data("wrld_simpl")
    
    # We'll need three colors: insect, plants, insect+plants
    # plot_colors <- hcl.colors(n = 3, palette = "Cividis")
    # 
    # plot_file <- paste0("output/maps/", nice_name, "-overlap-", 
    #                     predictor, ".pdf")
    
    # Write to pdf instead of screen
    # pdf(file = plot_file, useDingbats = FALSE)
    #   main_title <- paste0(species_name, " ", predictor)
    #   plot(all_pa, 
    #        main = main_title, 
    #        col = c(NA, plot_colors),
    #        legend = FALSE)
      # Add the map
      # plot(wrld_simpl, 
      #      add = TRUE,
      #      border = "grey30")
      # legend("topleft", 
      #        legend = c("Insect", "Host(s)", "Both"),
      #        fill = plot_colors[1:3])
#    dev.off() # stop writing to disk
    
    # message(paste0("PDF map for ", species_name, " written to ", plot_file))
    
    # Now do calculations for overlaps and return that
    # Calculate frequencies of all possible pixel values
    pixel_freqs <- data.frame(raster::freq(all_pa))
    # Drop row with NA counts
    pixel_freqs <- na.omit(pixel_freqs)
    
    # Count how many pixels are insect only (== 1)
    insect_only <- pixel_freqs$count[pixel_freqs$value == 1]
    # In case where there are NO pixels of insect only, need to set this to 0
    if (length(insect_only) == 0) {
      insect_only <- 0
    }
    
    # Count how many pixels are plant AND insect (== 3)
    insect_plant <- pixel_freqs$count[pixel_freqs$value == 3]
    # If there are no pixels with both, set to 0
    if (length(insect_plant) == 0) {
      insect_plant <- 0
    }
    
    # Calculate proportion of insect overlapping with plant relative to insect total 
    # insect only / (insect + plant AND insect)
    # This is the proportion of the insect's range that overlaps with host range
    prop_overlap <- insect_plant / (insect_only + insect_plant)
    return(list(prop_overlap = prop_overlap,
                overlap_map = overlap_plot))
    
  } else {
    # No host plants listed in file, message and return NULL
    message(paste0("No host plants listed for ", species_name, ". No map created"))
    return(NULL)
  }
}