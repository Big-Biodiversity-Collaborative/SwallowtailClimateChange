#' A ggplot object of range overlaps
#' 
#' @param species_name character vector with name of insect species, e.g. 
#' "Papilio multicaudata"
#' @param predictor character vector indicating which climate variables on 
#' which predictions are based
#' @param model character vector of model used to generate species distribution
#' model
#' @param crop_to_insect logical indicating whether plot should be cropped to 
#' range of the insect
overlap_map <- function(species_name, 
                            predictor = c("current", "GFDL-ESM4_RCP45"),
                            model = c("glm", "svm"),
                            crop_to_insect = FALSE) {
  if (!require(raster)) {
    stop("overlap_map requires raster package, but it could not be loaded")
  }
  if (!require(dplyr)) {
    stop("overlap_map requires dplyr package, but it could not be loaded")
  }
  if (!require(ggplot2)) {
    stop("overlap_map requires ggplot2 package, but it could not be loaded")
  }
  # Load up the functions from the functions folder
  function_files <- list.files(path = "./functions", 
                               pattern = ".R$", 
                               full.names = TRUE)
  for(fun_file in function_files) {
    source(file = fun_file)
  }
  
  predictor <- match.arg(predictor)
  model <- match.arg(model)
  
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species_name))
  
  overlap_file <- paste0("output/ranges/",
                         nice_name, 
                         "-overlap-",
                         model, 
                         "-",
                         predictor, 
                         ".rds")
  
  if (!file.exists(overlap_file)) {
    message(paste0("Could not find overlap file for ", species_name, 
                   "; no map produced"))
    return(NULL)
  }
  
  overlap <- readRDS(file = overlap_file)
  
  # If requested, crop the raster to extent of insect
  if (crop_to_insect) {
    # We need to determine geographic extent of insect (values of 1 and 3) in
    # order to do crop
    insect <- overlap
    # First classify pixels that are not 1 or 3 as NA
    insect <- raster::reclassify(x = insect, 
                                 rcl = matrix(data = c(0, NA, 2, NA), 
                                              ncol = 2, byrow = TRUE))
    
    # Make sure there are at least some non-NA pixels
    if (nrow(raster::freq(x = insect, useNA = "no")) > 0) {
      # Trim the raster to only include extent of non-NA pixels
      insect <- raster::trim(x = insect)
      # Get the extent of the insect
      insect_extent <- extent(insect)
      # Crop the overlap raster and go from there
      overlap <- raster::crop(x = overlap, y = insect_extent)
    } else {
      # No cells were found with insect, so cannot use crop to insect
      warning(paste0("No pixels indicated presence of ", species_name, 
                     " in overlap raster file ", overlap_file, 
                     ". Map will not be cropped."))
    }
  }
  
  # Use ggplot to plot these rasters, but we need to extract raster information 
  # into a data frame
  
  # Convert to data frame (two steps to do so)
  # First, to a SpatialPointsDataFrame
  overlap_points <- raster::rasterToPoints(x = overlap, 
                                           spatial = TRUE)
  # Then to a dataframe
  overlap_df <- data.frame(overlap_points)
  rm(overlap_points)
  
  # Rename columns so they plot without extra ggplot commands
  overlap_df <- overlap_df %>%
    dplyr::rename(Longitude = x,
                  Latitude = y)
  
  # Want an abbreviated version of species name for title & legend
  name_split <- unlist(strsplit(x = species_name, split = " "))
  abbr_name <- paste0(substr(x = name_split[1], start = 1, stop = 1),
                      ". ", name_split[2])
  and_hosts <- paste0(abbr_name, " & hosts")
  
  # Create column indicating what each layer means
  status_levels <- c("Absent", 
                     "Hosts only",
                     abbr_name, 
                     and_hosts)

  # TODO: Better way of doing this?
  # NOTE NOTE NOTE!!! level indexes differ from layer values
  overlap_df <- overlap_df %>%
    dplyr::mutate(Status = dplyr::case_when(layer == 0 ~ status_levels[1],
                                            layer == 1 ~ status_levels[3],
                                            layer == 2 ~ status_levels[2],
                                            layer == 3 ~ status_levels[4])) %>%
    dplyr::mutate(Status = factor(x = Status,
                                  levels = status_levels))
  
  color_vec <- c("#e5e5e5",   # Absent
                 "#b2df8a",   # Hosts only
                 "#a6cee3",   # Insect only
                 "#1f78b4")   # Hosts and insect
  
  names(color_vec) <- status_levels
  
  overlap_plot <- ggplot(data = overlap_df, 
                         mapping = aes(x = Longitude, 
                                       y = Latitude, 
                                       fill = Status)) +
    geom_raster() +
    scale_fill_manual(values = color_vec) +
    labs(title = paste0(abbr_name, " ", predictor)) +
    coord_equal() + 
    theme_bw() +
    theme(axis.title = element_blank(),
          legend.title = element_blank())
  
  return(overlap_plot)  
}