# A template for building delta rasters for an insect species from SVM
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-07-09

source(file = "load_functions.R")

genus <- "GENUS"
species <- "SPECIES"

model <- "svm"
output_format <- "png" # "pdf"

# Name for reporting
species_name <- paste0(genus, " ", species)
# A more compute-friendly name
nice_name <- tolower(paste0(genus, "_", species))

# Load in the two overlap rasters
predictors = c("current", "GFDL-ESM4_RCP45")

current_file <- paste0("output/ranges/",
                       nice_name, 
                       "-overlap-",
                       model, 
                       "-",
                       predictors[1], 
                       ".rds")

forecast_file <- paste0("output/ranges/",
                       nice_name, 
                       "-overlap-",
                       model, 
                       "-",
                       predictors[2], 
                       ".rds")

if (file.exists(current_file) & file.exists(forecast_file)) {
  current_raster <- readRDS(file = current_file)
  forecast_raster <- readRDS(file = forecast_file)  
  
  # For current raster
  # Change values of 1 and 2 to 0
  # Change values of 3 to 1
  current_raster[current_raster < 3] <- 0
  current_raster[current_raster == 3] <- 1
  
  # For forecast raster
  # Change values of 1 and 2 to 0
  # Change values of 3 to 2
  forecast_raster[forecast_raster < 3] <- 0
  forecast_raster[forecast_raster == 3] <- 2
  
  # Stack these rasters together using stack_raster with out = "total"
  # 0: neither raster had the insect overlapping with host at location
  # 1: only current raster had the insect overlapping with host
  # 2: only forecast raster had the insect overlapping with host
  # 3: both current and forecast raster had insect overlapping with host
  combined_raster <- stack_rasters(r = list(current_raster, forecast_raster),
                                   out = "total")
    
  # Create a ggplot object, borrowing code from overlap_map
  # Convert to data frame (two steps to do so)
  # First, to a SpatialPointsDataFrame
  combined_points <- raster::rasterToPoints(x = combined_raster, 
                                           spatial = TRUE)
  # Then to a dataframe
  combined_df <- data.frame(combined_points)
  rm(combined_points)
  
  # Rename columns so they plot without extra ggplot commands
  combined_df <- combined_df %>%
    dplyr::rename(Longitude = x,
                  Latitude = y)
  
  # Want an abbreviated version of species name for title
  name_split <- unlist(strsplit(x = species_name, split = " "))
  abbr_name <- paste0(substr(x = name_split[1], start = 1, stop = 1),
                      ". ", name_split[2])
  
  # Create column indicating what each layer means
  status_levels <- c("Absent", 
                     "Contraction",
                     "Expansion", 
                     "Stable")
  
  combined_df <- combined_df %>%
    dplyr::mutate(Status = dplyr::case_when(layer == 0 ~ status_levels[1],
                                            layer == 1 ~ status_levels[2],
                                            layer == 2 ~ status_levels[3],
                                            layer == 3 ~ status_levels[4])) %>%
    dplyr::mutate(Status = factor(x = Status,
                                  levels = status_levels))
  
  color_vec <- c("#e5e5e5",   # Absent
                 "#af8dc3",   # Contraction (only in current)
                 "#7fbf7b",   # Expansion (only in forecast)
                 "#f6e8c3")   # Stable
  
  names(color_vec) <- status_levels
  
  combined_plot <- ggplot(data = combined_df, 
                         mapping = aes(x = Longitude, 
                                       y = Latitude, 
                                       fill = Status)) +
    geom_raster() +
    scale_fill_manual(values = color_vec) +
    labs(title = paste0(abbr_name, " range dynamics")) +
    coord_equal() + 
    theme_bw() +
    theme(axis.title = element_blank(),
          legend.title = element_blank()) +
    xlim(c(-170, -50)) +
    ylim(c(10, 70))
 
  plot_file <- paste0("output/maps/",
                      nice_name,
                      "-delta-",
                      model, 
                      ".", 
                      output_format)
  ggsave(filename = plot_file,
         plot = combined_plot)
  
} else {
  warning(paste0("One or more overlap files for ", 
                 species_name, 
                 " could not be found; no delta map created."))
}