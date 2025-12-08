# Look at richness in protected and unprotected areas
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-12-01

require(dplyr)
require(terra)

# Want to know average richness in protected vs. unprotected areas (maybe not?)
#    TODO: For the above, may need to decide which areas to look at - all of 
#          North America?
# Want to compare richness in protected areas in current vs. future climates

# Read in protected areas info (may take 3 minutes or so)
shpfile_path <- "data/protected-areas/protected-areas-categorized.shp"
message("Reading protected areas shapefile (may take a few minutes)...")
protected_areas <- terra::vect(shpfile_path)
message("Protected areas shapefile read.")

# Load list of climate models
climate_models <- read.csv(file = "data/climate-models.csv")
climate_models <- climate_models %>%
  mutate(name_short = gsub(pattern = "ensemble_",
                           replacement = "",
                           x = name))
  
# Create list to hold summary stats for each model
protected_stats <- vector(mode = "list", length = nrow(climate_models))
names(protected_stats) <- climate_models$name_short

# Create list to hold statistical analysis results
richness_t <- vector(mode = "list", length = nrow(climate_models) - 1)
names(richness_t) <- climate_models$name_short[-1]

# Read in current richness raster
current_richness <- terra::rast(x = "output/richness/current-richness-ov.rds")

# Calculate average richness in protected areas for current climate
# Pull out richness values for each type of protected area (takes less than a 
# minute). We need individual cell richness values for subsequent t-tests
current_cell_richness <- exactextractr::exact_extract(x = current_richness, 
                                      y = sf::st_as_sf(protected_areas),
                                      progress = FALSE)
# Resultant list is unnamed, so we want to name it
names(current_cell_richness) <- data.frame(protected_areas)$AGNCY_SHOR

# We can use exact_extract to get summary stats, too, with means and standard 
# deviation weighted by cell coverage
summary_stats <- exactextractr::exact_extract(x = current_richness,
                                              y = sf::st_as_sf(protected_areas),
                                              progress = FALSE,
                                              fun = c("mean", "stdev"),
                                              force_df = TRUE)
summary_stats <- cbind(agency = data.frame(protected_areas)$AGNCY_SHOR, 
                       summary_stats)
protected_stats[["current"]] <- summary_stats

# Iterate over forecast climate models
forecast_models <- climate_models %>%
  filter(name != "current")

for (model_i in 1:nrow(forecast_models)) {
  model_name <- forecast_models$name[model_i]
  model_name_short <- forecast_models$name_short[model_i]

  # Read in forecast richness raster
  forecast_file <- paste0("output/richness/",
                          model_name, 
                          "-richness-ov.rds")
  forecast_richness <- terra::rast(forecast_file)
    
  # Calculate average richness in protected areas
  # message("Calculating richness and summary stats for ", model_name_short)
  # forecast_cell_richness <- exactextractr::exact_extract(x = forecast_richness, 
  #                                               y = sf::st_as_sf(protected_areas),
  #                                               progress = FALSE)
  # Resultant list is unnamed, so we want to name it
  # names(forecast_cell_richness) <- data.frame(protected_areas)$AGNCY_SHOR
  
  # Get summary stats for each protected area type
  message("Calculating summary stats for ", model_name_short)
  summary_stats <- exactextractr::exact_extract(x = forecast_richness,
                                                y = sf::st_as_sf(protected_areas),
                                                progress = FALSE,
                                                fun = c("mean", "stdev"),
                                                force_df = TRUE)
  summary_stats <- cbind(agency = data.frame(protected_areas)$AGNCY_SHOR, 
                         summary_stats)
  protected_stats[[model_name_short]] <- summary_stats
  
  # Instead of doing paired t-test with two vectors, do paired t-test with a 
  # single vector that is the difference between...
  message("Running t-test on current vs forecast richness for ", model_name_short)
  # We need to make sure rasters are the same extent, so do a pair of crops as 
  # we are doing the math
  delta_richness = terra::crop(current_richness, forecast_richness) - 
    terra::crop(forecast_richness, current_richness)
  
  delta_cell_richness <- exactextractr::exact_extract(x = delta_richness,
                                                      y = sf::st_as_sf(protected_areas),
                                                      progress = FALSE)
  names(delta_cell_richness) <- data.frame(protected_areas)$AGNCY_SHOR

  # Use protection level as iterator with lapply
  prot_level <- names(delta_cell_richness)
  # Extract t-test results, elements of a t-test object that we are interested 
  # in: p.value, estimate, conf.int (a two-element vector), statistic, 
  # parameter (df in this case)
  richness_t_test <- lapply(X = prot_level,
                            FUN = function(x, delta_cells) {
                              delta_t <- t.test(x = delta_cells[[x]][, "value"],
                                                mu = 0)
                              delta_t <- data.frame(t = delta_t$statistic,
                                                    df = delta_t$parameter,
                                                    p = delta_t$p.value,
                                                    estimate = delta_t$estimate,
                                                    lwr = delta_t$conf.int[1],
                                                    upr = delta_t$conf.int[2])
                              rownames(delta_t) <- NULL
                              return(delta_t)
                            },
                            delta_cells = delta_cell_richness)
  
  names(richness_t_test) <- prot_level
  richness_t[[model_name_short]] <- richness_t_test %>%
    bind_rows(.id = "protection_level")
} # End iterating over all forecast climate models

# Combine summary stats list elements into a single data frame
protected_stats <- protected_stats %>%
  bind_rows(.id = "climate_model")

# Write those summary stats to file
write.csv(x = protected_stats,
          file = "output/summary-stats/protected-areas-richness.csv",
          row.names = FALSE)

# Extract t-test results
richness_t <- richness_t %>% 
  bind_rows(.id = "climate_model")

# Multiple comparison correction. Holm-Bonferroni makes sense.
richness_t <- richness_t %>%
  mutate(adj_p = p.adjust(p = p, method = "holm"))
  
write.csv(x = richness_t,
          file = "output/summary-stats/richness-t.csv",
          row.names = FALSE)

