# Look at richness in protected and unprotected areas
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-12-01

require(dplyr)
require(tidyr)
require(terra)
require(exactextractr)
require(ggplot2)
require(weights) # weighted t-tests

# TODO: for the protected areas vectors, we still have Hawaii included. Will 
# Need to crop these vectors to same geographic extent (or, at least, crop any 
# assets we create with them to same geographic extent)

# Want to know average richness in protected vs. unprotected areas (maybe not?)
#    TODO: For the above, may need to decide which areas to look at - all of 
#          North America?
# Want to compare richness in protected areas in current vs. future climates

# TODO: If we want to combine protected areas polygons (say, to identify those
# areas with NO protection), see terra::combineGeoms(). Behavior is a little 
# odd, as it combines the second set of polygons (y) to the first geometry 
# of the other set of polygons (x). Getting a single polygon will then require 
# three steps. e.g.
# national + state
# (national+state) + local
# (national+state+local) + private
# UPDATE: try terra::aggregate instead

# To get areas with no protection, create a singl SpatVector polygon that is 
# the extent of the area and create a difference. maybe with terra::symdif, 
# passing the large rectangle as the x, and the protected areas SpatVector as 
# the y

# TODO: Since polygons will often only cover part of a grid cell (basically 
# the unit of measurement here), we should include coverage as a weight. The 
# weights package has a weighted t-test function, wtd.t.test() we can use.

# Read in protected areas info (may take 3 minutes or so)
shpfile_path <- "data/protected-areas/protected-areas-categorized.shp"
message("Reading protected areas shapefile (may take a few minutes)...")
protected_areas <- terra::vect(shpfile_path)
message("Protected areas shapefile read.")

# Also read in the unprotected areas shapefile
unprotected_path <- "data/protected-areas/unprotected-areas.shp"
message("Reading unprotected areas shapefile (may take several minutes)...")
unprotected_areas <- terra::vect(unprotected_path)
message("Unprotected areas shapefile read.")

# TODO: UNTESTED!!!!
# Add that no protection SpatVector to the protected areas object
protected_areas <- rbind(protected_areas, unprotected_areas)
rm(unprotected_areas)
gc()
################################################################################
# Testing terra vector manipulation
# top left: 39.65, -120.45
# bottom right: 38.15, -117.60

# Use a little rectangle from Nevada / California border for testing
small_ext <- terra::ext(c(-120.45, -117.60, 38.15, 39.65))
# Crop the protected areas SpatVector and plot for reality check
small_pa <- terra::crop(protected_areas, small_ext)
plot(small_pa, col = c("green", "red", "blue", "orange"))

# Combine the four different geometries in the protected areas SpatVector to 
# have a single polygon of protected areas. We do this so we can ultimately 
# create a SpatVector of areas that have no protection
small_all_pa <- terra::aggregate(small_pa)

# Create a rectangle that covers the extent of the protected areas polygon
small_area_rect <- terra::vect(terra::ext(small_pa))
# Use terra's symdif() to substract the protected areas polygons from the 
# rectangle covering the extent. The resultant SpatVector is the polygon of 
# areas that have no protection. Plot for reality check; unprotected areas are 
# in orange, and protected areas in green
small_no_protect <- terra::symdif(small_area_rect, small_all_pa)
plot(small_no_protect, col = "#d95f02", alpha = 0.3)
plot(small_all_pa, col = "#1b9e77", alpha = 0.6, add = TRUE)

# End test of vector (polygon) manipulation
################################################################################

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
                                      progress = TRUE)
# Resultant list is unnamed, so we want to name it
names(current_cell_richness) <- data.frame(protected_areas)$AGNCY_SHOR

# We can use exact_extract to get summary stats, too, with means and standard 
# deviation weighted by cell coverage
cell_stats <- c("mean", "stdev", "count")
summary_stats <- exactextractr::exact_extract(x = current_richness,
                                              y = sf::st_as_sf(protected_areas),
                                              progress = TRUE,
                                              fun = cell_stats,
                                              force_df = TRUE)
# TODO: It would be nice if we could get a count of how many cells are not NA, 
# although maybe they are ignored. The documentation for exact_extract() says:
# "In all of the summary operations, NA values in the the primary raster (x) 
# raster are ignored (i.e., na.rm = TRUE.)"
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

  # Get summary stats for each protected area type
  message("Calculating summary stats for ", model_name_short)
  summary_stats <- exactextractr::exact_extract(x = forecast_richness,
                                                y = sf::st_as_sf(protected_areas),
                                                progress = FALSE,
                                                fun = cell_stats,
                                                force_df = TRUE)
  summary_stats <- cbind(agency = data.frame(protected_areas)$AGNCY_SHOR, 
                         summary_stats)
  protected_stats[[model_name_short]] <- summary_stats
  
  # Instead of doing paired t-test with two vectors, do paired t-test with a 
  # single vector that is the difference between current richness and forecast 
  # richness. Null is that the value is 0.
  message("Running t-test on current vs forecast richness for ", model_name_short)
  # We need to make sure rasters are the same extent, so do a pair of crops as 
  # we are doing the math
  delta_richness = terra::crop(forecast_richness, current_richness) - 
    terra::crop(current_richness, forecast_richness)
  
  delta_cell_richness <- exactextractr::exact_extract(x = delta_richness,
                                                      y = sf::st_as_sf(protected_areas),
                                                      progress = FALSE)
  names(delta_cell_richness) <- data.frame(protected_areas)$AGNCY_SHOR

  # Use protection level as iterator with lapply
  prot_level <- names(delta_cell_richness)
  # Extract t-test results, elements of a t-test object that we are interested 
  # in: p.value, estimate, conf.int (a two-element vector), statistic, 
  # parameter (df in this case)
  # richness_t_test <- lapply(X = prot_level,
  #                           FUN = function(x, delta_cells) {
  #                             delta_t <- t.test(x = delta_cells[[x]][, "value"],
  #                                               mu = 0)
  #                             delta_t <- data.frame(t = delta_t$statistic,
  #                                                   df = delta_t$parameter,
  #                                                   p = delta_t$p.value,
  #                                                   estimate = delta_t$estimate,
  #                                                   lwr = delta_t$conf.int[1],
  #                                                   upr = delta_t$conf.int[2],
  #                                                   comparison = "forecast - current")
  #                             rownames(delta_t) <- NULL
  #                             return(delta_t)
  #                           },
  #                           delta_cells = delta_cell_richness)
  # t-test using coverage_fraction as weight
  richness_t_test <- lapply(X = prot_level,
                            FUN = function(x, delta_cells) {
                              delta_t <- wtd.t.test(x = delta_cells[[x]][, "value"],
                                                    y = 0,
                                                    weight = delta_cells[[x]][, "coverage_fraction"])
                              delta_t <- data.frame(t = delta_t$coefficients["t.value"],
                                                    df = delta_t$coefficients["df"],
                                                    p = delta_t$coefficients["p.value"],
                                                    estimate = delta_t$additional["Mean"],
                                                    sterr = delta_t$additional["Std. Err"],
                                                    comparison = "forecast - current")
                              rownames(delta_t) <- NULL
                              return(delta_t)
                            },
                            delta_cells = delta_cell_richness)
  
  names(richness_t_test) <- prot_level
  richness_t[[model_name_short]] <- richness_t_test %>%
    bind_rows(.id = "protection_level")

  make_histograms <- FALSE
  if (make_histograms) {  
    # Since we have the deltas for each cell, create a histogram of values, for 
    # each type of protected area (four histograms per climate model).
    delta_cell_rich_df <- delta_cell_richness %>%
      bind_rows(.id = "AGNCY_SHOR") %>%
      tidyr::drop_na() %>%
      mutate(AGNCY_SHOR = factor(AGNCY_SHOR,
                                 levels = c("National", "State",
                                            "Local", "Private", "None")))
    
    # TODO: Coverage is more often that not only part of the grid cell. That is, 
    # the polygons of the protected area may only be part of the grid cell. What 
    # sort of impact would this have on our analyses?
    ggplot(data = delta_cell_rich_df, 
           mapping = aes(x = value)) +
      geom_histogram(binwidth = 1) + 
      facet_wrap(~ AGNCY_SHOR, nrow = 2, scales = "free_y") +
      theme_bw()
  }

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
          file = "output/summary-stats/protected-areas-richness-t.csv",
          row.names = FALSE)

# Do a plot of richness, comparing current to forecast. For now, just do the 
# SSP3-7.0 2041; four panes: one for each protection level
protected_stats <- read.csv(file = "output/summary-stats/protected-areas-richness.csv")
protected_for_plot <- protected_stats %>%
  filter(climate_model %in% c("current", "ssp370_2041"))

protected_for_plot <- protected_for_plot %>%
  mutate(agency = factor(x = agency,
                         levels = c("National", "State", "Local", "Private", "None")))

protected_scatterplot <- ggplot(data = protected_for_plot,
                                mapping = aes(x = climate_model,
                                              y = mean)) +
  geom_point() +
  geom_errorbar(mapping = aes(ymin = mean - (stdev/sqrt(count)),
                              ymax = mean + (stdev/sqrt(count))),
                width = 0.3) +
  facet_wrap(~ agency, scales = "free_y") +
  ylab("Mean # species") +
  theme_bw() +
  theme(axis.title.x = element_blank())
protected_scatterplot

ggplot(data = protected_for_plot,
       mapping = aes(x = agency,
                     y = mean,
                     color = climate_model)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(mapping = aes(ymin = mean - (stdev/sqrt(count)),
                              ymax = mean + (stdev/sqrt(count))),
                width = 0.3,
                position = position_dodge(width = 0.4)) +
  ylab("Mean # species") +
  theme_bw() +
  theme(axis.title.x = element_blank())
