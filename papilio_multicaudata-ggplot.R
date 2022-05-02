# Testing ggplot maps of distributions
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-05

require(raster)
require(dplyr)   # some data wrangling
require(ggplot2)
require(RColorBrewer)

distribution <- readRDS(file = "output/distributions/papilio_multicaudata-distribution-svm-current.rds")

# Need to convert to data frame (two steps to do so)
# First, to a SpatialPointsDataFrame
dist_points <- raster::rasterToPoints(x = distribution, 
                                      spatial = TRUE)
# Then to a 'conventional' dataframe
dist_df  <- data.frame(dist_points)
rm(dist_points)

# Create column indicating presence/absence
dist_df <- dist_df %>%
  mutate(PA = if_else(condition = layer == 0, 
                      true = "Absent",
                      false = "Present"))

# Rename columns so they plot without extra ggplot commands
dist_df <- dist_df %>%
  dplyr::rename(Longitude = x,
                Latitude = y)

ggplot(data = dist_df, mapping = aes(x = Longitude, y = Latitude, fill = PA)) +
  geom_raster() +
  scale_fill_discrete(type = c("Absent" = "#e5e5e5", "Present" = "#5ab4ac")) +
  labs(title = "Papilio multicaudata") +
  coord_equal() + 
  theme_minimal() +
  theme(axis.title = element_blank(),
        legend.title = element_blank())

# Doing plots of multiple hosts

host1 <- readRDS(file = "output/distributions/fraxinus_anomala-distribution-svm-current.rds")
host2 <- readRDS(file = "output/distributions/ptelea_trifoliata-distribution-svm-current.rds")

r <- list(distribution, host1, host2)

# Process for stacking insect and host rasters; handled now by stack_rasters.R
# Need to find largest extent of everything
x <- r
# Remove names from x because they cause issues?
names(x) <- NULL
x$fun = sum
x$na.rm = TRUE
mosaic_raster <- do.call(raster::mosaic, x)

sum_r <- NULL
for (i in 1:length(r)) {
  one_raster <- r[[i]]
  
  if (is.null(sum_r)) {
    # First one, so just extend the raster to the appropriate extent, leaving
    # missing values as missing, and assign to sum_r
    sum_r <- raster::extend(x = one_raster,
                            y = mosaic_raster)
  } else {
    # for each additional raster, need to have two extentions: One with missing 
    # values as missing, one with missing values as zero
    
    # We use the 0 raster for actual addition, but the NA raster to then turn all 
    # zeros back to that should be NA afterwards
    one_raster_NA <- raster::extend(x = one_raster,
                                    y = mosaic_raster)
    one_raster_0 <- raster::extend(x = one_raster,
                                   y = mosaic_raster,
                                   value = 0)
    # We're adding to the sum_r raster, which has zeros and NAs at this point.
    # Make a copy of sum_r, and convert the NAs to zeros, so we can sum the 
    # rasters
    sum_r_0 <- sum_r
    sum_r_0[is.na(sum_r_0)] <- 0
    
    # Add the two rasters that have missings as zero together
    sum_r <- sum_r_0 + one_raster_0
    
    # Now use indexing of the two rasters that have missing as NA to change 
    # back any cells that are NA in both rasters
    sum_r[is.na(sum_r) & is.na(one_raster_NA)] <- NA
  }
}

# Convert raster to a data frame for use with ggplot
all_points <- raster::rasterToPoints(x = sum_r, 
                                      spatial = TRUE)
# Then to a 'conventional' dataframe
all_df  <- data.frame(all_points)
rm(all_points)

# Rename columns so they plot without extra ggplot commands
all_df <- all_df %>%
  dplyr::rename(Longitude = x,
                Latitude = y)


# Converting layer to factor so it plots correctly
all_df <- all_df %>%
  mutate(layer = factor(layer))

# Plot, but should use overlap_map instead
ggplot(data = all_df, mapping = aes(x = Longitude, y = Latitude, fill = layer)) +
  geom_raster() +
  scale_fill_discrete(type = c("#e5e5e5",     # Absent
                               "#a6cee3",     # Insect only
                               "#b2df8a",     # Hosts only
                               "#1f78b4")) +  # Hosts and insect
  labs(title = "Papilio multicaudata") +
  coord_equal() + 
  theme_minimal() +
  theme(axis.title = element_blank(),
        legend.position = "none")
