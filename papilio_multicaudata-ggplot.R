# Testing ggplot maps of distributions
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-05

library(raster)
library(dplyr)   # some data wrangling
library(ggplot2)
library(RColorBrewer)

distribution <- readRDS(file = "output/distributions/papilio_multicaudata-distribution-svm-current.rds")

# May need to convert to data frame?
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

# dist_df$layer <- factor(dist_df$layer)

# Rename columns so they plot without extra ggplot commands
dist_df <- dist_df %>%
  dplyr::rename(Longitude = x,
                Latitude = y)

ggplot(data = dist_df, mapping = aes(x = Longitude, y = Latitude, fill = PA)) +
  geom_raster() +
  scale_fill_discrete() +
  coord_equal() + 
  theme_bw()
