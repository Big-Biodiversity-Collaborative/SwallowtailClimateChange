# Non-parametric tests comparing east vs west suitability shift differences
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-10-08

library(dplyr)

# Load suitable area calculations; created by 
# src/summary/summary-2-compare-ranges.R, and written to 
# output/summary-stats/overlap-summary-allspp.csv

range_info <- read.csv(file = "output/summary-stats/overlap-summary-allspp.csv")
ew <- read.csv(file = "data/insect-eastwest.csv")

range_info <- range_info %>%
  left_join(ew, by = c("insect" = "insect"))

# Now select the columns we want and climate model of interest
climate_model <- "ssp370_2041"
range_info <- range_info %>%
  filter(climate == climate_model) %>%
  filter(distribution == "insect + host") %>%
  select(insect, ew, lat_min_05_shift, lat_max_05_shift, lon_min_05_shift, 
         lon_max_05_shift)

# Now would be a great spot for lapply or a purrr something...

# Test for difference in southern edge shift
south_shift <- wilcox.test(formula = lat_min_05_shift ~ ew,
                           data = range_info,
                           alternative = "two.sided",
                           paired = FALSE)

# Test for difference in northern edge shift
north_shift <- wilcox.test(formula = lat_max_05_shift ~ ew,
                           data = range_info,
                           alternative = "two.sided",
                           paired = FALSE)

# Test for difference in western edge shift
west_shift <- wilcox.test(formula = lon_min_05_shift ~ ew,
                          data = range_info,
                          alternative = "two.sided",
                          paired = FALSE)

# Test for difference in eastern edge shift
east_shift <- wilcox.test(formula = lon_max_05_shift ~ ew,
                          data = range_info,
                          alternative = "two.sided",
                          paired = FALSE)

# write the stats to output
sink(file = "output/summary-stats/shift-nonparametric.txt")
test_results <- south_shift

cat("Southern edge shifts", "\n")
cat("method: ", test_results$method, "\n")
cat("W = ", test_results$statistic, "\n")
cat("p = ", test_results$p.value, "\n")
cat("alternative: ", test_results$alternative, "\n")

test_results <- north_shift

cat("\n***\n\nNorthern edge shifts", "\n")
cat("method: ", test_results$method, "\n")
cat("W = ", test_results$statistic, "\n")
cat("p = ", test_results$p.value, "\n")
cat("alternative: ", test_results$alternative, "\n")

test_results <- west_shift

cat("\n***\n\nWestern edge shifts", "\n")
cat("method: ", test_results$method, "\n")
cat("W = ", test_results$statistic, "\n")
cat("p = ", test_results$p.value, "\n")
cat("alternative: ", test_results$alternative, "\n")

test_results <- east_shift

cat("\n***\n\nEastern edge shifts", "\n")
cat("method: ", test_results$method, "\n")
cat("W = ", test_results$statistic, "\n")
cat("p = ", test_results$p.value, "\n")
cat("alternative: ", test_results$alternative, "\n")

sink()