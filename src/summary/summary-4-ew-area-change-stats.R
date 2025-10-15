# Compare changes in suitable area between east and west
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-08-11

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
  select(insect, area, area_gained, area_lost, area_retained, ew)

# Calculate proportion current retained, net change (area and percentage)
range_info <- range_info %>%
  mutate(prop_retained = area_retained/(area_lost + area_retained)) %>%
  mutate(area_net_change = area_gained - area_lost) %>%
  mutate(perc_net_change = (area/(area_lost + area_retained) - 1) * 100)
head(range_info)

# First test for proportion retained
mw_prop_retained <- wilcox.test(formula = prop_retained ~ ew,
                                data = range_info,
                                alternative = "less",
                                paired = FALSE)

# Next test the percentage net change
mw_perc_net_change <- wilcox.test(formula = perc_net_change ~ ew,
                                  data = range_info,
                                  alternative = "less",
                                  paired = FALSE)

# Finally, test for net area differences, but I'm not sure this is useful?
mw_net_change <- wilcox.test(formula = area_net_change ~ ew,
                             data = range_info,
                             alternative = "less",
                             paired = FALSE)

# write the stats to output
sink(file = "output/summary-stats/area-change-nonparametric.txt")
test_results <- mw_prop_retained

cat("Proportion current suitable area retained", "\n")
cat("method: ", test_results$method, "\n")
cat("W = ", test_results$statistic, "\n")
cat("p = ", test_results$p.value, "\n")
cat("alternative: ", test_results$alternative, "\n")

test_results <- mw_perc_net_change

cat("\n***\n\nPercentage change in suitable area", "\n")
cat("method: ", test_results$method, "\n")
cat("W = ", test_results$statistic, "\n")
cat("p = ", test_results$p.value, "\n")
cat("alternative: ", test_results$alternative, "\n")

test_results <- mw_net_change

cat("\n***\n\nNet change in suitable area", "\n")
cat("method: ", test_results$method, "\n")
cat("W = ", test_results$statistic, "\n")
cat("p = ", test_results$p.value, "\n")
cat("alternative: ", test_results$alternative, "\n")

sink()
