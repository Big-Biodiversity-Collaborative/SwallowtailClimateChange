# Analyze protected area proportion change
# Jeff Oliver; Erin Zylstra
# jcoliver@arizona.edu; ezylstra@arizona.edu
# 2024-08-07

library(dplyr)
library(lme4)
library(lmerTest)

# Want to know if, on a per species basis, 
# there are differences in proportion protected between
#   + now and 2041-2070
#   + now and 2071-2100
# including east/west as an interaction term

# protected proportion ~ time + region + time x region
# Or, do area and include species as random intercept (probably not possible)
# protected area ~ time + region + time x region + 1|species

protected_areas <- read.csv(file = "output/summary-stats/protected-areas-allspp.csv")

# Only look at areas of overlap with hostplants
protected_areas <- protected_areas %>%
  filter(distribution == "insect + host")

# Create east/west indicator
east <- "appalachiensis|brevicauda|canadensis|cresphontes|glaucus|palamedes|polyxenes|troilus"
west <- "euymedon|indra|machaon|multicaudata|rumiko|rutulus|zelicaon"
protected_areas <- protected_areas %>%
  mutate(ew = if_else(grepl(east, insect), "East", "West"))

# Separate out SSP from time information
# Create a "model_year" variable for the regression analysis
# current = 2010.5
# 2041 = 2055.5
# 2071 = 2085.5
protected_areas <- protected_areas %>%
  mutate(ssp = if_else(substr(climate, 1, 3) == "ssp",
                       true = substr(climate, 1, 6), 
                       false = NA)) %>%
  mutate(model_year = if_else(climate == "current",
                              true = 2010.5,
                              false = as.numeric(substr(climate, 
                                                        8, 11)) + 14.5))

ssps <- c("ssp245", "ssp370", "ssp585")
forecast_years <- c(2055.5, 2085.5)
list_names <- do.call(paste, expand.grid(ssps, forecast_years))

########################################
# Using proportion protected as response, weighting by area
results_list <- vector(mode = "list",
                       length = length(ssps) * length(forecast_years))
names(results_list) <- list_names

for (ssp_i in ssps) {
  for (forecast_year_i in forecast_years) {
    list_name <- paste0(ssp_i, " ", forecast_year_i)
    results_list[[list_name]] <- lm(proportion_protected ~ model_year * ew,
                                    weights = area_sqkm,
                                    data = protected_areas %>%
                                      filter(ssp %in% c(NA, ssp_i)) %>%
                                      filter(model_year %in% c(2010.5, forecast_year_i)))
  }
}

results_summaries <- lapply(X = results_list, 
                            FUN = summary)

################################################################################
# Below is exploratory, but not likely appropriate ways of analyzing the data


########################################
# Using area protected as response, weighting by area
# This is too all-over the place with such high variation in area (could be 
# accommodated by adding species random intercept?)
results_list <- vector(mode = "list",
                       length = length(ssps) * length(forecast_years))
names(results_list) <- list_names

for (ssp_i in ssps) {
  for (forecast_year_i in forecast_years) {
    list_name <- paste0(ssp_i, " ", forecast_year_i)
    results_list[[list_name]] <- lm(area_protected_sqkm ~ model_year * ew,
                                    weights = area_sqkm,
                                    data = protected_areas %>%
                                      filter(ssp %in% c(NA, ssp_i)) %>%
                                      filter(model_year %in% c(2010.5, forecast_year_i)))
  }
}

results_summaries <- lapply(X = results_list, 
                            FUN = summary)

########################################
# Using proportion protected as response, species as random intercept
results_list <- vector(mode = "list",
                       length = length(ssps) * length(forecast_years))
names(results_list) <- list_names

for (ssp_i in ssps) {
  for (forecast_year_i in forecast_years) {
    list_name <- paste0(ssp_i, " ", forecast_year_i)
    results_list[[list_name]] <- lmer(proportion_protected ~ model_year * ew + (1|insect),
                                      data = protected_areas %>%
                                        filter(ssp %in% c(NA, ssp_i)) %>%
                                        filter(model_year %in% c(2010.5, forecast_year_i)))
  }
}

results_summaries <- lapply(X = results_list, 
                            FUN = summary)

########################################
# Using area protected as response, species as random intercept
results_list <- vector(mode = "list",
                       length = length(ssps) * length(forecast_years))
names(results_list) <- list_names

for (ssp_i in ssps) {
  for (forecast_year_i in forecast_years) {
    list_name <- paste0(ssp_i, " ", forecast_year_i)
    results_list[[list_name]] <- lmer(area_protected_sqkm ~ model_year * ew + (1|insect),
                                      data = protected_areas %>%
                                        filter(ssp %in% c(NA, ssp_i)) %>%
                                        filter(model_year %in% c(2010.5, forecast_year_i)))
  }
}

results_summaries <- lapply(X = results_list, 
                            FUN = summary)

########################################
# Using area protected as response, species as random intercept, weighting by 
# area (is this duplicitous with random effect?)
results_list <- vector(mode = "list",
                       length = length(ssps) * length(forecast_years))
names(results_list) <- list_names

for (ssp_i in ssps) {
  for (forecast_year_i in forecast_years) {
    list_name <- paste0(ssp_i, " ", forecast_year_i)
    results_list[[list_name]] <- lmer(area_protected_sqkm ~ model_year * ew + (1|insect),
                                      weights = area_sqkm,
                                      data = protected_areas %>%
                                        filter(ssp %in% c(NA, ssp_i)) %>%
                                        filter(model_year %in% c(2010.5, forecast_year_i)))
  }
}

results_summaries <- lapply(X = results_list, 
                            FUN = summary)