# Plot GLM coefficient estimate loadings in relation to insect arid or not
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-14

# DEPRECATED

# Look at glm model results to see if the different predictor bioclim variables 
# are behaving differently between arid vs. non-arid insect species.

sdm_method <- "glm"

require(dplyr)
require(tidyr)
require(ggplot2)

# Read in insect data
insects_hosts <- read.csv(file = "data/insect-host.csv")

# Identify unique species of insects
insect_species <- unique(insects_hosts$insect)

# Grab the information on arid/non-arid
arid <- read.csv(file = "data/arid-estimates.csv")

# Data frame to hold coefficient estimates
coeff_estimates <- data.frame()

# Pull out the coefficient estimates for each species
for (i in 1:length(insect_species)) {
  species_name <- insect_species[i]
  nice_name <- tolower(x = gsub(pattern = " ",
                                replacement = "_",
                                x = species_name))
  model_file <- paste0("output/SDMs/",
                       nice_name, "-",
                       sdm_method,
                       ".rds")

  if (file.exists(model_file)) {
    # Read in the output of run_glm et al
    sdm_model <- readRDS(model_file)
    sdm_summary <- summary(sdm_model$model)
    coeff_est <- sdm_summary$coefficients
    
    # Want all the coefficient estimates info
    estimates <- data.frame(insect = species_name, 
                            coefficient = rownames(coeff_est),
                            estimate = coeff_est[, "Estimate"],
                            se = coeff_est[, "Std. Error"],
                            z_value = coeff_est[, "z value"],
                            p_value = coeff_est[, "Pr(>|z|)"])
  } else { # no corresponding model on disk
    estimates <- data.frame(insect = species_name, 
                            coefficient = NA,
                            estimate = NA,
                            se = NA,
                            z_value = NA,
                            p_value = NA)
  }
  rownames(estimates) <- NULL
  
  # Add those estimates to the data frame
  if (i == 1) {
    coeff_estimates <- estimates
  } else {
    # Already have columns add as appropriate
    coeff_estimates <- coeff_estimates %>%
      bind_rows(estimates)
  }
}

# Add in data on whether the beast is arid or not; dropping intercept
coeff_data <- coeff_estimates %>%
  filter(!is.na(coefficient)) %>% # Dropping any species that didn't have model
  filter(coefficient != "(Intercept)") %>%
  inner_join(arid)

# Find out which variables were significant across species, need to count 
# number of arid/non-arid in resulting coeff_dataset
arid_non <- coeff_data %>%
  select(insect, arid) %>%
  distinct()
num_arid <- sum(arid_non$arid)
num_non_arid <- sum(arid_non$arid)

sig_coeffs <- coeff_data %>%
  filter(p_value < 0.05) %>%
  group_by(coefficient, arid) %>%
  summarize(total = n()) %>%
  summarize(prop_sig = if_else(arid,
                               true = total/num_arid,
                               false = total/num_non_arid),
            arid = arid) %>%
  arrange(coefficient) %>%
  ungroup()

# Plot each coefficient to see if difference...(needs to ultimately be 
# corrected for different # of species in arid/non-arid)
ggplot(data = sig_coeffs %>% filter(coefficient != "(Intercept)"), 
       mapping = aes(x = coefficient, y = prop_sig, group = arid, fill = arid)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw()

################################################################################
# Run Wilcox rank test on all predictors (omitting P. aristodemus), comparing 
# coefficient estimate ranks between arid and non-arid species
coeff_data_good <- coeff_data %>%
  filter(insect != "Papilio aristodemus") %>%
  # filter(insect != "Papilio brevicauda") %>%
  # filter(insect != "Papilio appalachiensis") %>%
  mutate(arid_text = if_else(arid,
                             true = "Arid",
                             false = "Non-arid"))

# Just plot those estimates and color by arid / non-arid
ggplot(data = coeff_data_good,
       mapping = aes(x = coefficient, y = estimate, color = arid_text)) +
  geom_point() +
  scale_color_manual(values = c("#66c2a5", "#fc8d62")) +
  theme_bw()

# Same as above, with facets
ggplot(data = coeff_data_good,
       mapping = aes(x = arid_text, y = estimate, color = arid_text)) +
  geom_point() +
  scale_color_manual(values = c("#66c2a5", "#fc8d62")) +
  facet_wrap(~ coefficient, scales = "free_y") +
  theme_bw()

wilcox_results <- NULL
coeff_names <- unique(coeff_data_good$coefficient) 
for (i in 1:length(coeff_names)) {
  # Extract estimates for one of the bioclim predictors
  one_coeff <- coeff_data_good %>%
    filter(coefficient == coeff_names[i])
  
  # Run Wilcox rank test
  one_wilcox <- wilcox.test(x = one_coeff$estimate[one_coeff$arid],
                            y = one_coeff$estimate[!one_coeff$arid],
                            paired = FALSE)
  
  # Extract W and p values
  one_result <- data.frame(coefficient = coeff_names[i],
                           p_value = one_wilcox$p.value,
                           W = one_wilcox$statistic)
  rownames(one_result) <- NULL

  # Add to data frame
  if (is.null(wilcox_results)) {
    wilcox_results <- one_result
  } else {
    wilcox_results <- wilcox_results %>%
      dplyr::bind_rows(one_result)
  }
}
wilcox_results
# bio15 p = 0.03596 estimate is positive (or zero) for arid, negative for non-arid
# bio18 p = 0.00759 estimate is negative for arid, positive for non-arid

# When only P. aristodemus is excluded, bio15 and bio18 are different between 
# arid and non-arid
# 15: Precipitation Seasonality (Coefficient of Variation)
# 18: Precipitation of Warmest Quarter  (i.e. summer rain)
# When P. appalaciensis, P. aristodemus, and P. brevicaudata are all excluded 
# (results not shown), only bio18 remains significant
ggplot(data = coeff_data_good %>% filter(coefficient %in% c("bio15", "bio18")),
       mapping = aes(x = arid_text, y = estimate)) +
  geom_boxplot() +
  geom_jitter() +
  facet_wrap(~ coefficient, scales = "free_y") +
  theme_bw()
