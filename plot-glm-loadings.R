# Plot GLM coefficient estimate loadings
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-06-14

# require(raster)
require(dplyr)
require(tidyr)
require(ggplot2)

# Load up the functions from the functions folder
function_files <- list.files(path = "./functions", 
                             pattern = ".R$", 
                             full.names = TRUE)
for(fun_file in function_files) {
  source(file = fun_file)
}

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
  model_file <- paste0("output/models/",
                         nice_name, 
                         "-model-glm-current.rds")
  
  # Read in the output of run_glm et al
  glm_model <- readRDS(model_file)
  glm_summary <- summary(glm_model$model)
  coeff_est <- glm_summary$coefficients

  # Want the coefficient estimates from the model and the p-values
  estimates <- data.frame(insect = species_name, 
                          coefficient = rownames(coeff_est),
                          estimate = coeff_est[, "Estimate"],
                          p_value = coeff_est[, "Pr(>|z|)"])
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

# Add in data on whether the beast is arid or not
coeff_estimates <- coeff_estimates %>%
  inner_join(arid)

# Find out which variables were significant across species
num_arid <- sum(arid$arid)
num_non_arid <- sum(!arid$arid)
sig_coeffs <- coeff_estimates %>%
  filter(coefficient != "(Intercept)") %>%
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

# Three species has large deviations from every other species - omit 
# P. aristodemus, P. appalachiensis, P. brevicauda
coeff_long <- coeff_estimates %>%
  pivot_longer(-insect, names_to = "coefficient", values_to = "estimate") %>%
  filter(!(insect %in% c("Papilio aristodemus", 
                         "Papilio appalachiensis", 
                         "Papilio brevicauda")))


# Plot, ignoring whether or not things are arid or not
agnostic_plot <- ggplot(data = coeff_long %>% filter(coefficient != "(Intercept)"), 
                        mapping = aes(x = coefficient, y = estimate, color = insect)) +
  # geom_boxplot() +
  geom_jitter() +
  facet_wrap(~ coefficient, scales = "free") +
  theme_bw()
agnostic_plot

# p = 1/(1 + e^(-B x))
# What does a unit increase in x do to probability?
coeff_long <- coeff_long %>%
  mutate(p = 1 / (1 + exp(x = (-1 * estimate))))
agnostic_plot <- ggplot(data = coeff_long %>% filter(coefficient != "(Intercept)"), 
                        mapping = aes(x = coefficient, y = p, color = insect)) +
  # geom_boxplot() +
  geom_jitter() +
  theme_bw()
agnostic_plot

