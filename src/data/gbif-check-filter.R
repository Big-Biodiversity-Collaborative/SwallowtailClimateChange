# Assess impact of excluding species with <40 observations post-filtering
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-01-10

library(dplyr)

# Following filtering, some species may have fewer than 40 observations, want 
# to report:
#     1. How many insect species would this exclude?
#     2. How many plant species would this exclude? (less important)
#     3. For each insect species, how many / what proportion of their hosts 
#        would be excluded?

# File to write output
filter_summary_file <- "data/gbif-filter-summary.csv"

min_records <- 40
insect_host <- read.csv(file = "data/insect-host.csv")

# Read in summary file, which comes from src/data/gbif-3-presence-absence.R
gbif_pa <- read.csv(file = "data/gbif-pa-summary.csv")

insects <- unique(insect_host$insect)

# Data frame to hold summary results
filter_summary <- data.frame(insect = insects,
                             num_filtered = NA_integer_,
                             included = NA,
                             total_hosts = NA_integer_,
                             included_hosts = NA_integer_)
for (i in 1:nrow(filter_summary)) {
  # Pull out name of insect species
  insect <- filter_summary$insect[i]
  # Find out if the insect species has <40 records
  filter_summary$num_filtered[i] <- gbif_pa$n_filtered[gbif_pa$species == insect]
  filter_summary$included[i] <- filter_summary$num_filtered[i] >= min_records
  # Now pull out all hosts of this insect species from the summary table
  hosts <- gbif_pa %>%
    filter(species %in% insect_host$host_accepted[insect_host$insect == insect])
  filter_summary$total_hosts[i] <- nrow(hosts)
  filter_summary$included_hosts[i] <- sum(hosts$n_filtered >= min_records)
}
# Finally, do proportion calculation
filter_summary <- filter_summary %>%
  mutate(host_prop = included_hosts / total_hosts)

# Write results to file
write.csv(x = filter_summary,
          file = filter_summary_file,
          row.names = FALSE)