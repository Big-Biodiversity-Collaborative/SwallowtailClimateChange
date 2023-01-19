# Install third-party R libraries
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-01-19

# Primarily designed to be executed on an HPC

# Read in list of dependencies

# Remove any that are base-R packages from that list (no need to install)

# Install each library and try to load after installation; use 
# repos = "https://cran.microsoft.com/" to pre-select CRAN mirror

# For now, we only install packages that are NOT already installed
installed <- rownames(installed.packages())
libs <- c("dplyr", "kernlab")
for (one_lib in libs) {
  if (!(one_lib %in% installed)) {
    install.packages(one_lib)
  } else {
    message(one_lib, " already installed.")
  }
}

