# Install third-party R libraries
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-01-19

# Read in list of dependencies
libs <- read.csv(file = "src/dependencies.csv", header = TRUE)
libs <- libs[, 1]
# To test just a pair of libraries
# libs <- c("dplyr", "kernlab")

# Install each library and try to load after installation; use 
# repos = "https://cran.microsoft.com/" to pre-select CRAN mirror

# For now, we only install packages that are NOT already installed
installed <- rownames(installed.packages())
for (one_lib in libs) {
  if (!(one_lib %in% installed)) {
    install.packages(one_lib, 
                     repos = "https://cran.microsoft.com/",
                     dependencies = TRUE) # So "Suggests" get installed, too
  } else {
    message(one_lib, " already installed.")
  }
}

# Now run through each of the dependencies attempting to load
for (one_lib in libs) {
  if (!require(one_lib, character.only = TRUE)) {
    warning("Could NOT load ", one_lib)
  } else { # Library loaded fine, unload
    message(one_lib, " loaded successfully")
  }
}
