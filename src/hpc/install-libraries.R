# Install third-party R libraries
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-01-19

# Logical indicating to skip libraries that are already installed (i.e. won't 
# update libraries)
skip_installed <- TRUE

# If this script is called from bash (e.g. Rscript run-all-...), parse
# arguments and update variables accordingly. e.g. 
# $ Rscript install-libraries.R -u
#    -u: sets skip_installed to FALSE
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  skip_installed <- !("-u" %in% args)
}

# Read in list of dependencies
libs <- read.csv(file = "src/dependencies.csv", header = TRUE)
libs <- libs[, 1]
# Add remotes because we need to install at least one from GitHub; adding at 
# the very beginning of vector so it is installed/checked first
libs <- c("remotes", libs)
# To test just a pair of libraries
# libs <- c("dplyr", "kernlab")

# Install each library and try to load after installation; use 
# repos = "https://cran.microsoft.com/" to pre-select CRAN mirror

installed <- rownames(installed.packages())
if (skip_installed) {
  installed <- ""
}
for (one_lib in libs) {
  if (!(one_lib %in% installed)) {
    deps <- NA
    if (one_lib == "ENMeval") {
      deps <- TRUE # So "Suggests" get installed, too
    }
    if (one_lib == "flexsdm") { # Not on CRAN
      remotes::install_github(repo = "sjevelazco/flexsdm@HEAD")
    } else {
      install.packages(one_lib, 
                       repos = "https://cran.microsoft.com/",
                       dependencies = deps)
    }
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
