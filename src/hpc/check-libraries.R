# Check to make sure all dependencies are installed
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-01-20

# Note does _not_ check for dependencies or suggests of packages themselves
installed <- rownames(installed.packages())

libs <- read.csv(file = "src/dependencies.csv", header = FALSE)
libs <- libs[, 1]

diffs <- setdiff(x = libs, y = installed)

if (length(diffs) > 0) {
  message("The following required libraries are not installed: ",
          paste0(diffs, collapse = ", "))
}