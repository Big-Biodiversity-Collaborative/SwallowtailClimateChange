# Test of unzipping a file on multiple OSes
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-11-02

# Rationale: Want to make sure directory structure is preserved with archives
# we are tracking on GitHub

# Start by making an archive; no need to run these commented-out lines
# Write a little csv to the test folder
# write.csv(x = iris, file = "tests/iris-test.csv", row.names = FALSE)
# List the files in that folder that we want to zip
# files_to_zip <- list.files(path = "tests", pattern = "*.csv", full.names = TRUE)
# Make the zip archive
# zip(zipfile = "tests/test.zip", files = files_to_zip)

# Check our working directory. Should be top-level folder for this project, 
# i.e. path should end in SwallowtailClimateChange
getwd()
# Now we extract it
unzip(zipfile = "tests/test.zip")
# Make sure the extracted csv is in the tests directory (iris-test.csv)
list.files(path = "tests")
