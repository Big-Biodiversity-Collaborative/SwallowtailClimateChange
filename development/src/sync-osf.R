# Synchronize output stored locally with that on OSF project
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-12-14

require(osfr)

# Goal of this script is to allow storage of output (species distribution 
# model objects, predicted distributions, maps, etc.) on the Open Science 
# Framework. The osfr package allows us to send and receive data of an OSF 
# project via R. It *does* require authentication of the user (especially for 
# write access).

# Each user will need an OSF personal access token (PAT). To get a PAT, log 
# into OSF and got to the settings to add a token 
# (https://osf.io/settings/tokens/). Be sure to check osf.full_read and 
# osf.full_write in the Create token dialog.

# Copy the token and save it in a plain text file OUTSIDE of this project 
# Somewhere like /home/<USERNAME>/.osf-pat.txt, replacing "<USERNAME>" with 
# your user name on your computer.

# Now create _another_ text file in this project's top-level directory; call 
# this file osf-pat-path.txt. The file should be ignored by git (it is listed 
# in the .gitignore file). If it shows up in the Git dialog, make sure the file
# is spelled correctly. In this text file, add the path to the file that 
# actually contains the PAT. This should be something like 
# /home/<USERNAME>/.osf-pat.txt or C:/Users/Documents/.osf-pat.txt.

# Read in the location of the PAT. Note this file does NOT have the PAT. The 
# file, osf-pat-path.txt, should include a single line of text with the path to 
# file that includes the actual PAT. The file that includes the PAT should be 
# stored OUTSIDE of this project.
pat_path <- scan(file = "osf-pat-path.txt", what = character())

# Make sure the PAT file exists before proceding
if (!file.exists(pat_path)) {
  stop("Could not find file with PAT path information; check: ", pat_path)
}

# Read the PAT from the file and use it to authenticate session
osfr::osf_auth(scan(file = pat_path, what = character()))

# Connect to this project on OSF
osf_node <- osfr::osf_retrieve_node(id = "https://osf.io/94knd/")

# For testing purposes only, add two files from the output folder
# NOTE: path information is NOT preserved via upload. That is, both files are 
# uploaded to top-level directory, not output folder. See instructions under 
# "Uploading to subdirectories" section of osf_upload documentation.
# invisible({
#   osfr::osf_upload(x = osf_node, path = "output/gbif-name-check.csv")
#   osfr::osf_upload(x = osf_node, path = "output/areas/delta-contemporary-glm.csv")
# })

# Retrieve information about files on OSF
osf_files <- osfr::osf_ls_files(osf_node)

# Ideally would be able to use hashes to see if files are different (but then 
# still need to make a call of which one to use...)
# For first file, the following might be useful:
osf_files$meta[[1]]$attributes$date_modified
# These don't seem to match up with locally-computed hashes...
osf_files$meta[[1]]$attributes$extra$hashes$md5
openssl::md5(system.file("output/gbif-name-check.csv"))
osf_files$meta[[1]]$attributes$extra$hashes$sha256
openssl::sha256(system.file("output/gbif-name-check.csv"))

# Extract path + timestamp information for files on OSF

# Extract path + timestamp information for files on OSF

# Do uploading or downloading as necessary...