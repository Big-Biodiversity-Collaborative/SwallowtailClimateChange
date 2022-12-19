# Synchronize output stored locally with that on OSF project
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-12-14

require(osfr)
require(dplyr)

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
#   output_dir <- osfr::osf_mkdir(x = osf_node, path = "output")
#   osfr::osf_upload(x = output_dir, path = "output/gbif-name-check.csv")
#   areas_dir <- osfr::osf_mkdir(x = osf_node, path = "output/areas")
#   osfr::osf_upload(x = areas_dir, path = "output/areas/delta-contemporary-glm.csv")
# })

# Retrieve information about files on OSF
osf_files <- osfr::osf_ls_files(osf_node)

# Retrieve information about the output folder
osf_files_output <- osfr::osf_ls_files(output_dir)

# Can get file vs. folder information through the meta tag
osf_files_output$meta[[1]]$attributes$kind
# file
osf_files_output$meta[[2]]$attributes$kind
# folder
# Or, to get all the file/folder information
osf_attributes <- lapply(osf_files_output$meta, "[[", "attributes")
osf_kinds <- unlist(lapply(osf_attributes, "[[", "kind"))

# Ideally would be able to use hashes to see if files are different (but then 
# still need to make a call of which one to use...)
# For first file, the following might be useful:
osf_files$meta[[1]]$attributes$date_modified
# Can use tools::md5sum for comparable hashes (don't use openssl, they don't match)
osf_files$meta[[1]]$attributes$extra$hashes$md5
tools::md5sum("output/gbif-name-check.csv")
osf_files$meta[[1]]$attributes$extra$hashes$md5 == tools::md5sum("output/gbif-name-check.csv")
# openssl::md5(system.file("output/gbif-name-check.csv"))
# osf_files$meta[[1]]$attributes$extra$hashes$sha256
# openssl::sha256(system.file("output/gbif-name-check.csv"))

# Extract path + timestamp information for files on OSF

# Extract path + timestamp information for files on OSF

# Do uploading or downloading as necessary...

# Will want to create the local file structure on OSF. Directories that already 
# exist will be ignored. We have to create an object that corresponds to each
# of these directories in order to upload anything to it.
output_dirs <- list.dirs(path = "output")


# We now have a list of all folders in output. Currently only one level deep 
# (i.e no subfolders in any of the folder *within* output), but this might 
# change. We would like to see where local and OSF directories differ. Will 
# need to manually recurse through OSF elements in order to figure out 
# structure!?!

# Could create a data frame with folder paths, populated first by local 
# folder structure. We would then have a column for each of the two places a 
# folder can be: local and OSF. 
# output_folders <- data.frame(path = list.dirs(path = "output"),
#                              local = TRUE,
#                              osf = FALSE)

# For an initial migration of local output to osf, we can walk through 
# directories
# Even if output exists on destination, here's how we make a local object to 
# represent the OSF output folder
output_osf <- osfr::osf_mkdir(x = osf_node, 
                              path = "output")
# Want the folders in local output, but need some wrangling so the output 
# folder itself is *not* included?
# output_folders <- list.dirs(path = "output", full.names = FALSE)
# output_folders <- output_folders[nchar(output_folders) > 0]

#' Upload contents of directory to OSF
#' 
#' @param osf_parent the \code{osf_tbl_node} corresponding to the parent 
#' directory on OSF
#' @param path full relative path to local directory, e.g. "output/areas"
upload_dir <- function(osf_parent, path) {
  # Need full path information in this function, because while we are 
  # recursing, the list.files & list.dirs don't know that we are recursing 
  # (i.e. working directory is not changing)
  folder_name <- unlist(strsplit(x = path,
                                 split = "/")) %>% # Not excited about other OS
    tail(n = 1) # Get very last element of this vector
  
  # Start by finding/making directory on OSF (yes, via mkdir, weird)
  # osf_dir <- osfr::osf_mkdir(x = osf_parent,
  #                            path = folder_name)
  osf_dir <- paste0("Created OSF component for ", folder_name)

  # See if the path has any sub-directories, if so, upload those first
  local_sub_dirs <- list.dirs(path = path)[-1] # Self is always first
  if (length(local_sub_dirs) > 0) {
    for (sub_i in local_sub_dirs){
      upload_result <- upload_dir(osf_parent = osf_dir,
                                  path = sub_i)
    }
  }
  # Sub-directories are done, now we upload the files for *this* directory
  # TODO: sub-directories are listed in the output, so will need to deal with 
  # them explicitly
  local_files <- list.files(path = path, all.files = TRUE, full.names = TRUE)
  
}

# To kick off recursion, we pass the top-level project object (osf_node) as the 
# osf_parent and "output" as the path

output_folders <- list.dirs(path = "output")

for (dir_i in 1:output_folders) {
  # List the files in this folder; skipping any sub-folders
  # is this a subfolder? if so , need to recurse

  # dir_path <- output_folders$path[dir_i]

  
  
#   my_project %>%
#     osf_create_component("Research Materials") %>%
#     osf_upload(path = list.files(pattern = "\\.docx$"))

  # Does the directory have any sub-directories?
  sub_dirs <- list.dirs(dir_path)
  if (length(sub_dirs) > 1) { # self is always listed (first)
    for (sub_dir_i in 2:length(sub_dirs)) {
      # Recursion here, passing current folder as osf component
    }
  }

  # Now that we have already done whatever we need to do with sub-directories, 
  # we can upload the current folder's files to OSF
    
  # Use path as name for list element
  if (dir_path %in% names(osf_dirs)) {
    
  } 
}

# Do the recursive process of folders from OSF...
