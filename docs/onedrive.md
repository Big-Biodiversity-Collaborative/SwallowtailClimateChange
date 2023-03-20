# Set up remote cloud storage for output

Summary: Goal is to have a place to store the various output files so all 
parties on the project do not need to run all models and predictions. We 
ultimately chose OneDrive for easy of synchronizing. Instructions below are 
currently only written for Linux.

## Other options

+ Git LFS: Can make it onerous for future clones of the repository.
+ OSF + osfr: osfr and API functionality make navigating directory structure 
difficult
+ Google Drive: (not investigated)

## rclone Setup

For Linux, working through the program rclone. Alternative is to install the 
[open source OneDrive client](https://abraunegg.github.io/).

### Installing and configuring rclone

`sudo apt-get install rclone`
`rclone config`

+ Select 'n' for new remote
+ Name it 'onedrive'
+ In list that appears, identify number corresponding to Microsoft OneDrive and
select it (it was 22).
+ Leave client ID and client secret blank ('Enter' twice)
+ Select 'n' to skip advanced configuration
+ Select 'y' to use autoconfig
+ Should open a browser window to login. Use UA NetID authentication credentials
to log in to OneDrive
+ When asked for Persmissions, click 'Accept'
+ For account type, select number corresponding to OneDrive Personal or Business
+ Select '0' for the drive to use
+ Select 'y' to confirm business onedrive location (a URL)
+ Select 'y' once more to confirm
+ Select 'q' to quit rclone configuration

### Initial local to remote

Generic command: `rclone copy /home/source remote:backup`
Implementation (from within the SwallowtailClimateChange folder)
i.e. `pwd` gives `$~/Documents/Work/SwallowtailClimateChange`:

`rclone -v copy output onedrive:SwallowtailClimateChange/output`

(the `-v` flag makes it verbose; otherwise rclone is silent)

### Getting latest from remote

Want to download any files to local system that are on the remote (OneDrive) 
and update any local files that have newer version on remote. For now, we 
ignore any local files that are not on remote (this may eventually change).

Like initial migration (above), be sure to run this from within the 
SwallowtailClimateChange directory.

To test (no files changed, report of what would happen is printed):

`rclone copy -v --dry-run onedrive:SwallowtailClimateChange/output output`

To actually run:

`rclone copy -v onedrive:SwallowtailClimateChange/output output`

### Sending from local to remote

Works like getting latest to remote, but reversing source and destination. Once 
again, be sure this is run from SwallowtailClimateChange directory.

To test (no files changed, report of what would happen is printed):

`rclone copy -v --dry-run output onedrive:SwallowtailClimateChange/output`

To actually run:

`rclone copy -v output onedrive:SwallowtailClimateChange/output`

### Sending from HPC to remote

#### Setup

Set up rclone on the HPC, likely as above (see #Installing-and-configuring-rclone 
and [https://public.confluence.arizona.edu/display/UAHPC/Transferring+Data#TransferringData-rclone](https://public.confluence.arizona.edu/display/UAHPC/Transferring+Data#TransferringData-rclone)). 
Start the configuration process by starting an interactive session, then 
running the configuration process:

`interactive -a <username>`
`rclone config`

A couple of deviations from local setup:

+ When choosing the type of storage, Microsoft OneDrive is number 28.
+ There is a prompt to choose a national cloud region for OneDrive, select 1 
(Microsoft Cloud Global)
+ Select 'n' when prompted to use autoconfig, this will require a call to 
rclone on a _local_ machine with a web browser that can be used for 
authentication. This will return a very long key that should be pasted into the 
HPC shell session.
+ It will ask if the Drive is OK (it'll be a URL to the OneDrive that was set 
up above)
+ The long key will print again and ask if the remote is OK (enter 'y')

#### Transfer

To test transfer of distribution files from HPC to remote OneDrive (do this 
from _within_ the SwallowtailClimateChange folder on the HPC):

```
interactive -a <username>
rclone copy -v --dry-run output/distributions onedrive:SwallowtailClimateChange/output/distributions
```

To actually run:

`rclone copy -v output/distributions onedrive:SwallowtailClimateChange/output/distributions`

Some files may fail to transfer (idiosyncratic error?); if this happens, try 
calling `rclone copy` again.

**Note** The initial transfer did not complete (ended around the "T"s 
somewhere) because the interactive session timed out. Restarting the 
interactive sessions and running `rclone copy` finished the job _and_ 
transferred (most of) the files that had errored out before.

##### Transfer specific files

rclone does not support wildcard expansion, but we can transfer multiple files
using an `include` flag. To test:

`rclone copy -v --dry-run output/SDMs/ --include "papilio_appalachiensis*" onedrive:SwallowtailClimateChange/output/SDMs/`

and to actually copy:

`rclone copy -v output/SDMs/ --include "papilio_appalachiensis*" onedrive:SwallowtailClimateChange/output/SDMs/`

### Troubleshooting

Tokens expire after 90 days if not used. To refresh tokens, run 
`rclone config reconnect onedrive:`

### References for rclone

1. [https://itsfoss.com/use-onedrive-linux-rclone/](https://itsfoss.com/use-onedrive-linux-rclone/)
2. [https://rclone.org/onedrive/](https://rclone.org/onedrive/)

## OneDrive Windows client setup

Some gymnastics were required, but ultimately, we 

1. Shared the SwallowtailClimateChange on OneDrive to the other collaborator's 
institutional Microsoft account (@arizona.edu).
2. Set up the institutional OneDrive account on a Windows laptop; this was running parallel to another OneDrive account on the same laptop.
3. Synced the laptop to the (cloud) OneDrive, which pulled down the 
SwallowtailClimateChange/output folder to the laptop.
4. **Deleted** the local OneDrive output folder, but left the
SwallowtailClimateChange folder in place.
5. Made a symbolic link from the "real" local output folder (the output folder 
that lives with all the other assets under Git version control) to a "new" 
output folder in the OneDrive SwallowtailClimateChange folder:
```
mklink \d "C:\Users\username\OneDrive - University of Arizona\SwallowtailClimateChange\output" \
"C:\Users\username\Documents\SwallowtailClimateChange\output"
```
  + If there are errors about a file existing, make sure the output folder on 
  OneDrive (the place where the link is created, i.e. the first path), make 
  sure there is no "output" folder already there.
  + If there are errors about permissions, make sure the command terminal is 
  being run as Administrator
6. If it does not happen automatically, sync the OneDrive files. Check by 
logging into web interface for OneDrive to see that files are updated (note 
containing folder date/owner information may not change).

### References for OneDrive

1. [https://answers.microsoft.com/en-us/windows/forum/all/onedrive-on-windows-10-and-symbolichard-links/9003d014-bb3f-4069-a3e9-fe387aeb748d](https://answers.microsoft.com/en-us/windows/forum/all/onedrive-on-windows-10-and-symbolichard-links/9003d014-bb3f-4069-a3e9-fe387aeb748d)
2. [https://support.microsoft.com/en-us/office/can-t-synchronize-onedrive-files-and-folders-from-a-local-file-location-other-than-the-default-onedrive-path-b7eef9d4-4203-431d-8345-fe49254f9da0](https://support.microsoft.com/en-us/office/can-t-synchronize-onedrive-files-and-folders-from-a-local-file-location-other-than-the-default-onedrive-path-b7eef9d4-4203-431d-8345-fe49254f9da0)