# Set up remote cloud storage for output

Summary: Goal is to have a place to store the various output files so all 
parties on the project do not need to run all models and predictions. We 
ultimately chose OneDrive for easy of synchronizing.

## Other options

+ Git LFS: Can make it onerous for future clones of the repository.
+ OSF + osfr: osfr and API functionality make navigating directory structure 
difficult
+ Google Drive: (not investigated)

## Setup

For Linux, working through the program rclone. Alternative is to install the 
[open source OneDrive client](https://abraunegg.github.io/).

### Installing & configuring rclone

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

### Troubleshooting

Tokens expire after 90 days if not used. To refresh tokens, run 
`rclone config reconnect onedrive:`

### References for rclone

1. [https://itsfoss.com/use-onedrive-linux-rclone/](https://itsfoss.com/use-onedrive-linux-rclone/)
2. [https://rclone.org/onedrive/](https://rclone.org/onedrive/)
