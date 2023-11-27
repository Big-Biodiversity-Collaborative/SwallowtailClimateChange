#!/bin/bash
### Run CV evaluation on one species (P. rumiko)
### Jeff Oliver
### jcoliver@arizona.edu
### 2023-01-19

#SBATCH --job-name=xfer-onedrive
#SBATCH --ntasks=1
#SBATCH --nodes=1
### Asking for 10 hours
#SBATCH --time=10:00:00
#SBATCH --partition=standard
#SBATCH --account=jcoliver

### May need to refresh tokens via
### rclone config reconnect onedrive:

### Before running, start an interactive session and do a dry run:
### interactive -a <username>
### cd ~/SwallowtailClimateChange
### rclone copy -v --dry-run output/distributions onedrive:SwallowtailClimateChange/output/distributions

cd ~/SwallowtailClimateChange
rclone copy -v output/distributions onedrive:SwallowtailClimateChange/output/distributions
rclone copy -v output/suitabilities onedrive:SwallowtailClimateChange/output/suitabilities
cd ~