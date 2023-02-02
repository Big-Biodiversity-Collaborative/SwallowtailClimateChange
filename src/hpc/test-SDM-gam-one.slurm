#!/bin/bash
### Test for running one gam SDM (P. rumiko)
### Jeff Oliver
### jcoliver@arizona.edu
### 2023-01-20

#SBATCH --job-name=test-SDM-gam-one
#SBATCH --ntasks=1
#SBATCH --nodes=1
### Asking for 10 minutes
#SBATCH --time=00:10:00
#SBATCH --partition=standard
#SBATCH --account=jcoliver

### GDAL and PROJ modules necessary for some packages (e.g. rgdal)
module load gdal
module load proj
module load geos
module load R/4.2.2
src/bash/build-scripts-SDM.sh
Rscript src/indiv/papilio_rumiko-SDM-gam.R