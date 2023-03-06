#!/bin/bash
### Test for running gam distribution predictions on all insects and hosts
### Jeff Oliver
### jcoliver@arizona.edu
### 2023-01-19

#SBATCH --job-name=dist-gam-all
#SBATCH --ntasks=8
#SBATCH --nodes=1
### Asking for 20 hours
#SBATCH --time=20:00:00
#SBATCH --partition=standard
#SBATCH --account=jcoliver

### GDAL and PROJ modules necessary for some packages (e.g. rgdal)
module load gdal
module load proj
module load geos
module load R/4.2.2
src/bash/build-scripts-SDM.sh
Rscript src/run-indiv/run-all-distribution-gam-scripts.R -a