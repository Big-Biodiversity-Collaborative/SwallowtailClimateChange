#!/bin/bash
### Test for running maxent-tune SDMs on two insects and hosts
### Jeff Oliver
### jcoliver@arizona.edu
### 2023-01-19

#SBATCH --job-name=SDM-maxent-tune-many
#SBATCH --ntasks=8
#SBATCH --nodes=1
### Asking for 2 hours
#SBATCH --time=02:00:00
#SBATCH --partition=standard
#SBATCH --account=jcoliver

### GDAL and PROJ modules necessary for some packages (e.g. rgdal)
module load gdal
module load proj
module load geos
module load R/4.2.2
src/bash/build-scripts-SDM.sh
Rscript src/run-indiv/run-all-SDM-maxent-tune-scripts.R