#!/bin/bash
### Test for running maxent-tune distribution predictions on all insects and hosts
### Jeff Oliver
### jcoliver@arizona.edu
### 2023-01-19

#SBATCH --job-name=run-all-distribution-maxent-tune
#SBATCH --ntasks=8
#SBATCH --nodes=1
### Asking for 10 hours
#SBATCH --time=10:00:00
#SBATCH --partition=standard
#SBATCH --account=jcoliver

### GDAL and PROJ modules necessary for some packages (e.g. rgdal)
module load gdal
module load proj
module load geos
module load R/4.2.2
src/bash/build-scripts-SDM.sh
Rscript src/run-indiv/run-all-distribution-maxent-tune-scripts.R -a