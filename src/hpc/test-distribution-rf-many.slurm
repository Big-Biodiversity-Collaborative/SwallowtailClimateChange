#!/bin/bash
### Test for running rf predictions on two insects and hosts
### Jeff Oliver
### jcoliver@arizona.edu
### 2023-02-03

#SBATCH --job-name=test-distribution-rf-many
#SBATCH --ntasks=16
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
src/bash/build-scripts-distribution.sh
Rscript src/run-indiv/run-all-distribution-rf-scripts.R