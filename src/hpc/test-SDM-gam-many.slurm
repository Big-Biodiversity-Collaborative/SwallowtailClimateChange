#!/bin/bash
### Test for running gam SDMs on two insects and hosts
### Jeff Oliver
### jcoliver@arizona.edu
### 2023-01-19

#SBATCH --job-name=test-SDM-gam-many
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
Rscript src/run-indiv/run-all-SDM-gam-scripts.R