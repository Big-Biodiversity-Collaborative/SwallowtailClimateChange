#!/bin/bash
### Running R script to install terra library
### Jeff Oliver
### jcoliver@arizona.edu
### 2023-01-19

#SBATCH --job-name=R-terra-install
#SBATCH --ntasks=1
#SBATCH --nodes=1
### Asking for 2 hours
#SBATCH --time=02:00:00
#SBATCH --partition=standard
#SBATCH --account=jcoliver

### GDAL, PROJ, and GEOS modules necessary for some packages; PROJ and GEOS 
# get loaded by GDAL, so no need to load them explicitly. Using specific 
# version of GDAL module because the terra install may require a specific
# (older) version of PROJ (the version with 3.3.2 GDAL module)
module load gdal/3.3.2
# module load proj
# module load geos
module load R/4.3
Rscript src/hpc/prep-install-terra.R