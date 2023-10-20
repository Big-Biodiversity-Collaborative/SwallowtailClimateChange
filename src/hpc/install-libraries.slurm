#!/bin/bash
### Running R script to install libraries
### Jeff Oliver
### jcoliver@arizona.edu
### 2023-01-19

#SBATCH --job-name=R-library-install
#SBATCH --ntasks=1
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
Rscript src/hpc/install-libraries.R -u