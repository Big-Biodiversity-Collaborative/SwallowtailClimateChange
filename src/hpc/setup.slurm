#!/bin/bash
### Extract data files and make sure dependencies are installed
### Jeff Oliver
### jcoliver@arizona.edu
### 2023-01-19

#SBATCH --job-name=R-library-test
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
unzip data/gbif-pa.zip
unzip data/gbif-shapefiles.zip
Rscript src/hpc/check-libraries.R