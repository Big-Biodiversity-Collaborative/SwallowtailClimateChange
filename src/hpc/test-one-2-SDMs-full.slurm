#!/bin/bash
### Run SDMs on full datasets on one species (P. rumiko)
### Jeff Oliver
### jcoliver@arizona.edu
### 2023-01-19

#SBATCH --job-name=2-SDMs-full-all
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
module load R/4.3.2
src/bash/build-scripts-2-SDMs-full.sh
Rscript src/indiv/papilio_rumiko-2-SDMs-full.R