#!/bin/bash
### Run CV evaluation on all insects and hosts
### Jeff Oliver
### jcoliver@arizona.edu
### 2023-01-19

#SBATCH --job-name=1-CV-all
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
Rscript src/hpc/prep-extract-data.R
### Old approach required building of individual species' scripts from template
### src/bash/build-scripts-1-CV.sh
### Rscript src/run-indiv/run-all-1-CV-scripts.R -a
### New approach goes right to wrapper
Rscript src/run-indiv/run-all-1-CV.R -a -f