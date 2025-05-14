#!/bin/bash
### Run CV evaluation on all insects and hosts
### Jeff Oliver
### jcoliver@arizona.edu
### 2023-01-19

#SBATCH --job-name=1-CV-all
#SBATCH --ntasks=8
#SBATCH --nodes=1
### Asking for 60 hours
#SBATCH --time=60:00:00
#SBATCH --partition=standard
#SBATCH --account=jcoliver

### GDAL will load PROJ and GEOS modules, too
module load gdal/3.3.2
module load R/4.3
Rscript src/hpc/prep-extract-data.R
Rscript src/run-analyses/run-all-1-CV.R -a -f