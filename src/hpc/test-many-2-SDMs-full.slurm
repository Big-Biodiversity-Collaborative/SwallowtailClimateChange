#!/bin/bash
### Run SDMs on full datasets for subset of insects and hosts
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

### GDAL will load PROJ and GEOS modules, too
module load gdal/3.3.2
module load R/4.3
src/bash/build-scripts-2-SDMs-full.sh
Rscript src/run-indiv/run-all-2-SDMs-full-scripts.R