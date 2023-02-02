#!/bin/bash
### Test for running predictions on one insect (P. rumiko) and all SDM models
### Jeff Oliver
### jcoliver@arizona.edu
### 2023-01-19

#SBATCH --job-name=test-distribution-lasso-many
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
Rscript src/indiv/papilio_rumiko-distribution-brt.R
Rscript src/indiv/papilio_rumiko-distribution-gam.R
Rscript src/indiv/papilio_rumiko-distribution-lasso.R
Rscript src/indiv/papilio_rumiko-distribution-maxent-tune.R
Rscript src/indiv/papilio_rumiko-distribution-rf.R