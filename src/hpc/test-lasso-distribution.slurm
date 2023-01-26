#!/bin/bash
### Test for running lasso predictions on two insects and hosts
### Jeff Oliver
### jcoliver@arizona.edu
### 2023-01-19

#SBATCH --job-name=R-lasso-prediction-test
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --cpus-per-task=4
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
Rscript src/run-indiv/run-all-distribution-lasso-scripts.R

### To avoid OOM errors, may need to do array jobs
### Find all the lasso distribution scripts
### allfiles=$(ls src/indiv/*distribution-lasso.R)
### Change to an array
### lassos=($allfiles)
### Reality check
### for i in ${lassos[@]}; do echo $i; done
### See 
### https://help.rc.ufl.edu/doc/SLURM_Job_Arrays#Using_the_array_ID_Index
### https://blog.ronin.cloud/slurm-job-arrays/