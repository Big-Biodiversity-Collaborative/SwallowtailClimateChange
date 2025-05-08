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
module load R/4.3.2
# Need to install terra in a special way
# Start by setting up mamba
module load micromamba
micromamba shell init -s bash -r ~/micromamba
source ~/.bashrc
# Create environment
micromamba create -n r4.3.2 r=4.3.2
micromamba activate r4.3.2
micromamba install r-terra
# Now install the rest of the libraries
Rscript src/hpc/prep-install-libraries.R -u